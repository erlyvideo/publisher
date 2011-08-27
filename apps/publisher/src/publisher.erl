-module(publisher).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-export([publish/2, listen/2, run/0]).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([send_frame/3]).

-record(publisher, {
  url,
  rtmp,
  socket,
  stream,
  encoder
}).

run() ->
  os:putenv("LD_LIBRARY_PATH", "deps/h264/priv"),
  code:add_pathz("deps/uvc/ebin"),
  code:add_pathz("deps/alsa/ebin"),
  code:add_pathz("deps/jpeg/ebin"),
  code:add_pathz("deps/h264/ebin"),
  [code:add_pathz(P) || P <- filelib:wildcard("../erlyvideo/apps/*/ebin")],
  [code:add_pathz(P) || P <- filelib:wildcard("/opt/erlyvideo/lib/*/ebin")],
  application:start(rtmp),
  application:start(publisher),
  io:format("Starting~n"),
  {ok, Config, _Path} = file:path_consult(["."], "publisher.conf"),
  case proplists:get_value(publish, Config) of
    undefined -> io:format("Publish disabled~n");
    RTMP -> publisher:publish(RTMP, Config)
  end,
  case proplists:get_value(listen, Config) of
    undefined -> io:format("Listen disabled~n");
    Listen -> publisher:listen(Listen, Config)
  end,
  {ok, erlang:whereis(publisher_sup)}.

publish(URL, Options) ->
  publisher_sup:start_publisher(URL, Options).


listen(Listen, Options) ->
  publisher_sup:start_listener(Listen, Options).

start_link(URL, Options) ->
  gen_server:start_link(?MODULE, [URL, Options], []).



init([URL, Options]) ->
  
  {ok, RTMP} = rtmp_socket:connect(URL),
  Stream = receive
    {rtmp, RTMP, connected} ->
      rtmp_socket:setopts(RTMP, [{active, true}]),
      rtmp_lib:connect(RTMP, [{app, <<"live">>}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
      Stream1 = rtmp_lib:createStream(RTMP),
      {rtmp, _UserInfo, _Host, _Port, [$/ | Path], _Query} = http_uri2:parse(URL),
      rtmp_lib:publish(RTMP, Stream1, Path),
      rtmp_socket:setopts(RTMP, [{chunk_size, 16#200000}]),
      Stream1
  after
    1000 ->
      erlang:exit(rtmp_timeout)
  end,
  {rtmp, Socket} = rtmp_socket:get_socket(RTMP),
  inet:setopts(Socket, [{sndbuf,1024*1024}]),
  {ok, Encoder} = publish_encoder:start_link(self(), Options),
  {ok, #publisher{
    url = URL,
    socket = Socket,
    rtmp = RTMP,
    stream = Stream,
    encoder = Encoder
  }}.


channel_id(#video_frame{content = metadata}) -> 4;
channel_id(#video_frame{content = audio}) -> 5;
channel_id(#video_frame{content = video}) -> 6.


rtmp_message(#video_frame{dts = DTS, content = Type} = Frame, StreamId) ->
  #rtmp_message{
    channel_id = channel_id(Frame), 
    timestamp = DTS,
    type = Type,
    stream_id = StreamId,
    body = flv_video_frame:encode(Frame)}.

send_frame(Socket, Stream, #video_frame{} = Frame) when is_port(Socket) ->
  FlvFrameGen = flv:rtmp_tag_generator(Frame),
  gen_tcp:send(Socket, FlvFrameGen(0, Stream)),
  ok;

send_frame(RTMP, Stream, #video_frame{} = Frame) when is_pid(RTMP) ->
  Message = rtmp_message(Frame, Stream),
  rtmp_socket:send(RTMP, Message).
  

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.
  


handle_info({rtmp, _RTMP, #rtmp_message{type = ack_read}}, State) ->
  {noreply, State};

handle_info({rtmp, _, _} = Msg, State) ->
  io:format("rtmp: ~p~n", [Msg]),
  {noreply, State};

handle_info(#video_frame{} = Frame, #publisher{socket = Socket, stream = Stream} = State) ->
  send_frame(Socket, Stream, Frame),
  {noreply, State};

% handle_info(status, #publisher{buffer = Buf} = State) ->
%   io:format("buffer: ~p, ~p~n", [length(Buf), [{C,D,size(B)} || #video_frame{codec = C, dts = D, body = B} <- Buf]]),
%   {noreply, State};

handle_info(Else, State) ->
  {stop, {undefined_message,Else}, State}.

terminate(_Reason, _State) -> ok.

