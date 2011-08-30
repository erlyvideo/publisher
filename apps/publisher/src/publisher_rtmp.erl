-module(publisher_rtmp).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").


-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(publisher, {
  rtmp,
  url,
  stream,
  options,
  last_dts,
  encoder
}).



start_link(Type, RTMP, Options) ->
  gen_server:start_link(?MODULE, [Type, RTMP, Options], []).




init([Type, RTMP, Options]) ->
  Publisher = case Type of
    passive -> init_passive(RTMP, Options);
    active  -> init_active(RTMP, Options)
  end,
  timer:send_interval(10000, dump_status),
  {ok, Publisher}.


init_passive(RTMP, Options) ->
  #publisher{
    rtmp = RTMP,
    options = Options
  }.


init_active(URL, Options) ->

  {ok, RTMP} = rtmp_socket:connect(URL),
  Stream = receive
    {rtmp, RTMP, connected} ->
      {rtmp, _UserInfo, _Host, _Port, [$/ | FullPath], _Query} = http_uri2:parse(URL),
      {match, [App | Path]} = re:run(FullPath, "([^\\/]+)/(.*)", [{capture,all_but_first,list}]),
      
      rtmp_socket:setopts(RTMP, [{active, true}]),
      rtmp_lib:connect(RTMP, [{app, list_to_binary(App)}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
      Stream1 = rtmp_lib:createStream(RTMP),
      rtmp_lib:publish(RTMP, Stream1, Path),
      rtmp_socket:setopts(RTMP, [{chunk_size, 16#200000}]),
      Stream1
  after
    1000 ->
      erlang:exit(rtmp_timeout)
  end,
  {rtmp, Socket} = rtmp_socket:get_socket(RTMP),
  inet:setopts(Socket, [{sndbuf,1024*1024}]),
  Encoder = proplists:get_value(encoder, Options),
  publish_encoder:subscribe(Encoder),
  #publisher{
    url = URL,
    rtmp = RTMP,
    stream = Stream,
    encoder = Encoder
  }.



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
  {stop, {unknown_call,Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.

handle_info({rtmp, RTMP, connected}, State) ->
  rtmp_socket:setopts(RTMP, [{active, true}]),
  {rtmp, Socket} = rtmp_socket:get_socket(RTMP),
  inet:setopts(Socket, [{sndbuf,1024*1024}]),
  {noreply, State};

handle_info({rtmp, _Socket, disconnect, _}, State) ->
  io:format("RTMP client disconnected~n"),
  {stop, normal, State};

handle_info({rtmp, _Socket, #rtmp_message{} = Message}, State) ->
  handle_message(Message, State);

handle_info(#video_frame{dts = DTS} = Frame, #publisher{rtmp = RTMP, stream = Stream} = State) ->
  send_frame(RTMP, Stream, Frame),
  {noreply, State#publisher{last_dts = DTS}};

handle_info(dump_status, #publisher{encoder = Encoder, last_dts = LastDTS} = State) ->
  EncStatus = publish_encoder:status(Encoder),
  BufferedFrames = proplists:get_value(buffered_frames, EncStatus),
  AbsDelta = proplists:get_value(abs_delta, EncStatus),
  BufInfo = case proplists:get_value(buffer, EncStatus) of
    L when length(L) >= 2 ->
      {_,L1} = lists:nth(1, L),
      {_,L2} = lists:nth(length(L), L),
      [L1,L2, {delta,L2-L1},{frames, round((L2-L1)*32 / 1024)}];
    L -> L
  end,
  error_logger:info_msg("buffered:~p(~p) last_dts:~p abs_delta:~p delay:~p~n", [BufferedFrames, BufInfo, LastDTS, AbsDelta, AbsDelta - LastDTS]),
  {noreply, State};

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.


handle_message(#rtmp_message{type = invoke, body = Funcall}, State) ->
  handle_invoke(Funcall, State);

handle_message(#rtmp_message{type = Type}, State) when Type == ack_read orelse Type == pong ->
  {noreply, State};

handle_message(#rtmp_message{} = Message, State) ->
  io:format("Unknown message ~p~n", [Message]),
  {noreply, State}.

handle_invoke(#rtmp_funcall{command = <<"connect">>} = _AMF, #publisher{rtmp = RTMP} = State) ->
  rtmp_lib:accept_connection(RTMP),
  {noreply, State};

handle_invoke(#rtmp_funcall{command = <<"createStream">>} = AMF, #publisher{rtmp = RTMP} = State) ->
  Stream = 1,
  rtmp_lib:reply(RTMP, AMF, [Stream]),
  {noreply, State#publisher{stream = Stream}};

handle_invoke(#rtmp_funcall{command = <<"play">>, stream_id = StreamId, args = [null, Camera |_]} = _AMF, #publisher{rtmp = RTMP} = State) ->
  rtmp_lib:play_start(RTMP, StreamId, 0, live),
  Encoder = binary_to_existing_atom(Camera, latin1),
  publish_encoder:subscribe(Encoder),
  {noreply, State#publisher{encoder = Encoder}};

handle_invoke(#rtmp_funcall{command = <<"onStatus">>}, #publisher{} = State) ->
  {noreply, State};

handle_invoke(AMF, State) ->
  io:format("Unknown funcall ~p~n", [AMF]),
  {noreply, State}.


terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
