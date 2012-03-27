%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009-2012 Max Lapshin
%%% @doc        Starts in-process http stream
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(publisher_rtmp).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").


-export([start_link/3, stop/1]).
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
  gen_server:start_link({local, publisher_instance}, ?MODULE, [Type, RTMP, Options], []).


stop(RTMP) ->
  RTMP ! exit.

-define(RECHECK_SCHEDULE, 10000).


init([Type, URL, Options]) ->
  Publisher = case Type of
    passive -> init_passive(URL, Options);
    active  -> init_active(URL, Options)
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
      {rtmp, _UserInfo, Host, _Port, _Path, _Query} = http_uri2:parse(URL),

      {_HostPort, [$/ | FullPathWithQuery]} = http_uri2:extract_path_with_query(URL),
      {match, [App | Path]} = re:run(FullPathWithQuery, "([^\\/]+)/(.*)", [{capture,all_but_first,list}]),
      
      rtmp_socket:setopts(RTMP, [{active, true}]),
      TcUrl = list_to_binary("rtmp://"++ Host ++"/" ++ App),
      rtmp_lib:connect(RTMP, [{app, list_to_binary(App)}, {tcUrl, TcUrl}]),
      Stream1 = rtmp_lib:createStream(RTMP),
      rtmp_lib:publish(RTMP, Stream1, Path),
      rtmp_socket:setopts(RTMP, [{chunk_size, 16#200000}]),
      Stream1
  after
    1000 ->
      erlang:exit(rtmp_timeout)
  end,
  {rtmp, Socket} = rtmp_socket:get_socket(RTMP),
  inet:setopts(Socket, [{sndbuf,16*1024*1024}]),
  #publisher{
    url = URL,
    rtmp = RTMP,
    options = Options,
    stream = Stream
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

handle_info(dump_status, #publisher{encoder = undefined} = State) ->
  error_logger:info_msg("not encoding~n"),
  {noreply, State};

handle_info(dump_status, #publisher{encoder = Encoder, last_dts = LastDTS} = State) ->
  EncStatus = publish_encoder:status(Encoder),
  BufferedFrames = proplists:get_value(buffered_frames, EncStatus),
  AbsDelta = proplists:get_value(abs_delta, EncStatus),
  BufInfo = case proplists:get_value(buffer, EncStatus, 0) of
    L when length(L) >= 2 ->
      {_,L1} = lists:nth(1, L),
      {_,L2} = lists:nth(length(L), L),
      [L1,L2, {delta,L2-L1},{frames, round((L2-L1)*32 / 1024)}];
    L -> L
  end,
  error_logger:info_msg("buffered:~p(~p) last_dts:~p abs_delta:~p delay:~p~n", [BufferedFrames, BufInfo, LastDTS, AbsDelta, AbsDelta - LastDTS]),
  {noreply, State};

handle_info(exit, #publisher{} = State) ->
  {stop, normal, State};

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

handle_invoke(#rtmp_funcall{command = <<"onStatus">>, args = [null, {object, Status}]}, #publisher{} = State) ->
  Code = proplists:get_value(code, Status),
  handle_status(Code, Status, State);

handle_invoke(AMF, State) ->
  io:format("Unknown funcall ~p~n", [AMF]),
  {noreply, State}.

handle_status(<<"NetStream.Publish.Start">>, _Status, #publisher{options = Options} = State) ->
  Encoder = proplists:get_value(encoder, Options),
  publish_encoder:subscribe(Encoder),
  {noreply, State#publisher{encoder = Encoder}};

handle_status(Code, _Status, State) ->
  io:format("onStatus: ~p~n", [Code]),
  {noreply, State}.


terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.


