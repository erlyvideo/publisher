-module(publish_listener).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-export([create_client/2, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

create_client(Socket, Options) ->
  start_link(Socket, Options).

-record(listener, {
  rtmp,
  stream,
  options,
  encoder
}).

start_link(RTMP, Options) ->
  gen_server:start_link(?MODULE, [RTMP, Options], []).
  


init([RTMP, Options]) ->
  {ok, #listener{
    rtmp = RTMP,
    options = Options
  }}.

handle_call(Call, _From, State) ->
  {stop, {unknown_call,Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.

handle_info({rtmp, Socket, connected}, State) ->
  rtmp_socket:setopts(Socket, [{active, true}]),
  {noreply, State};

handle_info({rtmp, _Socket, disconnect, _}, State) ->
  io:format("RTMP client disconnected~n"),
  {stop, normal, State};

handle_info({rtmp, _Socket, #rtmp_message{} = Message}, State) ->
  handle_message(Message, State);

handle_info(#video_frame{} = Frame, #listener{rtmp = RTMP, stream = Stream} = State) ->
  publisher:send_frame(RTMP, Stream, Frame),
  {noreply, State};


handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.


handle_message(#rtmp_message{type = invoke, body = Funcall}, State) ->
  handle_invoke(Funcall, State);

handle_message(#rtmp_message{} = Message, State) ->
  io:format("Unknown message ~p~n", [Message]),
  {noreply, State}.

handle_invoke(#rtmp_funcall{command = <<"connect">>} = AMF, #listener{rtmp = RTMP} = State) ->
  rtmp_lib:accept_connection(RTMP),
  {noreply, State};

handle_invoke(#rtmp_funcall{command = <<"createStream">>} = AMF, #listener{rtmp = RTMP} = State) ->
  Stream = 1,
  rtmp_lib:reply(RTMP, AMF, [Stream]),
  {noreply, State#listener{stream = Stream}};

handle_invoke(#rtmp_funcall{command = <<"play">>, stream_id = StreamId} = AMF, #listener{rtmp = RTMP, options = Options} = State) ->
  rtmp_lib:play_start(RTMP, StreamId, 0, live),
  {ok, Encoder} = publish_encoder:start_link(self(), Options),
  {noreply, State#listener{encoder = Encoder}};

handle_invoke(AMF, State) ->
  io:format("Unknown funcall ~p~n", [AMF]),
  {noreply, State}.


terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
