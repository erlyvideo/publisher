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
  rtmp
}).

start_link(RTMP, Options) ->
  gen_server:start_link(?MODULE, [RTMP, Options], []).
  


init([RTMP, Options]) ->
  {ok, #listener{
    rtmp = RTMP
  }}.

handle_call(Call, _From, State) ->
  {stop, {unknown_call,Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.

handle_info({rtmp, Socket, connected}, State) ->
  rtmp_socket:setopts(Socket, [{active, true}]),
  {noreply, State};

handle_info({rtmp, Socket, #rtmp_message{} = Message}, State) ->
  handle_message(Message, State);

handle_info(Msg, State) ->
  {stop, {unknown_info, Msg}, State}.


handle_message(#rtmp_message{} = Message, State) ->
  io:format("Unknown message ~p~n", [Message]),
  {noreply, State}.

terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
