-module(publisher_reconnect).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(URL, Options) ->
  gen_server:start_link(?MODULE, [URL, Options], []).

-record(reconnector, {
  url,
  options,
  counter = 0
}).

init([URL, Options]) ->
  self() ! reconnect,
  {ok, #reconnector{url = URL, options = Options}}.

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.


handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.


handle_info(reconnect, #reconnector{url = URL, options = Options, counter = Counter} = State) ->
  case publisher_sup:start_publisher(active, URL, Options) of
    {ok, Pid} ->
      erlang:monitor(process, Pid),
      {noreply, State#reconnector{counter = 0}};
    _Else ->
      timer:send_after(Counter * 200, reconnect),
      {noreply, State#reconnector{counter = Counter + 1}}
  end;

handle_info(Info, State) ->
  {stop, {unknown_state, Info}, State}.


terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
