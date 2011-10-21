-module(publisher_reconnect).
-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-behaviour(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_link(URL, Options) ->
  gen_server:start_link(?MODULE, [URL, Options], []).

-record(reconnector, {
  url,
  options,
  publisher,
  counter = 0
}).

init([URL, Options]) ->
  self() ! reconnect,
  {ok, #reconnector{url = URL, options = Options}}.

handle_call(Call, _From, State) ->
  {reply, {error, {unknown_call, Call}}, State}.


handle_cast(Cast, State) ->
  ?D({unknown_cast,Cast,State}),
  {noreply, State}.


handle_info(reconnect, #reconnector{url = URL, options = Options, counter = Counter} = State) ->
  flush_reconnect(),
  case publisher_sup:start_publisher(active, URL, Options) of
    {ok, Pid} ->
      erlang:monitor(process, Pid),
      {noreply, State#reconnector{counter = 0, publisher = Pid}};
    _Else ->
      timer:send_after(Counter * 200, reconnect),
      {noreply, State#reconnector{counter = Counter + 1}}
  end;

handle_info({'DOWN', _, process, Publisher, _}, #reconnector{publisher = Publisher} = State) ->
  self() ! reconnect,
  {noreply, State#reconnector{publisher = undefined}};

handle_info(Info, State) ->
  ?D({unknown_message,Info,State}),
  {noreply, State}.


terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

flush_reconnect() ->
  receive
    reconnect -> flush_reconnect()
  after
    0 -> ok
  end.
  
