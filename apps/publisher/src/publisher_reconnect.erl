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
  publisher_ref,
  schedule_url,
  schedule,
  counter = 0
}).

-define(CHECK_TIMEOUT, 2340).
-define(SCHEDULE_TIMEOUT, 3000).


init([URL, Options]) ->
  ScheduleUrl = proplists:get_value(schedule, Options),
  self() ! check,
  timer:send_interval(?SCHEDULE_TIMEOUT, refresh_schedule),
  {ok, #reconnector{url = URL, options = Options, schedule_url = ScheduleUrl}}.

handle_call(Call, _From, State) ->
  {reply, {error, {unknown_call, Call}}, State}.


handle_cast(Cast, State) ->
  ?D({unknown_cast,Cast,State}),
  {noreply, State}.

handle_info(refresh_schedule, #reconnector{} = State) ->
  {noreply, State#reconnector{schedule = undefined}};

handle_info(check, #reconnector{schedule_url = URL, schedule = undefined} = State) when URL =/= undefined ->
  {ok, Schedule} = publish_schedule:fetch(URL),
  handle_info(check, State#reconnector{schedule = Schedule});

handle_info(check, #reconnector{url = URL, options = Options, counter = Counter, 
  schedule = Schedule, publisher = OldPid, publisher_ref = OldRef} = State) ->
  flush_check(),
  
  IsStreamingScheduled = publish_schedule:is_streaming_scheduled(Schedule),
  PublisherIsActive = erlang:is_process_alive(OldPid),
  
  timer:send_after(?CHECK_TIMEOUT, check),
  case {IsStreamingScheduled,PublisherIsActive} of
    {true, true} ->
      {noreply, State};
    {true, false} ->
      ?D({need_to_launch, URL}),
      case publisher_sup:start_publisher(active, URL, Options) of
        {ok, Pid} ->
          Ref = erlang:monitor(process, Pid),
          {noreply, State#reconnector{counter = 0, publisher = Pid, publisher_ref = Ref}};
        _Else ->
          {noreply, State#reconnector{counter = Counter + 1, publisher = undefined, publisher_ref = undefined}}
      end;
    {false, false} ->
      {noreply, State};
    {false, true} ->
      ?D({need_to_stop,URL}),
      erlang:demonitor(OldRef, [flush]),
      publisher_rtmp:stop(OldPid),
      {noreply, State#reconnector{publisher = undefined, publisher_ref = undefined}}
  end;
  
handle_info({'DOWN', _, process, Publisher, _}, #reconnector{publisher = Publisher} = State) ->
  self() ! check,
  {noreply, State#reconnector{publisher = undefined}};

handle_info(Info, State) ->
  ?D({unknown_message,Info,State}),
  {noreply, State}.


terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.

flush_check() ->
  receive
    check -> flush_check()
  after
    0 -> ok
  end.
  
