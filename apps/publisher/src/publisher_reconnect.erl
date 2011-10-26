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
  schedule_url,
  schedule
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

handle_info(check, #reconnector{url = URL, schedule_url = ScheduleUrl, options = Options, schedule = Schedule} = State) ->
  flush_check(),
  
  IsStreamingScheduled = case ScheduleUrl of
    undefined -> true;
    _ -> publish_schedule:is_streaming_scheduled(Schedule)
  end,
  OldPid = gproc:lookup_local_name({publisher,URL}),
  PublisherIsActive = is_alive(OldPid),
  
  timer:send_after(?CHECK_TIMEOUT, check),
  case {IsStreamingScheduled,PublisherIsActive} of
    {true, true} ->
      {noreply, State};
    {true, false} ->
      ?D({need_to_launch, URL}),
      publisher_sup:start_publisher(active, URL, Options),
      {noreply, State};
    {false, false} ->
      {noreply, State};
    {false, true} ->
      ?D({need_to_stop,URL,OldPid}),
      publisher_rtmp:stop(OldPid),
      timer:sleep(100),
      (catch erlang:exit(OldPid, normal)),
      (catch erlang:exit(OldPid, kill)),
      {noreply, State}
  end;
  
handle_info({'DOWN', _, process, _Publisher, _}, #reconnector{} = State) ->
  self() ! check,
  {noreply, State};

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
  
is_alive(Pid) when is_pid(Pid) -> erlang:is_process_alive(Pid);
is_alive(_) -> false.
