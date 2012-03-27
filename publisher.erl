#!/usr/bin/env escript
%%! -boot start_sasl +A 4 +a 4096

% -include_lib("erlmedia/include/video_frame.hrl").

-define(TIME, 3).

main([]) ->
  io:format("UVC streamer~n"),
  %erlang:system_flag(scheduler_bind_type, spread),
  %io:format("~p~n", [erlang:system_info(scheduler_bind_type)]),
  %io:format("~p~n", [erlang:system_info(multi_scheduling)]),
  %io:format("~p~n", [{erlang:system_info(smp_support),erlang:system_info(threads),erlang:system_info(thread_pool_size),erlang:system_info(schedulers_online)}]),
  Root = filename:dirname(escript:script_name()),
  [code:add_pathz(P) || P <- filelib:wildcard(Root ++ "/apps/*/ebin")],
  [code:add_pathz(P) || P <- filelib:wildcard(Root ++ "/deps/*/ebin")],
  case file:read_file_info(Root ++ "/publisher.conf") of
    {error, enoent} -> os:cmd("cp "++Root++"/publisher.conf.sample "++Root++"/publisher.conf");
    _ -> ok
  end,
  {ok, Pid} = publisher:run(),
  erlang:monitor(process, Pid),
  receive
    stop -> ok;
    {'DOWN', _, process, Pid, _} -> ok
  end,
  ok.


