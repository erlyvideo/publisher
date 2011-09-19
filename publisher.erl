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
  code:add_pathz("apps/publisher/ebin"),
  Root = filename:join(filename:dirname(escript:script_name()), ".."),
  code:add_pathz(Root++"/ebin"),
  publisher:run(),
  receive
    stop -> ok
  end,
  ok.


