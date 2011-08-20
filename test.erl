#!/usr/bin/env escript
%%! -smp enable -A 16 -a 4096

% -include_lib("erlmedia/include/video_frame.hrl").

-define(TIME, 3).

main([]) ->
  io:format("UVC streamer~n"),
  io:format("~p~n", [erlang:system_info(scheduler_bind_type)]),
  erlang:system_flag(scheduler_bind_type, spread),
  code:add_pathz("apps/publisher/ebin"),
  publisher:run(),
  receive
    stop -> ok
  end,
  ok.


