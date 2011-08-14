#!/usr/bin/env escript
%%! -smp enable

% -include_lib("erlmedia/include/video_frame.hrl").

-define(TIME, 3).

main([]) ->
  io:format("UVC streamer~n"),
  code:add_pathz("uvc/ebin"),
  code:add_pathz("jpeg/ebin"),
  code:add_pathz("h264/ebin"),
  code:add_pathz("publisher/ebin"),
  code:add_pathz("../erlyvideo/apps/rtmp/ebin"),
  code:add_pathz("../erlyvideo/apps/erlmedia/ebin"),
  code:add_pathz("../erlyvideo/apps/amf/ebin"),
  [code:add_pathz(P) || P <- filelib:wildcard("../erlyvideo/lib/*/ebin")],
  application:start(rtmp),
  application:start(publisher),
  io:format("Starting~n"),
  Size = {640,360},
  %Size = {960,540},
  %Size = {1280,720},
  FPS = 20,
  {ok, Publisher} = publisher:publish("rtmp://192.168.0.103/cam5", [{device,0},{debug,false},{size,Size},{fps,FPS},{arecord,"default:CARD=U0x46d0x823"}]),
  receive
    stop -> ok
  end,
  ok.


