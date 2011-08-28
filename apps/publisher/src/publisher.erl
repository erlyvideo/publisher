-module(publisher).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-export([publish/2, listen/2, encode/2, run/0]).
-export([start_link/2]).

run() ->
  os:putenv("LD_LIBRARY_PATH", "deps/h264/priv"),
  code:add_pathz("deps/uvc/ebin"),
  code:add_pathz("deps/alsa/ebin"),
  code:add_pathz("deps/jpeg/ebin"),
  code:add_pathz("deps/h264/ebin"),
  [code:add_pathz(P) || P <- filelib:wildcard("../erlyvideo/apps/*/ebin")],
  [code:add_pathz(P) || P <- filelib:wildcard("/opt/erlyvideo/lib/*/ebin")],
  application:start(rtmp),
  application:start(publisher),
  io:format("Starting~n"),
  {ok, Config, _Path} = file:path_consult(["."], "publisher.conf"),
  
  publisher:encode(encoder1, Config),
  
  case proplists:get_value(publish, Config) of
    undefined -> io:format("Publish disabled~n");
    RTMP -> publisher:publish(RTMP, [{encoder,encoder1}|Config])
  end,
  case proplists:get_value(listen, Config) of
    undefined -> io:format("Listen disabled~n");
    Listen -> publisher:listen(Listen, [{encoder,encoder1}|Config])
  end,
  {ok, erlang:whereis(publisher_sup)}.


encode(Name, Options) ->
  publisher_sup:start_encoder(Name, Options).

publish(URL, Options) ->
  publisher_sup:start_publisher(URL, Options).

listen(Listen, Options) ->
  publisher_sup:start_listener(Listen, Options).

start_link(URL, Options) ->
  publisher_rtmp:start_link(active, URL, Options).



  