%%% @author     Max Lapshin <max@maxidoors.ru>
%%% @copyright  2009-2012 Max Lapshin
%%% @doc        Starts in-process http stream
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%% This file is part of erlyvideo.
%%% 
%%% erlyvideo is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% erlyvideo is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with erlyvideo.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%%---------------------------------------------------------------------------------------
-module(publisher).
-author('Max Lapshin <max@maxidoors.ru>').

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-export([publish/3, listen/2, encode/2, run/0]).

run() ->
  os:putenv("LD_LIBRARY_PATH", "deps/h264/priv"),
  [code:add_pathz(P) || P <- filelib:wildcard("deps/*/ebin")],
  [code:add_pathz(P) || P <- filelib:wildcard("../erlyvideo/apps/*/ebin")],
  [code:add_pathz(P) || P <- filelib:wildcard("/opt/erlyvideo/lib/*/ebin")],
  application:start(sasl),
  application:start(rtmp),
  application:start(gproc),
  application:start(publisher),
  io:format("Starting~n"),
  {ok, Config, ConfigPath} = file:path_consult([".", "/media/usb", "/etc/publisher"], "publisher.conf"),
  io:format("Read config from ~s~n", [ConfigPath]),
  
  load_config(Config),
  {ok, erlang:whereis(publisher_sup)}.
  


load_config([{capture, Camera, Options}|Config]) ->
  publisher:encode(Camera, Options),
  load_config(Config);
  
load_config([{publish, Camera, Publisher, RTMP}|Config]) ->
  load_config([{publish, Camera, Publisher, RTMP, []}|Config]);

load_config([{publish, Camera, Publisher, RTMP, Options}|Config]) ->
  publisher:publish(RTMP, Publisher, [{encoder,Camera}|Options]),
  load_config(Config);

load_config([{listen, Port}|Config]) ->
  load_config([{listen, Port, []}|Config]);


load_config([{listen, Port, Options}|Config]) ->
  publisher:listen(Port, Options),
  load_config(Config);

load_config([]) ->
  ok.


encode(Name, Options) ->
  publisher_sup:start_encoder(Name, Options).

publish(URL, Name, Options) ->
  publisher_sup:start_publish_reconnector(URL, Name, Options).

listen(Listen, Options) ->
  publisher_sup:start_listener(Listen, Options).



  
