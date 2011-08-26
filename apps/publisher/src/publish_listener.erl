-module(publish_listener).
-author('Max Lapshin <max@maxidoors.ru>').

-export([create_client/2]).

create_client(Socket, Options) ->
  io:format("Creating socket ~p, ~p~n", [Socket, Options]),
  erlang:error(not_implemented).
  

