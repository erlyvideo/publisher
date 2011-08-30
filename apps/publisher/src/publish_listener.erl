-module(publish_listener).
-author('Max Lapshin <max@maxidoors.ru>').

-export([create_client/2]).

create_client(Socket, Options) ->
  publisher_sup:start_publisher(passive, Socket, Options).

