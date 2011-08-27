-module(publisher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_publisher/2, start_listener/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_publisher(URL, Options) ->
  Name = list_to_atom("publisher_"++URL),
  supervisor:start_child(?MODULE, {Name, 
    {publisher, start_link, [URL, Options]},
    permanent,
    10000,
    worker,
    [publisher]
  }).

start_listener(Listen, Options) ->
  io:format("Start listener ~p~n", [Listen]),
  rtmp_socket:start_server(Listen, publish_listener1, publish_listener, [Options]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 5, 10}, []} }.

