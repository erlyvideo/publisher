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
-module(publisher_sup).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_publish_reconnector/3, start_listener/2, start_encoder/2, start_publisher/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_publish_reconnector(URL, Name, Options) ->
  supervisor:start_child(?MODULE, {Name, 
    {publisher_reconnect, start_link, [URL, Options]},
    permanent,
    10000,
    worker,
    [publisher]
  }).

start_publisher(Type, URL, Options) ->
  supervisor:start_child(publisher_rtmp_sup, [Type, URL, Options]).

start_listener(Listen, Options) ->
  rtmp_socket:start_server(Listen, publish_listener1, publish_listener, [Options]).


start_encoder(Name, Options) ->
  Sup = list_to_atom(atom_to_list(Name) ++ "_sup"),
  supervisor:start_child(?MODULE, {Sup,
    {publish_encoder, start_link, [Name, Options]},
    permanent,
    10000,
    worker,
    [publish_encoder]
  }).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([publisher_rtmp]) ->
  {ok, {{simple_one_for_one, 1, 100}, [
    {undefined,                               % Id       = internal id
    {publisher_rtmp,start_link,[]},                  % StartFun = {M, F, A}
    temporary,                               % Restart  = permanent | transient | temporary
    2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
    worker,                                  % Type     = worker | supervisor
    []                                       % Modules  = [Module] | dynamic
    }
  ]}};

init([]) ->
  Supervisors = [
  {   publisher_rtmp_sup,
      {supervisor,start_link,[{local, publisher_rtmp_sup}, ?MODULE, [publisher_rtmp]]},
      permanent,                               % Restart  = permanent | transient | temporary
      infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
      supervisor,                              % Type     = worker | supervisor
      []                                       % Modules  = [Module] | dynamic
  }
  ],
  {ok, { {one_for_one, 5, 10}, Supervisors} }.

