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
-module(publish_schedule).
-author('Max Lapshin <max@maxidoors.ru>').

-export([fetch/1, parse/1]).
-export([is_streaming_scheduled/1, is_streaming_scheduled/2]).

-export([time_in_segment/2, time_le/2]).

-record(schedule, {
  url,
  mode,
  work_now,
  schedule
}).

fetch(URL) ->
  try http_stream_pub:get_with_body(URL, [{timeout,5000}]) of
    {ok, Socket, _Headers, Body} ->
      gen_tcp:close(Socket),
      {ok, Schedule} = parse(Body),
      {ok, Schedule#schedule{url = URL}}
  catch
    Class:Error ->
      error_logger:error_msg("Failed to fetch schedule from ~s: ~p:~p~n~p~n", [URL, Class, Error, erlang:get_stacktrace()]),
      {ok, undefined}
  end.

parse(JSON) ->
  Info = jsonerl:decode(JSON),
  Schedule = parse_schedule(proplists:get_value(<<"schedule">>, Info)),
  Mode = proplists:get_value(<<"mode">>, Info),
  WorkNow = proplists:get_value(<<"work_now">>, Info),
  {ok, #schedule{mode = Mode, schedule = Schedule, work_now = WorkNow}}.


parse_schedule(Schedule) ->
  lists:map(fun(Info) ->
    Day = proplists:get_value(<<"day">>, Info),
    SegmentFun = fun(Segment) ->
      Start = parse_time(proplists:get_value(<<"start">>, Segment)),
      Finish = parse_time(proplists:get_value(<<"finish">>, Segment)),
      {Start,Finish}
    end,
    {Day,[SegmentFun(Segment) || Segment <- proplists:get_value(<<"segments">>, Info)]}
  end, Schedule).

parse_time(Time) ->
  [T1,T2] = string:tokens(binary_to_list(Time), ":"),
  {list_to_integer(T1), list_to_integer(T2), 0}.

is_streaming_scheduled(#schedule{work_now = WorkNow} = _Schedule) ->
  %Now = erlang:localtime(),
  %Result = is_streaming_scheduled(Schedule, Now),
  % io:format("scheduled(~p)? ~p, ~p  in ~p~n", [Result, Now, calendar:day_of_the_week(element(1,Now)), Schedule#schedule.schedule]),
  %Result;
  WorkNow;
  

is_streaming_scheduled(_) ->
  undefined.

is_streaming_scheduled(#schedule{schedule = Schedule}, {Date, Time}) ->
  Day = calendar:day_of_the_week(Date),
  case proplists:get_value(Day, Schedule) of
    undefined ->
      true;
    Segments ->
      % io:format("Segments: ~p ~p~n", [Time, lists:filter(fun(Segment) ->
      %   time_in_segment(Time,Segment)
      % end, Segments)]),
      [] =/= lists:filter(fun(Segment) ->
        time_in_segment(Time,Segment)
      end, Segments)
  end.

time_in_segment(Time, {Start, Finish}) ->
  time_le(Start, Time) andalso time_le(Time, Finish).

time_le({H1,M1,S1}, {H2,M2,S2}) ->
  H1*3600+M1*60+S1 =< H2*3600+M2*60+S2.




