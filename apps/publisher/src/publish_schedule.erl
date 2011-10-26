-module(publish_schedule).
-author('Max Lapshin <max@maxidoors.ru>').

-export([fetch/1, parse/1]).
-export([is_streaming_scheduled/1, is_streaming_scheduled/2]).


fetch(URL) ->
  {ok, _Headers, Body} = http_stream:get_with_body(URL, []),
  parse(Body).

parse(JSON) ->
  Info = jsonerl:decode(JSON),
  Schedule = parse_schedule(proplists:get_value(<<"schedule">>, Info)),
  Mode = proplists:get_value(<<"mode">>, Info),
  io:format("Schedule: ~s: ~p~n", [Mode, Schedule]),
  {ok, {Mode, Schedule}}.


parse_schedule(Schedule) ->
  lists:map(fun(Info) ->
    Day = proplists:get_value(<<"day">>, Info),
    SegmentFun = fun(Segment) ->
      Start = parse_time(proplists:get_value(<<"start">>, Segment)),
      Finish = parse_time(proplists:get_value(<<"finish">>, Segment)),
      [{start,Start},{finish,Finish}]
    end,
    [{day,Day},{segments, [SegmentFun(Segment) || Segment <- proplists:get_value(<<"segments">>, Info)]}]
  end, Schedule).

parse_time(Time) ->
  [T1,T2] = string:tokens(binary_to_list(Time), ":"),
  {list_to_integer(T1), list_to_integer(T2), 0}.

is_streaming_scheduled(Schedule) ->
  is_streaming_scheduled(Schedule, erlang:localtime()).

is_streaming_scheduled(Schedule, {Date, Time}) ->
  Day = calendar:day_of_the_week(Date),
  case proplists:get_value(Day, Schedule) of
    undefined -> 
      true;
    Segments ->
      [] =/= lists:filter(fun(Segment) ->
        time_in_segment(Time,Segment)
      end, Segments)
  end.

time_in_segment(Time, Segment) ->
  Start = proplists:get_value(start, Segment),
  Finish = proplists:get_value(finish, Segment),
  time_le(Start, Time) andalso time_le(Time, Finish).

time_le({H1,M1,S1}, {H2,M2,S2}) ->
  H1 =< H2 andalso M1 =< M2 andalso S1 =< S2.
