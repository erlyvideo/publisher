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
-module(publish_encoder).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("erlmedia/include/media_info.hrl").


%% External API
-export([start_link/1, start_link/2]).
-export([x264_helper/2, faac_helper/2]).
-export([status/1, subscribe/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(encoder, {
  clients = [],
  options,
  uvc,
  x264,
  audio,
  faac,
  width,
  height,
  rtmp,
  start,
  stream,
  buffer = [],
  aconfig,
  vconfig,
  last_dts = 0,
  audio_shift = 0,
  audio_count = 0,
  video_count = 0
}).


start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

start_link(Name, Options) ->
  gen_server:start_link({local, Name}, ?MODULE, [Options], []).

status(Encoder) ->
  gen_server:call(Encoder, status).
  
subscribe(Encoder) ->
  erlang:monitor(process, erlang:whereis(Encoder)),
  gen_server:call(Encoder, {subscribe, self()}).

init([Options]) ->
  process_flag(trap_exit, true),
  put(debug, proplists:get_value(debug, Options)),
  {ok, #encoder{options = Options}}.

ensure_capture(#encoder{start = undefined, clients = Clients, options = Options} = Encoder1) ->
  VideoCapture = proplists:get_value(video_capture, Options),
  {Width, Height} = proplists:get_value(size, VideoCapture),
  Meta = [
    {framerate, proplists:get_value(fps, VideoCapture)}, {width, Width}, {height, Height},
    {videocodecid, <<"avc1">>}, {audiocodecid, <<"mp4a">>}
  ],
  MetaFrame = #video_frame{
    content = metadata,
    dts = 0, pts = 0,
    body = [<<"@setDataFrame">>,<<"onMetaData">>, {object, Meta}]
  },
  [Client ! MetaFrame || Client <- Clients],
  Encoder2 = start_h264_capture(Encoder1),
  Encoder3 = start_aac_capture(Encoder2),
  Encoder3#encoder{
    last_dts = 0,
    start = erlang:now()
  };

ensure_capture(Encoder) ->
  Encoder.

start_h264_capture(#encoder{options = Options} = Encoder) ->
  VideoOptions = proplists:get_value(video_capture, Options),
  
  case proplists:get_value(type, VideoOptions, uvc) of
    uvc -> start_uvc_capture(Encoder, VideoOptions);
    rtsp -> start_rtsp_capture(Encoder, VideoOptions)
  end.

start_uvc_capture(#encoder{clients = Clients, last_dts = DTS} = Encoder, VideoOptions) ->
  {ok, UVC} = uvc:capture([{format,yuv},{consumer,self()}|VideoOptions]),
  erlang:monitor(process, UVC),
  {W,H} = proplists:get_value(size, VideoOptions),
  H264Config = proplists:get_value(config, VideoOptions, "h264/encoder.preset"),
  X264Options = [{width,W},{height,H},{config,H264Config},{annexb,false}|VideoOptions],
  {ok, X264, VConfig} = proc_lib:start_link(?MODULE, x264_helper, [self(), X264Options]),
  [Client ! VConfig#video_frame{dts = DTS, pts = DTS} || Client <- Clients],
  Encoder#encoder{uvc = UVC, vconfig = VConfig, width = W, height = H, x264 = X264}.


start_rtsp_capture(#encoder{clients = Clients, last_dts = DTS} = Encoder, VideoOptions) ->
  application:start(log4erl),
  ems_log:start(),
  application:start(rtsp),
  {ok, RTSP, MediaInfo} = rtsp_socket:read(proplists:get_value(url,VideoOptions), [{consumer,self()}]),
  % {ok, Media} = ems_media:start_link(Type, VideoOptions),
  % #media_info{video = VideoInfo} = ems_media:media_info(Media),
  #media_info{video = VideoInfo} = MediaInfo,
  length(VideoInfo) == 1 orelse erlang:error(invalid_video_stream),
  [#stream_info{} = VideoStream] = VideoInfo,
  erlang:monitor(process, RTSP),
  % ems_media:play(Media, []),
  VConfig = video_frame:config_frame(VideoStream),
  [Client ! VConfig#video_frame{dts = DTS, pts = DTS} || Client <- Clients],
  
  Encoder#encoder{vconfig = VConfig}.

start_aac_capture(#encoder{options = Options} = Encoder) ->
  case proplists:get_value(audio_capture, Options) of
    undefined -> Encoder;
    AudioOptions -> start_alsa_capture(Encoder, AudioOptions)
  end.

start_alsa_capture(#encoder{clients = Clients, last_dts = DTS} = Encoder, AudioOptions) ->
  alsa = proplists:get_value(type, AudioOptions),
  SampleRate = proplists:get_value(sample_rate, AudioOptions, 32000),
  Channels = proplists:get_value(channels, AudioOptions, 2),
  {ok, Capture} = alsa:start(SampleRate, Channels),
  AACOptions = [{sample_rate,SampleRate},{channels,Channels}],
  {ok, AACEnc, AConfig} = proc_lib:start_link(?MODULE, faac_helper, [self(), AACOptions]),
  AudioShift = proplists:get_value(audio_shift, AudioOptions, 0),
  [Client ! AConfig#video_frame{dts = DTS, pts = DTS} || Client <- Clients],
  Encoder#encoder{audio = Capture, aconfig = AConfig, faac = AACEnc, audio_shift = AudioShift}.


x264_helper(Master, Options) ->
  {ok, X264, VConfig} = x264:init(Options),
  erlang:monitor(process, Master),
  proc_lib:init_ack({ok, self(), VConfig}),
  x264_loop(Master, X264).

x264_loop(Master, X264) ->
  receive
    keyframe ->
      x264_loop(Master, X264);
      
    {yuv, YUV, PTS} ->
      drop(),
      case x264:encode(X264, YUV, PTS) of
        undefined -> ok;
        #video_frame{} = Frame -> Master ! Frame
      end,
      x264_loop(Master, X264);
    Else ->
      io:format("x264_loop is stopping: ~p~n", [Else])
  end.


faac_helper(Master, Options) ->
  {ok, AACEnc, AConfig} = faac:init(Options),
  erlang:monitor(process, Master),
  proc_lib:init_ack({ok, self(), AConfig}),
  put(prev_pts, 0),
  faac_loop(Master, AACEnc).


faac_loop(Master, AAC) ->
  receive
    {alsa, PCM, PTS} ->
      case faac:encode(AAC, PCM) of
        undefined -> ok;
        #video_frame{} = AFrame ->
          PrevPts = get(prev_pts),
          if PrevPts > PTS -> io:format("Damn! backjump of audio ~p ~p ~n", [get(prev_pts), PTS]);
            true -> ok
          end,
          put(prev_pts, PTS),
          Master ! AFrame#video_frame{dts = PTS, pts = PTS}
      end,
      faac_loop(Master, AAC);
    Else ->
      io:format("faac_loop is stopping: ~p~n", [Else])  
  end.


drop() ->
  {message_queue_len,Len} = process_info(self(), message_queue_len),
  Count = if
    Len > 100 -> drop(all, 0);
    Len > 30 -> drop(3, 0);
    Len > 10 -> drop(1, 0);
    true -> 0
  end,
  if
    Count > 0 -> error_logger:warning_msg("Drop ~p frames in publisher~n", [Count]);
    true -> ok
  end.
  


drop(Limit, Count) when is_number(Limit) andalso is_number(Count) andalso Count >= Limit ->
  Count;

drop(Limit, Count) ->
  receive
    {uvc, _UVC, _Codec, _PTS, _Jpeg} -> drop(Limit, Count + 1);
    {yuv, _YUV, _PTS} -> drop(Limit, Count + 1)
  after
    0 -> Count
  end.



handle_call(status, _From, #encoder{buffer = Buf, start = Start} = State) ->
  Status = [
    {buffer, [{C,D} || #video_frame{codec = C, dts = D} <- Buf]},
    {buffered_frames, length(Buf)},
    {abs_delta, timer:now_diff(erlang:now(), Start) div 1000}
  ],
  {reply, Status, State};

handle_call({subscribe, Client}, _From, #encoder{clients = Clients, aconfig = AConfig, vconfig = VConfig, x264 = X264, last_dts = DTS} = State) ->
  io:format("Subscribe ~p, ~p, ~p, ~p~n", [Client, DTS, VConfig, AConfig]),
  if AConfig == undefined -> ok; true -> Client ! AConfig#video_frame{dts = DTS, pts = DTS} end,
  if VConfig == undefined -> ok; true -> Client ! VConfig#video_frame{dts = DTS, pts = DTS} end,
  (catch X264 ! keyframe),
  erlang:monitor(process, Client),
  {reply, ok, ensure_capture(State#encoder{clients = [Client|Clients]})};
  

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.


-define(THRESHOLD, 10000).
check_frame_delay(#video_frame{dts = DTS} = Frame, Frames) ->
  case [true || #video_frame{dts = D} <- Frames, abs(D - DTS) > ?THRESHOLD] of
    [] -> true;
    _ ->
      io:format("Frame ~p delayed: ~p~n", [Frame, [{C,D} || #video_frame{codec = C, dts = D} <- Frames]]),
      false
  end.

enqueue(#video_frame{} = Frame, #encoder{aconfig = A, vconfig = V} = State) when A == undefined orelse V == undefined ->
  real_send([Frame], State);

enqueue(#video_frame{} = RawFrame, #encoder{buffer = Buf1} = State) ->
  Frame = shift_audio(RawFrame, State),
  Buf2 = lists:sort(fun frame_sorter/2, [Frame|Buf1]),
  {Buf3, ToSend} = try_flush(Buf2, []),
  real_send(ToSend, State#encoder{buffer = Buf3}).

shift_audio(#video_frame{content = audio, dts = DTS, pts = PTS} = Frame, #encoder{audio_shift = AudioShift}) ->
  Frame#video_frame{dts = DTS + AudioShift, pts = PTS + AudioShift};

shift_audio(Frame, _) ->
  Frame.

frame_sorter(#video_frame{dts = DTS1}, #video_frame{dts = DTS2}) when DTS1 < DTS2 -> true;
frame_sorter(#video_frame{dts = DTS, flavor = config}, #video_frame{dts = DTS, flavor = Flavor}) when Flavor =/= config -> true;
frame_sorter(#video_frame{dts = DTS, flavor = config, content = video}, #video_frame{dts = DTS, flavor = config, content = Content}) when Content=/= video -> true;
frame_sorter(#video_frame{}, #video_frame{}) -> false.


real_send(ToSend, #encoder{clients = Clients, start = Start} = State) ->
  AbsDelta = timer:now_diff(erlang:now(), Start) div 1000,
  lists:foreach(fun(F1) ->
    case get(debug) of
      true ->
        io:format("~4s ~8s ~8B ~8B ~8B ~8B~n", [F1#video_frame.codec, F1#video_frame.flavor, round(F1#video_frame.dts), round(F1#video_frame.pts), AbsDelta, round(AbsDelta - F1#video_frame.dts)]);
      _ -> ok
    end,
    [Client ! F1 || Client <- Clients]
  end, ToSend),
  State.
  

try_flush([F1|F2] = Frames, ToSend) ->
  Contents = [C || #video_frame{content = C} <- Frames],
  HasVideo = lists:member(video, Contents),
  HasAudio = lists:member(audio, Contents),
  if
    HasVideo andalso HasAudio -> try_flush(F2, [F1|ToSend]);
    true -> {Frames, lists:reverse(ToSend)}
  end.


handle_info({uvc, _UVC, yuv, PTS1, YUV}, State) ->
  case get(start_uvc_pts) of
    undefined -> put(start_uvc_pts, PTS1);
    _ -> ok
  end,
  PTS = PTS1 - get(start_uvc_pts),
  drop(),
  % T1 = erlang:now(),
  % PTS = timer:now_diff(T1, State#encoder.start) div 1000,  
  handle_info({yuv, YUV, PTS}, State);

handle_info({ems_stream, _, _}, State) ->
  {noreply, State};

handle_info({ems_stream, _, _, _}, State) ->
  {noreply, State};

handle_info(#video_frame{} = Frame, #encoder{} = State) ->
  case check_frame_delay(Frame, State#encoder.buffer) of
    true -> {noreply, enqueue(Frame, State)};
    false -> {noreply, State}
  end;

handle_info({yuv, YUV, PTS}, #encoder{x264 = X264} = State) ->
  X264 ! {yuv, YUV, PTS},

  VideoCount = State#encoder.video_count + 1,
  % ?D({v,VideoCount, VideoCount*50, timer:now_diff(erlang:now(),State#encoder.start) div 1000, Drop}),
  {noreply, State#encoder{video_count = VideoCount}};

handle_info({alsa, _Capture, DTS, PCM}, #encoder{faac = AACEnc} = State) ->
  AACEnc ! {alsa, PCM, DTS},

  % AudioCount = State#encoder.audio_count + (size(PCM) div 2),
  % AbsDelta = timer:now_diff(erlang:now(),State#encoder.start) div 1000,
  % StreamDelta = State#encoder.audio_count div (32*2),
  % ?D({a, DTS, StreamDelta, AbsDelta, AbsDelta - StreamDelta}),
  AudioCount = State#encoder.audio_count + 1,
  {noreply, State#encoder{audio_count = AudioCount}};

handle_info({'DOWN', _, process, UVC, _Reason}, #encoder{uvc = UVC} = Server) ->
  {stop, normal, Server};

handle_info({'DOWN', _, process, Client, _Reason}, #encoder{clients = Clients} = Server) ->
  case lists:member(Client, Clients) of
    true when length(Clients) == 1 ->
      {stop, normal, Server};
    true ->
      {noreply, Server#encoder{clients = lists:delete(Client, Clients)}};
    false ->
      {stop, normal, Server}  
  end;

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.

terminate(_Reason, #encoder{}) ->
  % alsa:stop(Alsa),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
