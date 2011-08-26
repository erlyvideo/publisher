-module(publish_encoder).
-author('Max Lapshin <max@maxidoors.ru>').

-behaviour(gen_server).
-include_lib("erlmedia/include/video_frame.hrl").


%% External API
-export([start_link/2]).
-export([x264_helper/2, faac_helper/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(encoder, {
  consumer,
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
  audio_count = 0,
  video_count = 0
}).


start_link(Consumer, Options) ->
  gen_server:start_link(?MODULE, [Consumer, Options], []).

init([Consumer, Options]) ->
  erlang:monitor(process, Consumer),
  
  {ok, UVC} = uvc:capture([{format,yuv},{consumer,self()}|Options]),
  % UVC = undefined,

  put(uvc_debug, proplists:get_value(debug, Options)),
  
  SampleRate = 32000,
  Channels = 2,
  Arecord = proplists:get_value(asound, Options),
  %{ok, Capture} = alsa:start(SampleRate, Channels),
  Capture = open_port({spawn, "arecord --disable-resample -c 2 -D " ++ Arecord ++ " -r 32000 -f S16_LE"}, [stream, binary]),
  put(pcm_buf, <<>>),
  put(pcm_dts, 0),
  AACOptions = [{sample_rate,SampleRate},{channels,Channels}],
  {ok, AACEnc, AConfig} = proc_lib:start_link(?MODULE, faac_helper, [self(), AACOptions]),
  
  Consumer ! AConfig,
  
  {W,H} = proplists:get_value(size, Options),
  H264Config = proplists:get_value(h264_config, Options, "h264/encoder.preset"),
  X264Options = [{width,W},{height,H},{config,H264Config},{annexb,false}|Options],
  {ok, X264, VConfig} = proc_lib:start_link(?MODULE, x264_helper, [self(), X264Options]),
  
  Consumer ! VConfig,
  
  {ok, #encoder{
    consumer = Consumer,
    options = Options,
    uvc = UVC,
    audio = Capture,
    faac = AACEnc,
    width = W,
    height = H,
    x264 = X264,
    start = erlang:now()
  }}.


x264_helper(Master, Options) ->
  {ok, X264, VConfig} = x264:init(Options),
  erlang:monitor(process, Master),
  proc_lib:init_ack({ok, self(), VConfig}),
  x264_loop(Master, X264).

x264_loop(Master, X264) ->
  receive
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
  faac_loop(Master, AACEnc).


faac_loop(Master, AAC) ->
  receive
    {alsa, PCM, PTS} ->
      case faac:encode(AAC, PCM) of
        undefined -> ok;
        #video_frame{} = AFrame -> Master ! AFrame#video_frame{dts = PTS, pts = PTS}
      end,
      faac_loop(Master, AAC);
    Else ->
      io:format("faac_loop is stopping: ~p~n", [Else])  
  end.


drop() ->
  % {message_queue_len,Len} = process_info(self(), message_queue_len),
  % if
  %   Len > 10 -> drop(0);
  %   true -> 0
  % end.
  drop(0).


drop(Count) ->
  receive
    {uvc, _UVC, _Codec, _PTS, _Jpeg} -> drop(Count + 1);
    {yuv, _YUV, _PTS} -> drop(Count + 1)
  after
    0 -> 
      if
        Count > 0 -> error_logger:warning_msg("Drop ~p frames in publisher~n", [Count]);
        true -> ok
      end
  end.

  

handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.

handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.



enqueue(#video_frame{} = Frame, #encoder{buffer = Buf1, consumer = Consumer, start = Start} = State) ->
  Buf2 = lists:keysort(#video_frame.dts, [Frame|Buf1]),
  AbsDelta = timer:now_diff(erlang:now(), Start) div 1000,
  {Buf3, ToSend} = try_flush(Buf2, []),
  lists:foreach(fun(F1) ->
    case get(uvc_debug) of
      true ->
        io:format("~4s ~8B ~8B ~8B ~8B~n", [F1#video_frame.codec, F1#video_frame.dts, F1#video_frame.pts, AbsDelta, AbsDelta - F1#video_frame.dts]);
      _ -> ok
    end,
    Consumer ! F1
  end, ToSend),
  State#encoder{buffer = Buf3}.

try_flush([F1|F2] = Frames, ToSend) ->
  Contents = [C || #video_frame{content = C} <- Frames],
  HasVideo = lists:member(video, Contents),
  HasAudio = lists:member(audio, Contents),
  if
    HasVideo andalso HasAudio -> try_flush(F2, [F1|ToSend]);
    true -> {Frames, lists:reverse(ToSend)}
  end.


% handle_info({uvc, UVC, Codec, PTS1, RAW}, #encoder{base_vpts = undefined} = State) ->
%   handle_info({uvc, UVC, Codec, 0, RAW}, State#encoder{base_vpts = PTS1});
% 
% handle_info({uvc, UVC, Codec, PTS1, RAW}, State) ->
%   {noreply, State};

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

handle_info(#video_frame{} = Frame, #encoder{} = State) ->
  {noreply, enqueue(Frame, State)};

handle_info({yuv, YUV, PTS}, #encoder{x264 = X264} = State) ->
  X264 ! {yuv, YUV, PTS},

  VideoCount = State#encoder.video_count + 1,
  % ?D({v,VideoCount, VideoCount*50, timer:now_diff(erlang:now(),State#encoder.start) div 1000, Drop}),
  {noreply, State#encoder{video_count = VideoCount}};

handle_info({Capture, {data, Raw}}, State) ->
  case <<(get(pcm_buf))/binary, Raw/binary>> of
    <<Bin:4096/binary, Rest/binary>> ->
      put(pcm_buf, Rest),
      DTS = get(pcm_dts) div (32*4),
      put(pcm_dts, get(pcm_dts)+size(Bin)),
      handle_info({alsa, Capture, DTS, Bin}, State);
    Bin ->
      put(pcm_buf, Bin),
      {noreply, State}
  end;

handle_info({alsa, _Capture, DTS, PCM}, #encoder{faac = AACEnc} = State) ->
  AACEnc ! {alsa, PCM, DTS},

  AudioCount = State#encoder.audio_count + (size(PCM) div 2),
  % AbsDelta = timer:now_diff(erlang:now(),State#encoder.start) div 1000,
  % StreamDelta = State#encoder.audio_count div (32*2),
  % ?D({a, DTS, StreamDelta, AbsDelta, AbsDelta - StreamDelta}),
  {noreply, State#encoder{audio_count = AudioCount}};

handle_info({'DOWN', _, process, _Client, _Reason}, Server) ->
  {stop, normal, Server};

handle_info(_Info, State) ->
  {stop, {unknown_message, _Info}, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
