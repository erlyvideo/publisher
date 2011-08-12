-module(uvc_publisher).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-export([publish/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(publisher, {
  url,
  options,
  uvc,
  x264,
  audio,
  faac,
  width,
  height,
  rtmp,
  base_vpts,
  start,
  stream,
  buffer = [],
  audio_count = 0,
  video_count = 0
}).

publish(URL, Options) ->
  gen_server:start_link(?MODULE, [URL, Options], []).




init([URL, Options]) ->
  {ok, UVC} = uvc4erl:open(Options),
  % UVC = undefined,
  
  {W,H} = proplists:get_value(size, Options),
  {ok, RTMP} = rtmp_socket:connect(URL),
  Stream = receive
    {rtmp, RTMP, connected} ->
      rtmp_socket:setopts(RTMP, [{active, true}]),
      rtmp_lib:connect(RTMP, [{app, <<"live">>}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
      Stream1 = rtmp_lib:createStream(RTMP),
      {rtmp, _UserInfo, _Host, _Port, [$/ | Path], _Query} = http_uri2:parse(URL),
      rtmp_lib:publish(RTMP, Stream1, Path),
      Stream1
  after
    1000 ->
      erlang:exit(rtmp_timeout)
  end,
  
  SampleRate = 32000,
  Channels = 2,
  % {ok, Capture} = audiocapture:start(SampleRate, Channels),
  Capture = open_port({spawn, "arecord --disable-resample -c 2 -D default:CARD=U0x46d0x821 -r 32000 -f S16_LE"}, [stream, binary]),
  put(pcm_buf, <<>>),
  put(pcm_dts, 0),
  {ok, AACEnc, AAConfig} = ems_sound2:init_faac(<<SampleRate:32/little, Channels:32/little>>),
  
  send_frame(RTMP, Stream, #video_frame{
    content = audio,
    flavor = config,
    codec = aac,
    sound = {stereo, bit16, rate44},
    pts = 0, 
    dts = 0,
    body = AAConfig
  }),
  
  {ok, X264, VConfig} = ems_video:init_x264([{width,W},{height,H},{config,"h264/priv/encoder.preset"},{annexb,false}]),
  send_frame(RTMP, Stream, #video_frame{
    content = video,
    flavor = config,
    codec = h264,
    pts = 0, 
    dts = 0,
    body = VConfig
  }),
  
  
  {ok, #publisher{
    url = URL,
    options = Options,
    uvc = UVC,
    audio = Capture,
    faac = AACEnc,
    width = W,
    height = H,
    rtmp = RTMP,
    x264 = X264,
    stream = Stream,
    start = erlang:now()
  }}.
  
drop() ->
  drop(0).

drop(Count) ->
  receive
    {uvc4erl, _UVC, _Codec, _PTS, _Jpeg} -> drop(Count + 1)
  after
    0 -> Count
  end.


channel_id(#video_frame{content = metadata}) -> 4;
channel_id(#video_frame{content = audio}) -> 5;
channel_id(#video_frame{content = video}) -> 6.


rtmp_message(#video_frame{dts = DTS, content = Type} = Frame, StreamId) ->
  #rtmp_message{
    channel_id = channel_id(Frame), 
    timestamp = DTS,
    type = Type,
    stream_id = StreamId,
    body = flv_video_frame:encode(Frame)}.

send_frame(RTMP, Stream, Frame) ->
  Message = rtmp_message(Frame, Stream),
	rtmp_socket:send(RTMP, Message).

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.
  


enqueue(#video_frame{} = Frame, #publisher{buffer = Buf1, rtmp = RTMP, stream = Stream} = State) ->
  Buf2 = lists:keysort(#video_frame.dts, [Frame|Buf1]),
  Buf3 = case Buf2 of
    [F1|Frames] when length(Frames) >= 6 ->
      io:format("~4s ~8B ~8B~n", [F1#video_frame.codec, F1#video_frame.dts, timer:now_diff(erlang:now(), State#publisher.start) div 1000]),
      send_frame(RTMP, Stream, F1),
      Frames;
    _ ->
      Buf2
  end,
  State#publisher{buffer = Buf3}.

% handle_info({uvc4erl, UVC, Codec, PTS1, RAW}, #publisher{base_vpts = undefined} = State) ->
%   handle_info({uvc4erl, UVC, Codec, 0, RAW}, State#publisher{base_vpts = PTS1});
% 
% handle_info({uvc4erl, UVC, Codec, PTS1, RAW}, State) ->
%   {noreply, State};

handle_info({uvc4erl, UVC, Codec, _PTS1, Jpeg}, #publisher{base_vpts = BaseVPTS, width = Width, height = Height, x264 = X264} = State) ->
  % PTS = PTS1 - BaseVPTS,
  Drop = drop(),
  % Drop = 0,

  T1 = erlang:now(),
  PTS = timer:now_diff(T1, State#publisher.start) div 1000,
  YUV = case Codec of
    jpeg -> {Y, Width, Height} = jpeg:decode(Jpeg), Y;
    yuv -> Jpeg
  end,
  T2 = erlang:now(),
  State1 = case ems_video:yuv_x264(X264, YUV, PTS) of
    ok -> State;
    {ok, Flavor, PTSEnc, DTSEnc, H264} ->
      T3 = erlang:now(),
      enqueue(#video_frame{
        content = video,
        flavor = Flavor,
        codec = h264,
        pts = PTSEnc,
        dts = DTSEnc,
        body = H264
      }, State)
  
    % file:write(get(file), H264)
  end,
  
  VideoCount = State#publisher.video_count + 1,
  % ?D({v,VideoCount, VideoCount*50, timer:now_diff(erlang:now(),State#publisher.start) div 1000, Drop}),
  {noreply, State1#publisher{video_count = VideoCount}};

handle_info({Capture, {data, Raw}}, State) ->
  case <<(get(pcm_buf))/binary, Raw/binary>> of
    <<Bin:4096/binary, Rest/binary>> ->
      put(pcm_buf, Rest),
      DTS = get(pcm_dts) div (32*4),
      put(pcm_dts, get(pcm_dts)+size(Bin)),
      handle_info({audiocapture, Capture, DTS, Bin}, State);
    Bin ->
      put(pcm_buf, Bin),
      {noreply, State}
  end;

handle_info({audiocapture, Capture, DTS, PCM}, #publisher{faac = AACEnc} = State) ->
  T1 = erlang:now(),
  State1 = case ems_sound2:pcm_aac(AACEnc, PCM) of
    undefined -> State;
    AAC -> 
      T2 = erlang:now(),
      enqueue(#video_frame{
        content = audio,
        flavor = frame,
        codec = aac,
        sound = {stereo, bit16, rate44},
        pts = DTS,
        dts = DTS,
        body = AAC
      }, State)
  end,
  
  AudioCount = State#publisher.audio_count + (size(PCM) div 2),
  AbsDelta = timer:now_diff(erlang:now(),State#publisher.start) div 1000,
  StreamDelta = State#publisher.audio_count div (32*2),
  % ?D({a, DTS, StreamDelta, AbsDelta, AbsDelta - StreamDelta}),
  {noreply, State1#publisher{audio_count = AudioCount}};

handle_info({rtmp, _, _} = Msg, State) ->
  io:format("rtmp: ~p~n", [Msg]),
  {noreply, State};

handle_info(Else, State) ->
  {stop, {undefined_message,Else}, State}.

terminate(_Reason, _State) -> ok.

