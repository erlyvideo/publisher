-module(publisher).

-include_lib("erlmedia/include/video_frame.hrl").
-include_lib("rtmp/include/rtmp.hrl").

-define(D(X), io:format("~p:~p ~p~n", [?MODULE, ?LINE, X])).

-export([publish/2, run/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([x264_helper/2, faac_helper/2]).

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
  socket,
  start,
  stream,
  buffer = [],
  audio_count = 0,
  video_count = 0
}).

run() ->
  os:putenv("LD_LIBRARY_PATH", "deps/h264/priv"),
  code:add_pathz("deps/uvc/ebin"),
  code:add_pathz("deps/alsa/ebin"),
  code:add_pathz("deps/jpeg/ebin"),
  code:add_pathz("deps/h264/ebin"),
  code:add_pathz("../erlyvideo/apps/rtmp/ebin"),
  code:add_pathz("../erlyvideo/apps/erlmedia/ebin"),
  code:add_pathz("../erlyvideo/apps/amf/ebin"),
  [code:add_pathz(P) || P <- filelib:wildcard("/opt/erlyvideo/lib/*/ebin")],
  application:start(rtmp),
  application:start(publisher),
  io:format("Starting~n"),
  {ok, Config, _Path} = file:path_consult(["."], "publisher.conf"),
  RTMP = proplists:get_value(rtmp, Config, "rtmp://127.0.0.1/cam5"),
  {ok, Publisher} = publisher:publish(RTMP, Config),
  {ok, Publisher}.

publish(URL, Options) ->
  gen_server:start_link(?MODULE, [URL, Options], []).


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
      


init([URL, Options]) ->
  {ok, UVC} = uvc:capture([{format,yuv},{consumer,self()}|Options]),
  % UVC = undefined,

  put(uvc_debug, proplists:get_value(debug, Options)),
  
  {W,H} = proplists:get_value(size, Options),
  {ok, RTMP} = rtmp_socket:connect(URL),
  Stream = receive
    {rtmp, RTMP, connected} ->
      rtmp_socket:setopts(RTMP, [{active, true}]),
      rtmp_lib:connect(RTMP, [{app, <<"live">>}, {tcUrl, <<"rtmp://localhost/live/a">>}]),
      Stream1 = rtmp_lib:createStream(RTMP),
      {rtmp, _UserInfo, _Host, _Port, [$/ | Path], _Query} = http_uri2:parse(URL),
      rtmp_lib:publish(RTMP, Stream1, Path),
      rtmp_socket:setopts(RTMP, [{chunk_size, 16#200000}]),
      Stream1
  after
    1000 ->
      erlang:exit(rtmp_timeout)
  end,
  
  SampleRate = 32000,
  Channels = 2,
  Arecord = proplists:get_value(asound, Options),
  %{ok, Capture} = alsa:start(SampleRate, Channels),
  Capture = open_port({spawn, "arecord --disable-resample -c 2 -D " ++ Arecord ++ " -r 32000 -f S16_LE"}, [stream, binary]),
  put(pcm_buf, <<>>),
  put(pcm_dts, 0),
  AACOptions = [{sample_rate,SampleRate},{channels,Channels}],
  {ok, AACEnc, AConfig} = proc_lib:start_link(?MODULE, faac_helper, [self(), AACOptions]),
  
  {rtmp, Socket} = rtmp_socket:get_socket(RTMP),
  inet:setopts(Socket, [{sndbuf,1024*1024}]),
  
  send_frame(Socket, Stream, AConfig),
  
  H264Config = proplists:get_value(h264_config, Options, "h264/encoder.preset"),
  X264Options = [{width,W},{height,H},{config,H264Config},{annexb,false}|Options],
  {ok, X264, VConfig} = proc_lib:start_link(?MODULE, x264_helper, [self(), X264Options]),
  send_frame(Socket, Stream, VConfig),
  
  {ok, #publisher{
    url = URL,
    options = Options,
    uvc = UVC,
    audio = Capture,
    faac = AACEnc,
    width = W,
    height = H,
    rtmp = RTMP,
    socket = Socket,
    x264 = X264,
    stream = Stream,
    start = erlang:now()
  }}.
  
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


% channel_id(#video_frame{content = metadata}) -> 4;
% channel_id(#video_frame{content = audio}) -> 5;
% channel_id(#video_frame{content = video}) -> 6.
% 
% 
% rtmp_message(#video_frame{dts = DTS, content = Type} = Frame, StreamId) ->
%   #rtmp_message{
%     channel_id = channel_id(Frame), 
%     timestamp = DTS,
%     type = Type,
%     stream_id = StreamId,
%     body = flv_video_frame:encode(Frame)}.

send_frame(Socket, Stream, #video_frame{} = Frame) ->
  FlvFrameGen = flv:rtmp_tag_generator(Frame),
  gen_tcp:send(Socket, FlvFrameGen(0, Stream)),
  %   Message = rtmp_message(Frame, Stream),
  % rtmp_socket:send(RTMP, Message).
  ok.

handle_call(Call, _From, State) ->
  {stop, {unknown_call, Call}, State}.

handle_cast(Cast, State) ->
  {stop, {unknown_cast, Cast}, State}.
  


enqueue(#video_frame{} = Frame, #publisher{buffer = Buf1, socket = Socket, rtmp = _RTMP, stream = Stream} = State) ->
  Buf2 = lists:keysort(#video_frame.dts, [Frame|Buf1]),
  AbsDelta = timer:now_diff(erlang:now(), State#publisher.start) div 1000,
  {Buf3, ToSend} = try_flush(Buf2, []),
  lists:foreach(fun(F1) ->
    case get(uvc_debug) of
      true ->
        io:format("~4s ~8B ~8B ~8B ~8B~n", [F1#video_frame.codec, F1#video_frame.dts, F1#video_frame.pts, AbsDelta, AbsDelta - F1#video_frame.dts]);
      _ -> ok
    end,
    send_frame(Socket, Stream, F1)
  end, ToSend),
  State#publisher{buffer = Buf3}.
  
try_flush([F1|F2] = Frames, ToSend) ->
  Contents = [C || #video_frame{content = C} <- Frames],
  HasVideo = lists:member(video, Contents),
  HasAudio = lists:member(audio, Contents),
  if
    HasVideo andalso HasAudio -> {F2, [F1|ToSend]};
    true -> {Frames, ToSend}
  end.
  

% handle_info({uvc, UVC, Codec, PTS1, RAW}, #publisher{base_vpts = undefined} = State) ->
%   handle_info({uvc, UVC, Codec, 0, RAW}, State#publisher{base_vpts = PTS1});
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
  % PTS = timer:now_diff(T1, State#publisher.start) div 1000,  
  handle_info({yuv, YUV, PTS}, State);

handle_info(#video_frame{} = Frame, #publisher{} = State) ->
  {noreply, enqueue(Frame, State)};

handle_info({yuv, YUV, PTS}, #publisher{x264 = X264} = State) ->
  X264 ! {yuv, YUV, PTS},
  
  VideoCount = State#publisher.video_count + 1,
  % ?D({v,VideoCount, VideoCount*50, timer:now_diff(erlang:now(),State#publisher.start) div 1000, Drop}),
  {noreply, State#publisher{video_count = VideoCount}};

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

handle_info({alsa, _Capture, DTS, PCM}, #publisher{faac = AACEnc} = State) ->
  AACEnc ! {alsa, PCM, DTS},
  
  AudioCount = State#publisher.audio_count + (size(PCM) div 2),
  % AbsDelta = timer:now_diff(erlang:now(),State#publisher.start) div 1000,
  % StreamDelta = State#publisher.audio_count div (32*2),
  % ?D({a, DTS, StreamDelta, AbsDelta, AbsDelta - StreamDelta}),
  {noreply, State#publisher{audio_count = AudioCount}};

handle_info({rtmp, _RTMP, #rtmp_message{type = ack_read}}, State) ->
  {noreply, State};

handle_info({rtmp, _, _} = Msg, State) ->
  io:format("rtmp: ~p~n", [Msg]),
  {noreply, State};

handle_info(Else, State) ->
  {stop, {undefined_message,Else}, State}.

terminate(_Reason, _State) -> ok.

