Publisher
=========


This software allows you to:
1) capture video from USB/RTSP camera + audio from USB camera/external microphone
2) encode it to H.264/AAC
3) publish it to streaming server erlyvideo  http://erlyvideo.org/


Clone it, make, edit self-descriptive publisher.conf and use runit folder to start it via "runit" software.


Debian installation
-------------------

```
apt-get install build-essential erlang-nox git libasound2-dev libfaac-dev libx264-dev libswscale-dev
git clone git://github.com/erlyvideo/publisher.git
cd publisher
make linux
./run
```