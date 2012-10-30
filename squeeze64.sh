#!/bin/sh


echo "deb http://www.deb-multimedia.org squeeze main non-free" > /etc/apt/sources.list.d/multimedia.list

apt-get update

apt-get -y --force-yes install deb-multimedia-keyring
apt-get -y --force-yes install build-essential erlang-nox git libx264-dev libfaac-dev libasound-dev libswscale-dev


git clone git://github.com/erlyvideo/publisher
cd publisher
make linux

