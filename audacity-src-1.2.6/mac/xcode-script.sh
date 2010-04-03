#!/bin/sh
echo "This script should be run once after building all of the libraries"
echo "in lib-src, but before trying to compiling Audacity with Xcode."
cp -f ../lib-src/allegro/allegro.a ../lib-src/allegro/liballegro.a
cp -f ../lib-src/expat/expat.a ../lib-src/expat/libexpat.a
cp -f ../lib-src/soundtouch/soundtouch.a ../lib-src/soundtouch/libsoundtouch.a
cp -f ../lib-src/portaudio/pa_mac_core/portaudio.a ../lib-src/portaudio/pa_mac_core/libportaudio.a
cp -f ../lib-src/portmixer/px_mac_core/portmixer.a ../lib-src/portmixer/px_mac_core/libportmixer.a
echo "Done."
