cd portaudio\pa_win_wmme
make
copy portaudio.a ..\..\lib\libportaudio.a
copy ..\pa_common\portaudio.h ..\..\include
copy ..\pa_common\pa_trace.h ..\..\include
cd ..\..\portmixer\px_win_wmme
make
copy portmixer.a ..\..\lib\libportmixer.a
copy ..\px_common\portmixer.h ..\..\include
cd ..\..\px_tests
make
cd ..\pa_tests
make

