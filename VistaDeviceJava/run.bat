rem produce the VistaAudioDevice.class
javac -source 1.4 -target 1.4 -d ./bin com/loadtrend/lib/sound/VistaAudioDevice.java

rem produce the .h file
javah -jni -classpath ./bin com.loadtrend.lib.sound.VistaAudioDevice

rem copy lib files from VistaDevice project.
copy ..\VistaDevice\LH_RETAIL\audio-device.dll .\lib
copy ..\VistaDevice\LH_RETAIL\audio-device.lib .\lib
copy ..\VistaDevice\audio-device.h .\lib

rem produce the .dll file
make

rem copy .\lib\audio-device.dll to .\bin
copy .\lib\audio-device.dll .\bin

rem test whether java code works with dll file.
java -Djava.library.path=bin -classpath ./bin com.loadtrend.lib.sound.VistaAudioDevice

rem produce the jar file
cd bin
jar cvf audio_device.jar .\com\*
cd ..

