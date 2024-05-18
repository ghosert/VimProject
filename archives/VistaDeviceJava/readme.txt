The files under "lib" folder comes from the project VimPoject/VistaDevice/

This project VimPoject/VistaDeviceJava/ is a Java wrapper for the project above.

Steps for JNI development:
1. add/modify native method in .java file.
2. run the "run.bat", try to compile java class, produce C .h file, error happens when produce JNI DLL file because no C implement modification in .c file.
3. open c .C file and update the implementations.
4. re-run the "run.bat" to produce JNI DLL file and test it with the java code.
5. No error is thrown means you success unless you are using XP system to develop VISTA app, if so, deploy and test your codes on VISTA.
6. All the steps above are passed, in BIN folder, "run.bat" will produce audio_device.jar, native dll, jni dll. Deploy them to the other Java Apps as an audio device lib.
   Make sure the other Java Apps set -Djava.library.lib=<JNI DLL PATH> -classpath=<audio_device.jar PATH> option and put NATIVE DLL to the "windows path"(may be not the same to JNI DLL PATH)

