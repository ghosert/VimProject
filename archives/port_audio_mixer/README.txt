Build guide:
1. Download audacity source code.
2. Copy audacity-src-1.2.6\lib-src\portaudio to the current folder.
3. Copy audacity-src-1.2.6\lib-src\portmixer to the current folder.
4. Copy portaudio\pa_tests to the current folder.
4. Copy portmixer\px_tests to the current folder.
4. Run build.bat to get libs, .h files to the lib, include folder and
   produce the executable files in the pa_tests/px_tests folders.
   You may get some error informations when you run build.bat, try to solve them.

How to Use:
1. Goto pa_tests/px_tests folders to see how to invoke portaudio portmixer libs.
2. Goto portaudio/portmixer folders to see/modify the source code if you want to other enhancement.
   Remember build.bat above to re-get your changed libs.
3. Use lib/include directly to build/try your own apps with portaudio/portmixer support in the current folder.
   You can run make command on the makefile which exists in the current folder to include portaudio/portmixer libs.

