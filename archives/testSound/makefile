main:
	gcc -c -o bin/testPlay.o testPlay.c -I./include
	gcc -o bin/testPlay bin/testPlay.o -L./lib -lsndfile-1 -lportaudio -lwinmm
	copy .\lib\libsndfile-1.dll .\bin\libsndfile-1.dll
clean:
	del bin\testPlay.o bin\testPlay.exe bin\libsndfile-1.dll

