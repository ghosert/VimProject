all: px_test

CFLAGS = -g -Wall -O -I../include

px_test: px_test.o ../include/portmixer.h
	gcc -g -o px_test px_test.o -L../lib -lportaudio -lportmixer -lwinmm

px_test.o: px_test.c ../include/portmixer.h
	gcc $(CFLAGS) -c px_test.c

