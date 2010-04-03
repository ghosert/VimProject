all: test

CFLAGS = -g -Wall -O -I./include

test:
	gcc $(CFLAGS) -c test.c -o bin\test.o
	gcc -g -o bin\test bin\test.o -L./lib -lportaudio -lportmixer -lwinmm