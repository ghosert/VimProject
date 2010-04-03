# Make PortAudio Library for Unix/OSS

CC = gcc

override CFLAGS += -g -O -I../pa_common
PASRC = ../pa_common/pa_lib.c pa_unix_oss.c pa_unix.c
PAINC = ../pa_common/portaudio.h
PAOBJ = ../pa_common/pa_lib.o pa_unix_oss.o pa_unix.o

portaudio.a: $(PAOBJ)
	ar ruv portaudio.a $(PAOBJ)
	ranlib portaudio.a
#	cp portaudio.a $(DEST)

clean:
	rm -f portaudio.a $(PAOBJ)

distclean: clean

%.o: %.c $(PAINC) Makefile
	$(CC) -c $(CFLAGS) $< -o $@
