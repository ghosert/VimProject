# Make PortMixer Library for Windows / Cygwin

CC = gcc

override CFLAGS = -g -I../px_common -I../../portaudio/pa_common
PASRC = px_win_wmme.c
PAINC = ../px_common/portmixer.h
PAOBJ = px_win_wmme.o 

portmixer.a: $(PAOBJ)
	ar ruv portmixer.a $(PAOBJ)
	ranlib portmixer.a

clean:
	rm -f portmixer.a *.o

%.o: %.c $(PAINC) Makefile
	$(CC) -c $(CFLAGS) $< -o $@
