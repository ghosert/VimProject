# Make PortMixer Library for Mac OS X / CoreAudio

CC = cc

CFLAGS = -g -I../px_common -I../../portaudio/pa_common
PASRC = px_mac_core.c
PAINC = ../px_common/portmixer.h
PAOBJ = px_mac_core.o 

portmixer.a: $(PAOBJ)
	ar ruv portmixer.a $(PAOBJ)
	ranlib portmixer.a

clean:
	rm -f portmixer.a *.o

%.o: %.c $(PAINC) Makefile
	$(CC) -c $(CFLAGS) $< -o $@
