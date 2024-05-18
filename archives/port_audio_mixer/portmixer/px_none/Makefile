# Make PortMixer Library for Mac OS X / CoreMixer

CC = cc

CFLAGS = -g -I../px_common -I../../portaudio/pa_common
PASRC = px_none.c
PAINC = ../px_common/portmixer.h
PAOBJ = px_none.o 

portmixer.a: $(PAOBJ)
	ar ruv portmixer.a $(PAOBJ)
	ranlib portmixer.a

clean:
	rm -f portmixer.a *.o

%.o: %.c $(PAINC) Makefile
	$(CC) -c $(CFLAGS) $< -o $@
