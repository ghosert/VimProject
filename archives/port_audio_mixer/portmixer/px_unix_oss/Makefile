# Make PortMixer Library for Unix / OSS

CC = gcc

override CFLAGS += -g -I../px_common -I../../portaudio/pa_common
PASRC = px_unix_oss.c
PAINC = ../px_common/portmixer.h
PAOBJ = px_unix_oss.o 

portmixer.a: $(PAOBJ)
	ar ruv portmixer.a $(PAOBJ)
	ranlib portmixer.a

clean:
	rm -f portmixer.a *.o

%.o: %.c $(PAINC) Makefile
	$(CC) -c $(CFLAGS) $< -o $@
