#
# FILE: Makefile
#   BY: Christopher Lee Fraley
#   and Roger B. Dannenberg.
# DESC: This file builds various utilities for Nyquist
#

CC = cc $(CFLAGS)

# the unix path gets us switches.h:
# the cmt path gets us swlogic.h:
CFLAGS = -g -I../sys -I../cmt

# Directory info:
BINPATH = .

# Intgen stuff:
intgen: cmdline.o intgen.o
	$(CC) cmdline.o intgen.o -o $(BINPATH)/intgen

cmdline.o: cext.h cmdline.h
intgen.o: cext.h cmdline.h

#sampleprint - dumb but useful
sampleprint: sampleprint.o
	$(CC) sampleprint.o -o $(BINPATH)/sampleprint

sampleprint.o: sampleprint.c

#sine
sne: sne.o
	$(CC) sne.o -o $(BINPATH)/sne

sne.o: sne.c

#play
play: play.o
	$(CC) play.o -o $(BINPATH)/play

play.o: play.c

#plot
plot: plot.o
	$(CC) plot.o -o $(BINPATH)/plot

plot.o: plot.c

#unpacker
unpacker_o = unpacker.o convert.o

unpacker: $(unpacker_o)
	$(CC) $(unpacker_o) -o unpacker

#packer
packer_o = packer.o convert.o

packer: $(packer_o)
	$(CC) $(packer_o) -o packer

clean:
	rm -f *.o

cleaner: clean
	rm -f intgen play sine plot packer unpacker
	rm -f *.BAK
	rm -f *~

