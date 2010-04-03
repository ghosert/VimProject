all: pa_test

CFLAGS = -g -Wall -O -I../include

pa_test:
	gcc $(CFLAGS) -c debug_dual.c
	gcc -g -o debug_dual debug_dual.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c debug_multi_in.c
	gcc -g -o debug_multi_in debug_multi_in.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c debug_multi_out.c
	gcc -g -o debug_multi_out debug_multi_out.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c debug_record.c
	gcc -g -o debug_record debug_record.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c debug_sine.c
	gcc -g -o debug_sine debug_sine.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c debug_test1.c
	gcc -g -o debug_test1 debug_test1.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c paqa_devs.c
	gcc -g -o paqa_devs paqa_devs.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c paqa_errs.c
	gcc -g -o paqa_errs paqa_errs.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest1.c
	gcc -g -o patest1 patest1.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_clip.c
	gcc -g -o patest_clip patest_clip.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_dither.c
	gcc -g -o patest_dither patest_dither.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_latency.c
	gcc -g -o patest_latency patest_latency.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_leftright.c
	gcc -g -o patest_leftright patest_leftright.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_longsine.c
	gcc -g -o patest_longsine patest_longsine.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_many.c
	gcc -g -o patest_many patest_many.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_maxsines.c
	gcc -g -o patest_maxsines patest_maxsines.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_pink.c
	gcc -g -o patest_pink patest_pink.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_record.c
	gcc -g -o patest_record patest_record.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_ringmix.c
	gcc -g -o patest_ringmix patest_ringmix.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_saw.c
	gcc -g -o patest_saw patest_saw.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_sine.c
	gcc -g -o patest_sine patest_sine.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_sine8.c
	gcc -g -o patest_sine8 patest_sine8.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_sine_time.c
	gcc -g -o patest_sine_time patest_sine_time.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_stop.c
	gcc -g -o patest_stop patest_stop.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_sync.c
	gcc -g -o patest_sync patest_sync.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c patest_wire.c
	gcc -g -o patest_wire patest_wire.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c pa_devs.c
	gcc -g -o pa_devs pa_devs.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c pa_fuzz.c
	gcc -g -o pa_fuzz pa_fuzz.o -L../lib -lportaudio -lportmixer -lwinmm
	gcc $(CFLAGS) -c pa_minlat.c
	gcc -g -o pa_minlat pa_minlat.o -L../lib -lportaudio -lportmixer -lwinmm

