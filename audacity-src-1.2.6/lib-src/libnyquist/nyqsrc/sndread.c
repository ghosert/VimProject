/* sndread.c -- read sound files */

/* CHANGELOG
 *
 * 29Jun95  RBD  ULAW fixed problems with signed chars
 * 28Apr03  dm   explicitly declare sndread_file_open_count as int
 */

#include "switches.h"
#include "stdio.h"
#include "string.h"
#ifdef UNIX
#include "sys/file.h"
#else
/* #include <unistd.h> */
#ifdef WINDOWS
#include <sys/stat.h>
#include "io.h"
#else
#include <stat.h>
#endif
#define L_SET SEEK_SET
#define L_INCR SEEK_CUR
#define PROTECTION 
#endif
#ifndef mips
#include "stdlib.h"
#endif
#include "snd.h"
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "sndread.h"
#include "multiread.h"

/* file.h doesn't define O_RDONLY under RS6K AIX */
#ifndef O_RDONLY
#define O_RDONLY 0
#endif

static int sndread_file_open_count = 0;

void read__fetch(susp, snd_list)
  register read_susp_type susp;
  snd_list_type snd_list;
{
    int n;
    sample_block_type out;
    register sample_block_values_type out_ptr;
    /* allow up to 4 bytes/sample: */
    char input_buffer[max_sample_block_len * 4];
    int in_count;
    float peak;

    falloc_sample_block(out, "read__fetch");
    out_ptr = out->samples;
    snd_list->block = out;

    in_count = snd_read(&susp->snd, input_buffer, max_sample_block_len);

    n = in_count;

    /* don't read too many */
    if (n > (susp->cnt - susp->susp.current)) {
        n = susp->cnt - susp->susp.current;
    }

    /* NOTE: this could be optimized for cvt_from_float_32 by reading
     * raw bytes from the file directly into out_ptr, but 32-bit files
     * do not seem important to optimize for.
     */
    (*susp->cvtfn)((void *) out_ptr, (void *) input_buffer, 
             n, 1.0F, &peak);

    snd_list->block_len = n;
    susp->susp.current += n;

    if (n == 0) {
        /* we didn't read anything, but can't return length zero, so
           convert snd_list to pointer to zero block */
        snd_list_terminate(snd_list);
    } else if (n < max_sample_block_len) {
        /* this should close file and free susp */
        snd_list_unref(snd_list->u.next);
        /* if something is in buffer, terminate by pointing to zero block */
        snd_list->u.next = zero_snd_list;
    }
} /* read__fetch */


void read_free(read_susp_type susp)
{
    (void) snd_close(&susp->snd);
    sndread_file_open_count--;
    ffree_generic(susp, sizeof(read_susp_node), "read_free");
}


void read_print_tree(read_susp_type susp, int n)
{
}


LVAL snd_make_read(
  unsigned char *filename, 	/* file to read */
  time_type offset, 	/* offset to skip (in seconds) */
  time_type t0,		/* start time of resulting sound */
  long *format,		/* AIFF, IRCAM, NeXT, etc. */
  long *channels,	/* number of channels */
  long *mode, 		/* sample format: PCM, ALAW, etc. */
  long *bits,		/* BPS: bits per sample */
  long *swap,           /* swap bytes? */
  double *srate,	/* srate: sample rate */
  double *dur,		/* duration (in seconds) to read */
  long *flags,		/* which parameters have been set */
  long *byte_offset)	/* byte offset in file of first sample */
{
    register read_susp_type susp;
    /* srate specified as input parameter */
    sample_type scale_factor = 1.0F;
    long bytes_per_frame;
    long bytes_of_data;

    falloc_generic(susp, read_susp_node, "snd_make_read");

    susp->snd.device = SND_DEVICE_FILE;
    susp->snd.write_flag = SND_READ;
    strcpy(susp->snd.u.file.filename, (char *) filename);
    susp->snd.u.file.header = *format;
    susp->snd.u.file.end_offset = 1000000000; /* 1 gig, in case open doesn't set it */
    susp->snd.format.channels = *channels;
    susp->snd.format.mode = *mode;
    susp->snd.format.bits = *bits;
    susp->snd.u.file.swap = *swap;
    susp->snd.format.srate = *srate;

    if (SND_SUCCESS != snd_open(&susp->snd, flags)) {
        char error[240];
        sprintf(error, "SND-READ: Cannot open file '%s'", filename);
        xlfail(error);
    }
    if (susp->snd.format.channels < 1) {
        snd_close(&susp->snd);
        xlfail("Must specify 1 or more channels");
    }

    if (offset < 0.0) xlfail("Negative offset");
    else if (offset > 0.0) {
        if (snd_seek(&susp->snd, offset) != SND_SUCCESS) {
            snd_close(&susp->snd);
            return NIL;
        }
    }

    /* see if file is shorter than requested duration and adjust */
    bytes_of_data = susp->snd.u.file.end_offset - susp->snd.u.file.byte_offset;
    bytes_per_frame = snd_bytes_per_frame(&susp->snd);
    if ((*flags & SND_HEAD_LEN) &&
        (((long) (*dur * susp->snd.format.srate + 0.5)) * bytes_per_frame >
         bytes_of_data)) {
        *dur = (bytes_of_data / bytes_per_frame) / susp->snd.format.srate;
    }

    /* initialize susp state */
    susp->susp.sr = susp->snd.format.srate;
    susp->susp.t0 = t0;
    susp->bytes_per_sample = (susp->snd.format.bits + 7) >> 3;
    susp->susp.mark = NULL;
    susp->susp.print_tree = read_print_tree;
    susp->susp.current = 0;
    susp->cnt = (long) ((*dur * susp->snd.format.srate) + 0.5);

    /* convert_from functions */
    if (susp->snd.format.bits == 8)
        susp->cvtfn = cvt_from_8[susp->snd.format.mode];
    else if (susp->snd.format.bits == 16)
        susp->cvtfn = cvt_from_16[susp->snd.format.mode];
    else if (susp->snd.format.bits == 32)
        susp->cvtfn = cvt_from_32[susp->snd.format.mode];
    else susp->cvtfn = cvt_from_unknown;

    if (susp->cvtfn == cvt_from_unknown) {
        snd_close(&susp->snd);
        return NIL;
    }

    *format = susp->snd.u.file.header;
    *channels = susp->snd.format.channels;
    *mode = susp->snd.format.mode;
    *bits = susp->snd.format.bits;
    *swap = susp->snd.u.file.swap;
    *srate = susp->snd.format.srate;
    *byte_offset = susp->snd.u.file.byte_offset;

    sndread_file_open_count++;
#ifdef MACINTOSH
    if (sndread_file_open_count > 24) {
        nyquist_printf("Warning: more than 24 sound files are now open\n");
    }
#endif
    
    if (susp->snd.format.channels == 1) {
        susp->susp.fetch = read__fetch;
        susp->susp.free = read_free;
        susp->susp.name = "read";
        return cvsound(sound_create((snd_susp_type)susp, t0, 
                                    susp->snd.format.srate, 
                                    scale_factor));
    } else {
        susp->susp.fetch = multiread_fetch;
        susp->susp.free = multiread_free;
        susp->susp.name = "multiread";
        return multiread_create(susp);
    }
}




