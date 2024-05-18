/*
 * $Id: patest_maxsines.c,v 1.2 2003/03/02 08:01:42 dmazzoni Exp $
 * patest_maxsines.c
 * How many sine waves can we calculate and play in less than 80% CPU Load.
 *
 * Authors:
 *    Ross Bencina <rossb@audiomulch.com>
 *    Phil Burk <philburk@softsynth.com>
 *
 * This program uses the PortAudio Portable Audio Library.
 * For more information see: http://www.audiomulch.com/portaudio/
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
#include <stdio.h>
#include <math.h>
#include "portaudio.h"

#define MAX_SINES     (500)
#define MAX_USAGE     (0.8)
#define SAMPLE_RATE   (44100)
#define FREQ_TO_PHASE_INC(freq)   (freq/(float)SAMPLE_RATE)

#define MIN_PHASE_INC  FREQ_TO_PHASE_INC(200.0f)
#define MAX_PHASE_INC  (MIN_PHASE_INC * (1 << 5))

#define FRAMES_PER_BUFFER  (512)
#ifndef M_PI
#define M_PI  (3.14159265)
#endif
#define TWOPI (M_PI * 2.0)

#define TABLE_SIZE   (512)

typedef struct paTestData
{
    int numSines;
    float sine[TABLE_SIZE + 1]; /* add one for guard point for interpolation */
    float phases[MAX_SINES];
}
paTestData;

/* Convert phase between and 1.0 to sine value
 * using linear interpolation.
 */
float LookupSine( paTestData *data, float phase );
float LookupSine( paTestData *data, float phase )
{
    float fIndex = phase*TABLE_SIZE;
    int   index = (int) fIndex;
    float fract = fIndex - index;
    float lo = data->sine[index];
    float hi = data->sine[index+1];
    float val = lo + fract*(hi-lo);
    return val;
}

/* This routine will be called by the PortAudio engine when audio is needed.
** It may called at interrupt level on some machines so don't do anything
** that could mess up the system like calling malloc() or free().
*/
static int patestCallback(   void *inputBuffer, void *outputBuffer,
                             unsigned long framesPerBuffer,
                             PaTimestamp outTime, void *userData )
{
    paTestData *data = (paTestData*)userData;
    float *out = (float*)outputBuffer;
    float outSample;
    float scaler;
    int numForScale;
    unsigned long i;
    int j;
    int finished = 0;
    (void) outTime; /* Prevent unused variable warnings. */
    (void) inputBuffer;

/* Detemine amplitude scaling factor */
    numForScale = data->numSines;
    if( numForScale < 8 ) numForScale = 8;  /* prevent pops at beginning */
    scaler = 1.0f / numForScale;
    
    for( i=0; i<framesPerBuffer; i++ )
    {
        float output = 0.0;
        float phaseInc = MIN_PHASE_INC;
        float phase;
        for( j=0; j<data->numSines; j++ )
        {
            /* Advance phase of next oscillator. */
            phase = data->phases[j];
            phase += phaseInc;
            if( phase >= 1.0 ) phase -= 1.0;

            output += LookupSine(data, phase); 
            data->phases[j] = phase;
            
            phaseInc *= 1.02f;
            if( phaseInc > MAX_PHASE_INC ) phaseInc = MIN_PHASE_INC;
        }

        outSample = (float) (output * scaler);
        *out++ = outSample; /* Left */
        *out++ = outSample; /* Right */
    }
    return finished;
}

/*******************************************************************/
int main(void);
int main(void)
{
	int i;
    PortAudioStream *stream;
    PaError err;
    paTestData data = {0};
    double load;
    printf("PortAudio Test: output sine wave. SR = %d, BufSize = %d\n", SAMPLE_RATE, FRAMES_PER_BUFFER);

    /* initialise sinusoidal wavetable */
    for( i=0; i<TABLE_SIZE; i++ )
    {
        data.sine[i] = (float) sin( ((double)i/(double)TABLE_SIZE) * M_PI * 2. );
    }
    data.sine[TABLE_SIZE] = data.sine[0]; /* set guard point */

    err = Pa_Initialize();
    if( err != paNoError ) goto error;
    err = Pa_OpenStream(
              &stream,
              paNoDevice,
              0,              /* no input */
              paFloat32,
              NULL,
              Pa_GetDefaultOutputDeviceID(), /* default output device */
              2,              /* stereo output */
              paFloat32,      /* 32 bit floating point output */
              NULL,
              SAMPLE_RATE,
              FRAMES_PER_BUFFER, 
              0,              /* number of buffers, if zero then use default minimum */
              paClipOff,      /* we won't output out of range samples so don't bother clipping them */
              patestCallback,
              &data );
    if( err != paNoError ) goto error;
    err = Pa_StartStream( stream );
    if( err != paNoError ) goto error;

/* Play an increasing number of sine waves until we hit MAX_USAGE */
    do
    {
        data.numSines++;
        Pa_Sleep( 200 );

        load = Pa_GetCPULoad( stream );
        printf("numSines = %d, CPU load = %f\n", data.numSines, load );
        fflush( stdout );
    }
    while( (load < MAX_USAGE) && (data.numSines < MAX_SINES) );

    err = Pa_StopStream( stream );
    if( err != paNoError ) goto error;
    err = Pa_CloseStream( stream );
    if( err != paNoError ) goto error;
    Pa_Terminate();
    printf("Test finished.\n");
    return err;
error:
    Pa_Terminate();
    fprintf( stderr, "An error occured while using the portaudio stream\n" );
    fprintf( stderr, "Error number: %d\n", err );
    fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
    return err;
}
