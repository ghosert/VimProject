#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "portmixer.h"
#include "portaudio.h"

/* part of getopt */
extern char *optarg;
extern int optind, opterr, optopt;

static int DummyCallbackFunc(void *inputBuffer, void *outputBuffer,
                             unsigned long framesPerBuffer,
                             PaTimestamp outTime, void *userData)
{
   return 0;
}

float getvolarg(const char *p)
{
   float f = atof(p);
   if (f < 0.0)
      return 0.0;
   else if (f > 1.0)
      return 1.0;
   else
      return f;
}

int main(int argc, char **argv)
{
   int num_mixers;
   int i;
   PaError error;
   PortAudioStream *stream;
   int recDeviceNum;
   int playDeviceNum;
   int inputChannels;
   int outputChannels;
   int num_devices;
   int device;
   int opt;
   int opts=-1, optm=0;
   float optv=-2, opto=-2, opti=-2, opth=-2, optb=-2;
   
   printf("px_test: a program to demonstrate the capabilities of PortMixer\n");
   printf("By Dominic Mazzoni\n");
   printf("\n");
   printf("Usage:\n");
   printf("  -d [device number]\n");
   printf("  -m [mixer number]\n");
   printf("  -v [vol] (Master volume)\n");
   printf("  -o [vol] (PCM output volume)\n");
   printf("  -i [vol] (Input volume)\n");
   printf("  -s [source number] (Input source)\n");
   printf("  -h [vol] (Playthrough)\n");
   printf("  -b [bal] (Balance: -1.0....1.0)\n");
   printf("\n");
   printf("All volumes are between 0.0 and 1.0.\n");
   printf("\n");

   error = Pa_Initialize();
   if (error != 0) {
      printf("PortAudio error: %s\n", Pa_GetErrorText(error));
      return -1;
   }

   num_devices = Pa_CountDevices();

   device = Pa_GetDefaultInputDeviceID();
   recDeviceNum = paNoDevice;
   playDeviceNum = paNoDevice;
   inputChannels = 0;
   outputChannels = 0;

   while(-1 != (opt=getopt(argc, argv, "d:m:v:o:i:s:h:b:"))) {
      switch(opt) {
      case 'd':
         device = atoi(optarg);
         printf("Set device to %d\n", device);
         break;
      case 'm':
         optm = atoi(optarg);
         printf("Set mixer number to %d\n", optm);
         break;
      case 'v':
         optv = getvolarg(optarg); break;
      case 'o':
         opto = getvolarg(optarg); break;
      case 'i':
         opti = getvolarg(optarg); break;
      case 'h':
         opth = getvolarg(optarg); break;
      case 'b':
         optb = atof(optarg); break;
      case 's':
         opts = atoi(optarg); break;
      }
   }

   printf("Devices:\n");
   for(i=0; i<num_devices; i++) {
      const PaDeviceInfo* deviceInfo = Pa_GetDeviceInfo(i);
      if (i==device) {
         printf("* ");
         if (deviceInfo->maxInputChannels > 0) {
            recDeviceNum = device;
            inputChannels = deviceInfo->maxInputChannels;
         }
         if (deviceInfo->maxOutputChannels > 0) {
            playDeviceNum = device;
            outputChannels = deviceInfo->maxOutputChannels;
         }
      }
      else
         printf("  ");
      printf("Device %d: %s in=%d out=%d",
             i, deviceInfo->name,
             deviceInfo->maxInputChannels, deviceInfo->maxOutputChannels);
      if (i == Pa_GetDefaultInputDeviceID())
         printf(" (default input)");
      if (i == Pa_GetDefaultOutputDeviceID())
         printf(" (default output)");
      printf("\n");
   }
   printf("\n");
   
   error = Pa_OpenStream(&stream, recDeviceNum, inputChannels, paFloat32, NULL,
                         playDeviceNum, outputChannels, paFloat32, NULL,
                         44101, 512, 1, paClipOff | paDitherOff,
                         DummyCallbackFunc, NULL);

   if (error) {
      printf("PortAudio error %d: %s\n", error,
             Pa_GetErrorText(error));
      return -1;
   }
   
   num_mixers = Px_GetNumMixers(stream);
   printf("Number of mixers for device %d: %d\n", device, num_mixers);
   for(i=0; i<num_mixers; i++) {
      PxMixer *mixer;
      int num;
      int j;

      printf("Mixer %d: %s\n", i, Px_GetMixerName(stream, i));
      mixer = Px_OpenMixer(stream, i);
      if (!mixer) {
         printf("  Could not open mixer!\n");
         continue;
      }

      if (i == optm) {
         if (optv!=-2) {
            Px_SetMasterVolume(mixer, optv);
            printf("  Set master volume\n");
         }
         if (opto!=-2) {
            Px_SetPCMOutputVolume(mixer, opto);
            printf("  Set output volume\n");
         }
         if (opti!=-2) {
            Px_SetInputVolume(mixer, opti);
            printf("  Set input volume\n");
         }
         if (opth!=-2) {
            Px_SetPlaythrough(mixer, opth);
            printf("  Set playthrough volume\n");
         }
         if (opts!=-2) {
            Px_SetCurrentInputSource(mixer, opts);
            printf("  Set input source\n");
         }
         if (optb!=-2) {
            Px_SetOutputBalance(mixer, optb);
            printf("  Set balance\n");
         }
      }
      
      printf("  Master volume: %.2f\n", Px_GetMasterVolume(mixer));
      printf("  PCM output volume: %.2f\n", Px_GetPCMOutputVolume(mixer));

      num = Px_GetNumOutputVolumes(mixer);
      printf("  Num outputs: %d\n", num);
      for(j=0; j<num; j++) {
         printf("    Output %d (%s): %.2f\n",
                j,
                Px_GetOutputVolumeName(mixer, j),
                Px_GetOutputVolume(mixer, j));
      }

      num = Px_GetNumInputSources(mixer);
      printf("  Num input sources: %d\n", num);
      for(j=0; j<num; j++) {
         printf("    Input %d (%s) %s\n",
                j,
                Px_GetInputSourceName(mixer, j),
                (Px_GetCurrentInputSource(mixer)==j?
                 "SELECTED": ""));
      }
      printf("  Input volume: %.2f\n", Px_GetInputVolume(mixer));

      printf("  Playthrough:");
      if (Px_SupportsPlaythrough(mixer))
         printf(" %.2f\n", Px_GetPlaythrough(mixer));
      else
         printf(" not supported.\n");

      printf("  Output balance:");
      if (Px_SupportsOutputBalance(mixer))
         printf(" %.2f\n", Px_GetOutputBalance(mixer));
      else
         printf(" not supported.\n");

      Px_CloseMixer(mixer);
   }

   Pa_CloseStream(stream);

   Pa_Terminate();

   return 0;
}
