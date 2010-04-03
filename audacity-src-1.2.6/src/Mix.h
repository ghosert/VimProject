/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni
  Markus Meyer

**********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

#include <wx/string.h>

#include "SampleFormat.h"
#include "WaveTrack.h"
#include "TimeTrack.h"
#include "ControlToolBar.h"
#include "Resample.h"

class ControlToolBar;
class DirManager;

bool QuickMix(TrackList * tracks, TrackFactory *factory,
              double rate, sampleFormat format,
              double startTime, double endTime,
              WaveTrack **newLeft, WaveTrack **newRight);

void MixBuffers(int numChannels, int *channelFlags, float *gains,
                sampleFormat format, samplePtr src,
                samplePtr *dests, int len, bool interleaved);

class Mixer {
 public:
   // 
   // Constructor / Destructor
   //

   Mixer(int numInputTracks, WaveTrack **inputTracks,
         TimeTrack *timeTrack,
         double startTime, double stopTime,
         int numOutChannels, int outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         bool highQuality = true);

   virtual ~ Mixer();

   //
   // Setup
   //

   void ApplyTrackGains(bool apply = true); // True by default

   //
   // Processing
   //

   /// Process a maximum of 'maxSamples' samples and put them into
   /// a buffer which can be retrieved by calling GetBuffer().
   /// Returns number of output samples, or 0, if there are no
   /// more samples that must be processed.
   sampleCount Process(sampleCount maxSamples);

   /// Restart processing at beginning of buffer next time
   /// Process() is called.
   void Restart();

   /// Current time in seconds
   double MixGetCurrentTime();

   /// Retrieve the main buffer or the interleaved buffer
   samplePtr GetBuffer();

   /// Retrieve one of the non-interleaved buffers
   samplePtr GetBuffer(int channel);

 private:

   void Clear();
   sampleCount MixSameRate(int *channelFlags, WaveTrack *src,
                           longSampleCount *pos);

   sampleCount MixVariableRates(int *channelFlags, WaveTrack *track,
                                longSampleCount *pos, float *queue,
                                int *queueStart, int *queueLen,
                                Resample *SRC);

 private:
   // Input
   int              mNumInputTracks;
   WaveTrack      **mInputTrack;
   TimeTrack       *mTimeTrack;
   longSampleCount *mSamplePos;
   bool             mApplyTrackGains;
   float           *mGains;
   double          *mEnvValues;
   double           mT;  // Current time
   double           mT0; // Start time
   double           mT1; // Stop time (none if mT0==mT1)   
   Resample       **mSRC;
   float          **mSampleQueue;
   int             *mQueueStart;
   int             *mQueueLen;
   int              mQueueMaxLen;
   int              mProcessLen;

   // Output
   int              mMaxOut;
   int              mNumChannels;
   int              mNumBuffers;
   int              mBufferSize;
   int              mInterleavedBufferSize;
   sampleFormat     mFormat;
   bool             mInterleaved;
   samplePtr       *mBuffer;
   samplePtr        mTemp;
   float           *mFloatBuffer;
   double           mRate;
};

#endif
