/**********************************************************************

  Audacity: A Digital Audio Editor

  TwoPassSimpleMono.h

  Dominic Mazzoni

  This bit by Martyn Shaw.
  This class implements a two pass process by using EffectSimpleMono.
  Inherit from it if your effect needs to pass twice over the data.
  It does the first pass on all selected tracks before going back and
  doing the second pass over all selected tracks.

**********************************************************************/
#ifndef __AUDACITY_EFFECT_TWOPASSSIMPLEMONO__
#define __AUDACITY_EFFECT_TWOPASSSIMPLEMONO__

#include "SimpleMono.h"

class WaveTrack;

class EffectTwoPassSimpleMono:public Effect {

 public:
   virtual bool Process();

 private:
   bool ProcessOne(WaveTrack * t,
                   longSampleCount start, longSampleCount end);
   bool ProcessPass();

 protected:  

   // Override these methods if you need to initialize something
   // before each pass. Return None if processing should stop.
   virtual bool InitPass1();
   virtual bool InitPass2();
   
   // Override these methods if you need to do things
   // before every track (including the first one)
   virtual bool NewTrackPass1();
   virtual bool NewTrackPass2();

   // Override this method to actually process audio
   virtual bool ProcessPass1(float *buffer, sampleCount len) = 0;
   virtual bool ProcessPass2(float *buffer, sampleCount len) = 0;
   
   // Call this if you know in advance that no second pass will be needed.
   // This is used as a hint for the progress bar
   void DisableSecondPass() { mSecondPassDisabled = true; }

   // Other useful information
   int    mCurTrackNum;
   double mCurRate;
   double mCurT0;
   double mCurT1;
   int    mCurChannel;
   int    mPass;
   bool   mSecondPassDisabled;

};

#endif
