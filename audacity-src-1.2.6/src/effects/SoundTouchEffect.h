/**********************************************************************

  Audacity: A Digital Audio Editor

  SoundTouchEffect.h

  Dominic Mazzoni, Vaughan Johnson

  This abstract class contains all of the common code for an
  effect that uses SoundTouch to do its processing (ChangeTempo
  and ChangePitch).

**********************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_SOUNDTOUCH__
#define __AUDACITY_EFFECT_SOUNDTOUCH__

#include "Effect.h"
#include <soundtouch/SoundTouch.h>
// this is the standard system path, then we use -I cflags to make
// it work if we have local soundtouch

using namespace soundtouch;


class WaveTrack;

class EffectSoundTouch:public Effect {

 public:
   virtual bool Process();

 protected:
   SoundTouch *mSoundTouch;

 private:
   bool ProcessOne(WaveTrack * t,
                   longSampleCount start, longSampleCount end);
   bool ProcessStereo(WaveTrack* leftTrack, WaveTrack* rightTrack, 
                        longSampleCount start, longSampleCount end);
   bool ProcessStereoResults(const unsigned int outputCount, 
                              WaveTrack* outputLeftTrack, 
                              WaveTrack* outputRightTrack);

   int    mCurTrackNum;
   double mCurT0;
   double mCurT1;

	double m_maxNewLength;
};

#endif

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 6f08259e-6cac-4c47-9aa5-1de7b68eb495

