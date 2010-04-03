/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/string.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>

#include "Effect.h"
#include "../AudioIO.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../WaveTrack.h"

//
// public static methods
//

EffectArray Effect::mEffects;
int Effect::sNumEffects = 0;
double Effect::sDefaultGenerateLen = 30.0;

// Some static variables used for Repeat Last Effect.
int Effect::LastType=0;
int Effect::LastIndex=0;
Effect * Effect::pLastEffect=NULL;

void Effect::RegisterEffect(Effect *f)
{
   f->mID = sNumEffects;
   sNumEffects++;

   // Insert the effect into the list in alphabetical order
   // A linear search is good enough as long as there are
   // only a few dozen or even a few hundred effects.
   wxString name = f->GetEffectName();
   int len = mEffects.GetCount();
   int i;
   for(i=0; i<len; i++)
      if (name.CmpNoCase(mEffects[i]->GetEffectName()) < 0) {
         mEffects.Insert(f, i);
         break;
      }
   if (i==len)
      mEffects.Add(f);
}

void Effect::UnregisterEffects()
{
   for(int i=0; i<sNumEffects; i++)
      delete mEffects[i];

   mEffects.Clear();
}

int Effect::GetNumEffects()
{
   return sNumEffects;
}

Effect *Effect::GetEffect(int ID)
{
   for(int i=0; i<sNumEffects; i++)
      if (mEffects[i]->mID == ID)
         return mEffects[i];
   
   return NULL;
}

EffectArray *Effect::GetEffects(int flags /* = ALL_EFFECTS */)
{
   EffectArray *results = new EffectArray();

   int len = mEffects.GetCount();
   for(int i=0; i<len; i++) {
      int g = mEffects[i]->GetEffectFlags();
      if ((flags & g) == g)
         results->Add(mEffects[i]);
   }

   return results;
}

//
// public methods
//

Effect::Effect()
{
   mWaveTracks = NULL;
   mProgress = NULL;
}

bool Effect::DoEffect(wxWindow *parent, int flags, TrackList *list,
                      TrackFactory *factory,
                      double *t0, double *t1)
{
   wxASSERT(*t0 <= *t1);

   if (mWaveTracks) {
      delete mWaveTracks;
      mWaveTracks = NULL;
   }

   mFactory = factory;
   mParent = parent;
   mTracks = list;
   mT0 = *t0;
   mT1 = *t1;
   CountWaveTracks();

   if (!Init())
      return false;

   // Don't prompt user if we are dealing with a 
   // effect that is already configured, e.g. repeating
   // the last effect on a different selection.
   if( (flags & CONFIGURED_EFFECT) == 0)
   {
      if (!PromptUser())
         return false;
   }
      
   wxBusyCursor busy;
   wxYield();
   wxStartTimer();

   bool returnVal = Process();

   End();
   
   if (mProgress) {
      delete mProgress;
      mProgress = NULL;
   }
   
   delete mWaveTracks;
   mWaveTracks = NULL;

   if (returnVal) {
      *t0 = mT0;
      *t1 = mT1;
   }
   
   return returnVal;
}

bool Effect::TotalProgress(double frac)
{
   if (!mProgress && wxGetElapsedTime(false) > 500) {
      mProgress =
         new wxProgressDialog(GetEffectName(),
                              GetEffectAction(),
                              1000,
                              mParent,
                              wxPD_CAN_ABORT |
                              wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
   }
   
   bool cancelling = false;

   if (mProgress) {
      cancelling =
         !mProgress->Update((int)(frac*1000 + 0.5));
   }
   
   return cancelling;
}

bool Effect::TrackProgress(int whichTrack, double frac)
{
   return TotalProgress((whichTrack+frac)/mNumTracks);
}

bool Effect::TrackGroupProgress(int whichGroup, double frac)
{
   return TotalProgress((whichGroup+frac)/mNumGroups);
}
void Effect::CountWaveTracks()
{
   mNumTracks = 0;
   mNumGroups = 0;
   mWaveTracks = new TrackList();
   
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   
   while(t) {
      if (!t->GetSelected()) {
         t = iter.Next();
         continue;
      }
      
      if (t->GetKind() == Track::Wave) {
         mWaveTracks->Add(t);
         mNumTracks++;
         if (!t->GetLinked())
            mNumGroups++;
      }
      t = iter.Next();
   }
}

float TrapFloat(float x, float min, float max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

double TrapDouble(double x, double min, double max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

long TrapLong(long x, long min, long max)
{
   if (x <= min)
      return min;
   else if (x >= max)
      return max;
   else
      return x;
}

wxString Effect::GetPreviewName()
{
   return _("Preview");
}

void Effect::Preview()
{
   if (gAudioIO->IsBusy())
      return;

   // Mix the first 3 seconds of audio from all of the tracks
   double previewLen = 3.0;

   WaveTrack *mixLeft = NULL;
   WaveTrack *mixRight = NULL;
   double rate = gPrefs->Read("/SamplingRate/DefaultProjectSampleRate",
                              AudioIO::GetOptimalSupportedSampleRate());
   double t0 = mT0;
   double t1 = t0 + previewLen;

   if (t1 > mT1)
      t1 = mT1;

   if (t1 <= t0)
      return;

   if (!::QuickMix(mWaveTracks, mFactory, rate, floatSample, t0, t1,
                   &mixLeft, &mixRight))
      return;

   // Apply effect

   TrackList *saveWaveTracks = mWaveTracks;
   mWaveTracks = new TrackList();
   mWaveTracks->Add(mixLeft);
   if (mixRight)
      mWaveTracks->Add(mixRight);

   t0 = 0.0;
   t1 = mixLeft->GetEndTime();

   double t0save = mT0;
   double t1save = mT1;
   mT0 = t0;
   mT1 = t1;

   // Effect is already inited; we call Process, End, and then Init
   // again, so the state is exactly the way it was before Preview
   // was called.
   Process();
   End();   
   if (mProgress) {
      delete mProgress;
      mProgress = NULL;
   }
   Init();

   mT0 = t0save;
   mT1 = t1save;

   WaveTrackArray playbackTracks;
   WaveTrackArray recordingTracks;
   playbackTracks.Add(mixLeft);
   if (mixRight)
      playbackTracks.Add(mixRight);

   // Start audio playing
   
   int token =
      gAudioIO->StartStream(playbackTracks, recordingTracks, NULL,
                            rate, t0, t1);
   if (token) {
      wxBusyCursor busy;
      ::wxUsleep((int)(previewLen*1000));

      while(gAudioIO->IsStreamActive(token)) {
         ::wxUsleep(100);
      }
      gAudioIO->StopStream();

      while(gAudioIO->IsBusy()) {
         ::wxUsleep(100);
      }
   }
   else {
      wxMessageBox(_("Error while opening sound device. Please check the output "
                     "device settings and the project sample rate."),
                   _("Error"), wxOK | wxICON_EXCLAMATION, mParent);
   }

   delete mWaveTracks;
   delete mixLeft;
   if (mixRight)
      delete mixRight;

   mWaveTracks = saveWaveTracks;
}

