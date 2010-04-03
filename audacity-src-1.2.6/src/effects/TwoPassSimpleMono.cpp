/**********************************************************************

  Audacity: A Digital Audio Editor

  TwoPassSimpleMono.cpp

  Dominic Mazzoni

  This bit by Martyn Shaw.
  This class implements a two pass process by using EffectSimpleMono.
  Inherit from it if your effect needs to pass twice over the data.
  It does the first pass on all selected tracks before going back and
  doing the second pass over all selected tracks.

**********************************************************************/
#include "TwoPassSimpleMono.h"

bool EffectTwoPassSimpleMono::Process()
{
    mPass = 0;
    mSecondPassDisabled = false;
    
    InitPass1();
    if (!ProcessPass())
        return false;
        
    if (!mSecondPassDisabled)
    {
        mPass = 1;
    	if(InitPass2()){
    		return ProcessPass();
    	}
    }
    
    return true;
}

bool EffectTwoPassSimpleMono::ProcessPass()
{
   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   mCurTrackNum = 0;
   while (track) {
      //Get start and end times from track
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {

         //Transform the marker timepoints to samples
         longSampleCount start = track->TimeToLongSamples(mCurT0);
         longSampleCount end = track->TimeToLongSamples(mCurT1);
         
         //Get the track rate and samples
         mCurRate = track->GetRate();
         mCurChannel = track->GetChannel();

         //NewTrackPass1/2() returns true by default
         bool ret;
         if (mPass == 0)
            ret = NewTrackPass1();
         else
            ret = NewTrackPass2();
         if (!ret)
            return false;

         //ProcessOne() (implemented below) processes a single track
         if (!ProcessOne(track, start, end))
            return false;
      }
      
      //Iterate to the next track
      track = (WaveTrack *) iter.Next();
      mCurTrackNum++;
   }

   return true;
}


//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessSimpleMono on these blocks
bool EffectTwoPassSimpleMono::ProcessOne(WaveTrack * track,
                                  longSampleCount start, longSampleCount end)
{
   bool ret;
   longSampleCount s;
   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later 
   double len = (double)(end - start);

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   float *buffer = new float[track->GetMaxBlockSize()];

   //Go through the track one buffer at a time. s counts which
   //sample the current buffer starts at.
   s = start;
   while (s < end) {
      //Get a block of samples (smaller than the size of the buffer)
      sampleCount block = track->GetBestBlockSize(s);

      //Adjust the block size if it is the final block in the track
      if (s + block > end)
         block = end - s;

      //Get the samples from the track and put them in the buffer
      track->Get((samplePtr) buffer, floatSample, s, block);

      //Process the buffer.  If it fails, clean up and exit.
      if (mPass == 0)
         ret = ProcessPass1(buffer, block);
      else
         ret = ProcessPass2(buffer, block);
      if (!ret) {
         delete[]buffer;

         //Return false because the effect failed.
         return false;
      }

      //Processing succeeded. copy the newly-changed samples back 
      //onto the track.
      track->Set((samplePtr) buffer, floatSample, s, block);

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (mSecondPassDisabled)
         ret = TotalProgress((mCurTrackNum + (s-start)/len) / GetNumWaveTracks());
      else  
         ret = TotalProgress((mCurTrackNum + (s-start)/len + GetNumWaveTracks()*mPass)/ (GetNumWaveTracks()*2));
      if (ret)
         return false;
   }

   //Clean up the buffer
   delete[]buffer;

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectTwoPassSimpleMono::NewTrackPass1()
{
   return true;
}

bool EffectTwoPassSimpleMono::NewTrackPass2()
{
   return true;
}

//Initialisations before the first pass
bool EffectTwoPassSimpleMono::InitPass1()
{
	return true;
}

//Initialisations before the second pass.
//Return true if you actually want the second pass to go ahead
bool EffectTwoPassSimpleMono::InitPass2()
{
	return true;
}

