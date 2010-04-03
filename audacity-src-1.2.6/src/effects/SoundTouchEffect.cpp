/**********************************************************************

  Audacity: A Digital Audio Editor

  SoundTouchEffect.cpp

  Dominic Mazzoni, Vaughan Johnson

  This abstract class contains all of the common code for an
  effect that uses SoundTouch to do its processing (ChangeTempo
  and ChangePitch).

**********************************************************************/

#include "../Audacity.h"

#if USE_SOUNDTOUCH

#include <math.h>

#include "SoundTouchEffect.h"
#include "../WaveTrack.h"


bool EffectSoundTouch::Process()
{
   // Assumes that mSoundTouch has already been initialized
   // by the subclass for subclass-specific parameters.

   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack* leftTrack = (WaveTrack*)(iter.First());
   WaveTrack* rightTrack = NULL;
   mCurTrackNum = 0;
	m_maxNewLength = 0.0;
   while (leftTrack) {
      //Get start and end times from track
      double trackStart = leftTrack->GetStartTime();
      double trackEnd = leftTrack->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {

         //Transform the marker timepoints to samples
         longSampleCount start = leftTrack->TimeToLongSamples(mCurT0);
         longSampleCount end = leftTrack->TimeToLongSamples(mCurT1);
         
         rightTrack = NULL; 
         if (leftTrack->GetLinked()) {
            rightTrack = (WaveTrack*)(iter.Next());
            mSoundTouch->setChannels(2);
            if (!ProcessStereo(leftTrack, rightTrack, start, end))
               return false;
            mCurTrackNum++; // Increment for rightTrack, too.
         } else {
            mSoundTouch->setChannels(1);
            //ProcessOne() (implemented below) processes a single track
            if (!ProcessOne(leftTrack, start, end))
               return false;
         }
      }
      
      //Iterate to the next track
      leftTrack = (WaveTrack*)(iter.Next());
      mCurTrackNum++;
   }

   delete mSoundTouch;
   mSoundTouch = NULL;

	mT1 = mT0 + m_maxNewLength; // Update selection.
   return true;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes ProcessSoundTouch on these blocks
bool EffectSoundTouch::ProcessOne(WaveTrack *track,
                                  longSampleCount start, longSampleCount end)
{
   WaveTrack *outputTrack;
   longSampleCount s;

   mSoundTouch->setSampleRate((unsigned int)(track->GetRate()+0.5));
   
   outputTrack = mFactory->NewWaveTrack(track->GetSampleFormat());

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

      //Add samples to SoundTouch
      mSoundTouch->putSamples(buffer, block);

      //Get back samples from SoundTouch
      unsigned int outputCount = mSoundTouch->numSamples();
      if (outputCount > 0) {
         float *buffer2 = new float[outputCount];
         mSoundTouch->receiveSamples(buffer2, outputCount);
         outputTrack->Append((samplePtr)buffer2, floatSample, outputCount);
         delete[] buffer2;
      }

      //Increment s one blockfull of samples
      s += block;

      //Update the Progress meter
      if (TrackProgress(mCurTrackNum, (s - start) / len))
         return false;
   }

   // Tell SoundTouch to finish processing any remaining samples
   mSoundTouch->flush();

   unsigned int outputCount = mSoundTouch->numSamples();
   if (outputCount > 0) {
      float *buffer2 = new float[outputCount];
      mSoundTouch->receiveSamples(buffer2, outputCount);
      outputTrack->Append((samplePtr)buffer2, floatSample, outputCount);
      delete[] buffer2;
   }

   // Flush the output WaveTrack (since it's buffered, too)
   outputTrack->Flush();

   // Clean up the buffer
   delete[]buffer;

   // Take the output track and insert it in place of the original
   // sample data

   track->Clear(mT0, mT1);
   track->Paste(mT0, outputTrack);

	double newLength = outputTrack->GetEndTime(); 
	if (newLength > m_maxNewLength) 
		m_maxNewLength = newLength; 

   // Delete the outputTrack now that its data is inserted in place
   delete outputTrack;

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectSoundTouch::ProcessStereo(WaveTrack* leftTrack, WaveTrack* rightTrack, 
                                       longSampleCount start, longSampleCount end)
{
   mSoundTouch->setSampleRate((unsigned int)(leftTrack->GetRate()+0.5));
   
   WaveTrack* outputLeftTrack = mFactory->NewWaveTrack(leftTrack->GetSampleFormat());
   WaveTrack* outputRightTrack = mFactory->NewWaveTrack(rightTrack->GetSampleFormat());

   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later 
   double len = (double)(end - start);

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   // Make soundTouchBuffer twice as big as MaxBlockSize for each channel, 
   // because Soundtouch wants them interleaved, i.e., each 
   // Soundtouch sample is left-right pair. 
   sampleCount maxBlockSize = leftTrack->GetMaxBlockSize();
   float* leftBuffer = new float[maxBlockSize];
   float* rightBuffer = new float[maxBlockSize];
   float* soundTouchBuffer = new float[maxBlockSize * 2];

   // Go through the track one stereo buffer at a time. 
   // sourceSampleCount counts the sample at which the current buffer starts, 
   // per channel.
   longSampleCount sourceSampleCount = start;
   while (sourceSampleCount < end) {
      //Get a block of samples (smaller than the size of the buffer)
      sampleCount blockSize = leftTrack->GetBestBlockSize(sourceSampleCount);

      //Adjust the block size if it is the final block in the track
      if (sourceSampleCount + blockSize > end)
         blockSize = end - sourceSampleCount;

      // Get the samples from the tracks and put them in the buffers.
      leftTrack->Get((samplePtr)(leftBuffer), floatSample, sourceSampleCount, blockSize);
      rightTrack->Get((samplePtr)(rightBuffer), floatSample, sourceSampleCount, blockSize);

      // Interleave into soundTouchBuffer.
      for (int index = 0; index < blockSize; index++) {
         soundTouchBuffer[index*2]       = leftBuffer[index];
         soundTouchBuffer[(index*2)+1]   = rightBuffer[index];
      }

      //Add samples to SoundTouch
      mSoundTouch->putSamples(soundTouchBuffer, blockSize);

      //Get back samples from SoundTouch
      unsigned int outputCount = mSoundTouch->numSamples();
      if (outputCount > 0) 
         this->ProcessStereoResults(outputCount, outputLeftTrack, outputRightTrack);

      //Increment sourceSampleCount one blockfull of samples
      sourceSampleCount += blockSize;

      //Update the Progress meter
      if (TrackProgress(mCurTrackNum, (sourceSampleCount - start) / len))
         return false;
   }

   // Tell SoundTouch to finish processing any remaining samples
   mSoundTouch->flush();

   unsigned int outputCount = mSoundTouch->numSamples();
   if (outputCount > 0) 
      this->ProcessStereoResults(outputCount, outputLeftTrack, outputRightTrack);

   // Flush the output WaveTracks (since they're buffered, too)
   outputLeftTrack->Flush();
   outputRightTrack->Flush();

   // Clean up the buffers.
   delete [] leftBuffer;
   delete [] rightBuffer;
   delete [] soundTouchBuffer;

   // Take the output tracks and insert in place of the original
   // sample data.
   leftTrack->Clear(mT0, mT1);
   leftTrack->Paste(mT0, outputLeftTrack);
   rightTrack->Clear(mT0, mT1);
   rightTrack->Paste(mT0, outputRightTrack);

	double newLength = outputLeftTrack->GetEndTime(); 
	if (newLength > m_maxNewLength) 
		m_maxNewLength = newLength; 

   // Delete the outputTracks now that their data are inserted in place.
   delete outputLeftTrack;
   delete outputRightTrack;

   //Return true because the effect processing succeeded.
   return true;
}

bool EffectSoundTouch::ProcessStereoResults(const unsigned int outputCount, 
                                            WaveTrack* outputLeftTrack, 
                                            WaveTrack* outputRightTrack)
{
   float* outputSoundTouchBuffer = new float[outputCount*2];
   mSoundTouch->receiveSamples(outputSoundTouchBuffer, outputCount);

   // Dis-interleave outputSoundTouchBuffer into separate track buffers.
   float* outputLeftBuffer = new float[outputCount];
   float* outputRightBuffer = new float[outputCount];
   for (int index = 0; index < outputCount; index++)
   {
      outputLeftBuffer[index] = outputSoundTouchBuffer[index*2];
      outputRightBuffer[index] = outputSoundTouchBuffer[(index*2)+1];
   }

   outputLeftTrack->Append((samplePtr)outputLeftBuffer, floatSample, outputCount);
   outputRightTrack->Append((samplePtr)outputRightBuffer, floatSample, outputCount);

   delete[] outputSoundTouchBuffer;
   delete[] outputLeftBuffer;
   delete[] outputRightBuffer;

   return true;
}

#endif // USE_SOUNDTOUCH

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 43212604-7b03-4b49-85bd-adde0f909460

