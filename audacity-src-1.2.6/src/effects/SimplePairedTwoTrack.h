/**********************************************************************

  Audacity: A Digital Audio Editor

  SimplePairedTwoTrack.h

  Vincent A. Busam
  Dominic Mazzoni

  This abstract class simplifies the implementation of a basic
  two track effect where you want the values of both tracks
  together.  Inherit from it if your effect doesn't just
  modifies a track in place and doesn't care how many samples
  it gets at a time.  Your derived class only needs to implement
  GetEffectName, GetEffectAction, and ProcessSimplePairedTwoTrack.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SIMPLE_PAIRED_TWO_TRACK__
#define __AUDACITY_EFFECT_SIMPLE_PAIRED_TWO_TRACK__

#include "Effect.h"

class WaveTrack;

// The only purpose for EffectSimplePairedTwoTrackBase is to provide virtual function
//		that EffectSimplePairedTwoTrack and its derived classes can use.  I couldn't
//		figure out how to do it otherwise.
// The unfortunate side effect of this base class is that it doesn't know the data types!

class EffectSimplePairedTwoTrackBase : public Effect
{
public:
	virtual ~EffectSimplePairedTwoTrackBase() {};

    // Override this method to actually process audio
    virtual bool ProcessSimplePairedTwoTrack(/*_DataType*/void *bufferLeft, 
											 /*_DataType*/void *bufferRight, // may be 0
											 sampleCount len)
						   { return false; };
};

// This class is a template.  We can do:

// typedef EffectSimplePairedTwoTrack<short int,int16Sample> EffectSimplePairedTwoTrack

//  and similarly for other types.

template<class _DataType,sampleFormat _xxxSample>
class EffectSimplePairedTwoTrack:public EffectSimplePairedTwoTrackBase 
{

public:
   // It is not usually necessary to override this method
   virtual bool Process();

   // It is necessary to provide an implementation for ProcessSimplePairedTwoTrack.

protected:
   // It is not usually necessary to override this method
   virtual bool Init();

   // It is not usually necessary to override this method
   virtual bool ProcessTwo( int count, WaveTrack *left, WaveTrack *right,
							longSampleCount lstart,
							longSampleCount rstart, sampleCount len);

   // It is not usually necessary to override this method
   void GetSamples(WaveTrack *track, 
	               longSampleCount *start,
				   sampleCount *len);

   // It is not usually necessary to override this method
   void End();

 protected:  

   // Other useful information

   sampleCount mnBlockSize;	// 0 if no processing done, thus no buffers allocated

   _DataType *mLeftBuffer;
   _DataType *mRightBuffer;

   int mnTracks;			// either 1 or 2, set in Init
};

/**********************************************************************
**********************************************************************/

#include <math.h>

#include <wx/defs.h>
#include <wx/msgdlg.h>

#include "../WaveTrack.h"		/*WAVETRACK*/

template<class _DataType,sampleFormat _xxxSample>
bool EffectSimplePairedTwoTrack<_DataType,_xxxSample>::Init()
{
   mnTracks = 1;
   mnBlockSize = 0;

   TrackListIterator iter(mTracks);
   WaveTrack *left = (WaveTrack*)(iter.First());
   if ( left == 0 )
	  return false;		// we need an existing track

   while(left) {
       longSampleCount lstart, rstart;
       sampleCount llen, rlen;
       GetSamples((WaveTrack *)left, &lstart, &llen);
     
       if (left->GetLinked()) {
	 	 mnTracks = 2;
         WaveTrack *right = (WaveTrack*)(iter.Next());
         GetSamples((WaveTrack *)right, &rstart, &rlen);
        
         if (llen != rlen || ((WaveTrack *)left)->GetRate() != ((WaveTrack *)right)->GetRate()) {
            wxMessageBox("Sorry, This effect cannot be performed on stereo tracks where "
                        "the individual channels of the track do not match.");
            return false;
		 }
      }
     
      left = (WaveTrack*)(iter.Next());
   }

   return true;
}

template<class _DataType,sampleFormat _xxxSample>
bool EffectSimplePairedTwoTrack<_DataType,_xxxSample>::Process()
{
   TrackListIterator iter(mTracks);
   int count = 0;
   WaveTrack *left = (WaveTrack*)(iter.First());
   WaveTrack *right;
   while(left) {
      longSampleCount lstart, rstart;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);

      right = NULL;
      if (left->GetLinked()) {
         right = (WaveTrack*)(iter.Next());         
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      bool success = ProcessTwo ( count,
                                  (WaveTrack *)left, 
								  (WaveTrack *)right,
                                  lstart, 
								  rstart, 
								  len );
      if (!success)
         return false;
   
      left = (WaveTrack*)(iter.Next());
      count++;
   }
   
   return true;
}

template<class _DataType,sampleFormat _xxxSample>
bool EffectSimplePairedTwoTrack<_DataType,_xxxSample>::ProcessTwo(int count, 
																	   WaveTrack *left, 
																	   WaveTrack *right,
																	   longSampleCount lstart,
																	   longSampleCount rstart, 
																	   sampleCount len)
{
   if (mnBlockSize == 0) {
      mnBlockSize = left->GetMaxBlockSize();

      mLeftBuffer  = new _DataType[mnBlockSize];
	  if ( mnTracks > 1 )
		  mRightBuffer = new _DataType[mnBlockSize];
	  else
		  mRightBuffer = 0;
   }

   // Get both buffers here

   sampleCount originalLen = len;
   longSampleCount ls = lstart;
   longSampleCount rs = rstart;
   while (len) {
      int block = mnBlockSize;
      if (block > len)
         block = len;

      left->Get((samplePtr)mLeftBuffer, _xxxSample, ls, block);
      if (right) {
         right->Get((samplePtr)mRightBuffer, _xxxSample, rs, block);
      }

	  // The derived class process the tracks here
      ProcessSimplePairedTwoTrack(mLeftBuffer, mRightBuffer, block);

      left->Set((samplePtr)mLeftBuffer, _xxxSample, ls, block);
      
      if (right) {
         right->Set((samplePtr)mRightBuffer, _xxxSample, rs, block);
      }      

      len -= block;
      ls += block;
      rs += block;
      
      if (mnTracks > 1) {      
         if (TrackGroupProgress(count, (ls-lstart)/(double)originalLen))
            break;
      }
      else {
         if (TrackProgress(count, (ls-lstart)/(double)originalLen))
            break;
      }
   }

   return true;
}

template<class _DataType,sampleFormat _xxxSample>
void EffectSimplePairedTwoTrack<_DataType,_xxxSample>::GetSamples(WaveTrack *track,
										         longSampleCount *start,
										         sampleCount *len)
{
   double trackStart = track->GetStartTime();
   double trackEnd = track->GetEndTime();
   double t0 = mT0 < trackStart? trackStart: mT0;
   double t1 = mT1 > trackEnd? trackEnd: mT1;
   
   if (t1 > t0) {
      *start = track->TimeToLongSamples(t0);
      longSampleCount end = track->TimeToLongSamples(t1);
      *len = (sampleCount)(end - *start);
   }
   else {
      *start = 0;
      *len  = 0;
   }
}

template<class _DataType,sampleFormat _xxxSample>
void EffectSimplePairedTwoTrack<_DataType,_xxxSample>::End()
{
   if (mnBlockSize) {
      delete[]mLeftBuffer;
      delete[]mRightBuffer;

   }
   mLeftBuffer = NULL;
   mRightBuffer = NULL;
}

#endif
