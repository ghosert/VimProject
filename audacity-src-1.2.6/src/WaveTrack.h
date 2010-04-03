/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_WAVETRACK__
#define __AUDACITY_WAVETRACK__

#include "Track.h"
#include "SampleFormat.h"
#include "Sequence.h"

#include <wx/longlong.h>
#include <wx/thread.h>

typedef wxLongLong_t longSampleCount; /* 64-bit int */

class Envelope;
class WaveCache;
class SpecCache;

class WaveTrack: public Track {

 private:

   //
   // Constructor / Destructor / Duplicator
   //
   // Private since only factories are allowed to construct WaveTracks
   //

   WaveTrack(DirManager * projDirManager, 
             sampleFormat format = (sampleFormat)0,
             double rate = 0);
   WaveTrack(WaveTrack &orig);

   void Init(const WaveTrack &orig);
   virtual Track *Duplicate();

   friend class TrackFactory;

   // REMOVE ME: one we figure out the coupling between AudacityProject
   // and AudioIO
   friend class AudioIO;

 public:

   virtual ~WaveTrack();

   //
   // Identifying the type of track
   //

   virtual int GetKind() const { return Wave; } 

   //
   // WaveTrack parameters
   //

   double GetRate() const;
   void SetRate(double newRate);

   // Multiplicative factor.  Only converted to dB for display.
   float GetGain() const;
   void SetGain(float newGain);

   // -1.0 (left) -> 1.0 (right)
   float GetPan() const;
   void SetPan(float newPan);

   // Takes gain and pan into account
   float GetChannelGain(int channel);

   virtual double GetOffset() const;
   virtual void SetOffset(double t);

   virtual double GetStartTime();
   virtual double GetEndTime();

   sampleFormat GetSampleFormat() {return mSequence->GetSampleFormat();}
   bool ConvertToSampleFormat(sampleFormat format);

   //
   // The following code will eventually become part of a GUIWaveTrack
   // and will be taken out of the WaveTrack class:
   //
   
   enum {
      WaveformDisplay,
      WaveformDBDisplay,
      SpectrumDisplay,
      PitchDisplay
   } WaveTrackDisplay;
   void SetDisplay(int display) {mDisplay = display;}
   int GetDisplay() {return mDisplay;}

   void GetDisplayBounds(float *min, float *max);
   void SetDisplayBounds(float min, float max);

   //
   // High-level editing
   //

   virtual bool Cut  (double t0, double t1, Track **dest);
   virtual bool Copy (double t0, double t1, Track **dest);
   virtual bool Clear(double t0, double t1);
   virtual bool Paste(double t, const Track *src);

   virtual bool Silence(double t0, double t1);
   virtual bool InsertSilence(double t, double len);

   //
   // Getting high-level data from the track for screen display and
   // clipping calculations
   //

   bool GetWaveDisplay(float *min, float *max, float *rms, sampleCount *where,
                       int numPixels, double t0, double pixelsPerSecond);
   bool GetSpectrogram(float *buffer, sampleCount *where,
                       int numPixels, int height,
                       double t0, double pixelsPerSecond,
                       bool autocorrelation);
   bool GetMinMax(float *min, float *max, double t0, double t1);

   //
   // Getting/setting samples.  The sample counts here are
   // expressed relative to t=0.0 at the track's sample rate.
   //

   bool Get(samplePtr buffer, sampleFormat format,
            longSampleCount start, sampleCount len);
   bool Set(samplePtr buffer, sampleFormat format,
            longSampleCount start, sampleCount len);

   /// You must call Flush after the last Append
   bool Append(samplePtr buffer, sampleFormat format,
               sampleCount len, unsigned int stride=1);
   /// Flush must be called after last Append
   bool Flush();

   bool AppendAlias(wxString fName, sampleCount start,
                    sampleCount len, int channel);


   //
   // Getting information about the track's internal block sizes
   // for efficiency
   //

   sampleCount GetBestBlockSize(longSampleCount t);
   sampleCount GetMaxBlockSize() const;
   sampleCount GetIdealBlockSize() const;

   //
   // XMLTagHandler callback methods for loading and saving
   //

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual void HandleXMLEndTag(const char *tag);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

   // Returns true if an error occurred while reading from XML
   virtual bool GetErrorOpening();

   //
   // Lock and unlock the track: you must lock the track before
   // doing a copy and paste between projects.
   //

   bool Lock();
   bool Unlock();

   //
   // Access the track's amplitude envelope
   //

   Envelope *GetEnvelope() { return mEnvelope; }

   // Utility function to convert between times in seconds
   // and sample positions

   longSampleCount TimeToLongSamples(double t0);

   //
   // Temporary - to be removed after TrackArtist is deleted:
   //

   Sequence *GetSequence() { return mSequence; }

 protected:

   //
   // Protected variables
   //

   Sequence     *mSequence;
   double        mRate;
   float         mGain;
   float         mPan;
   Envelope     *mEnvelope;
   samplePtr     mAppendBuffer;
   int           mAppendBufferLen;

   WaveCache    *mWaveCache;
   SpecCache    *mSpecCache;

   //
   // Data that should be part of GUIWaveTrack
   // and will be taken out of the WaveTrack class:
   //
   int           mDisplay;
   float         mDisplayMin;
   float         mDisplayMax;

   //
   // Protected methods
   //

   bool TimeToSamples(double t0, sampleCount *s0);
   void TimeToSamplesClip(double t0, sampleCount *s0);

 private:

   //
   // Private variables
   //

   wxCriticalSection mFlushCriticalSection;
   wxCriticalSection mAppendCriticalSection;

};

#endif // __AUDACITY_WAVETRACK__
