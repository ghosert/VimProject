/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveTrack.h

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/intl.h>

#include <math.h>

#include "WaveTrack.h"

#include "Envelope.h"
#include "Sequence.h"
#include "Spectrum.h"

#include "Prefs.h"
#include "Internat.h"

#include "AudioIO.h"

class WaveCache {
public:
   WaveCache(int cacheLen)
   {
      dirty = -1;
      start = -1.0;
      pps = 0.0;
      len = cacheLen;
      min = new float[len];
      max = new float[len];
      rms = new float[len];
      where = new sampleCount[len+1];
   }

   ~WaveCache()
   {
      delete[] min;
      delete[] max;
      delete[] rms;
      delete[] where;
   }

   int          dirty;
   sampleCount  len;
   double       start;
   double       pps;
   sampleCount *where;
   float       *min;
   float       *max;
   float       *rms;
};

class SpecCache {
public:
   SpecCache(int cacheLen, int viewHeight, bool autocorrelation)
   {
      dirty = -1;
      start = -1.0;
      pps = 0.0;
      len = cacheLen;
      ac = autocorrelation;
      height = viewHeight;
      freq = new float[len*height];
      where = new sampleCount[len+1];
   }

   ~SpecCache()
   {
      delete[] freq;
      delete[] where;
   }

   int          dirty;
   int          height;
   bool         ac;
   sampleCount  len;
   double       start;
   double       pps;
   sampleCount *where;
   float       *freq;
};

WaveTrack *TrackFactory::NewWaveTrack(sampleFormat format, double rate)
{
   if (format == (sampleFormat)0) 
   {
      format = (sampleFormat) gPrefs->
         Read("/SamplingRate/DefaultProjectSampleFormat", int16Sample);
   }
   if (rate == 0) 
   {
      rate = (double) gPrefs->
         Read("/SamplingRate/DefaultProjectSampleRate", AudioIO::GetOptimalSupportedSampleRate());
   }

   return new WaveTrack(mDirManager, format, rate);
}

WaveTrack::WaveTrack(DirManager *projDirManager, sampleFormat format, double rate):
   Track(projDirManager)
{
   if (format == (sampleFormat)0) 
   {
      format = (sampleFormat) gPrefs->
         Read("/SamplingRate/DefaultProjectSampleFormat", int16Sample);
   }
   if (rate == 0) 
   {
      rate = (double) gPrefs->
         Read("/SamplingRate/DefaultProjectSampleRate", AudioIO::GetOptimalSupportedSampleRate());
   }

   mDisplay = 0; // Move to GUIWaveTrack

   mSequence = new Sequence(projDirManager, format);
   mEnvelope = new Envelope();
   mRate = rate;
   mGain = 1.0;
   mPan = 0.0;
   mAppendBuffer = NULL;
   mAppendBufferLen = 0;
   SetName(_("Audio Track"));
   mWaveCache = new WaveCache(1);
   mSpecCache = new SpecCache(1, 1, false);
   mDisplayMin = -1.0;
   mDisplayMax = 1.0;
}

WaveTrack::WaveTrack(WaveTrack &orig):
   Track(orig)
{
   mDisplay = 0; // Move to GUIWaveTrack

   Init(orig);

   mSequence = new Sequence(*orig.mSequence);
   mAppendBuffer = NULL;
   mAppendBufferLen = 0;
   mEnvelope = new Envelope();
   mEnvelope->Paste(0.0, orig.mEnvelope);
   mEnvelope->SetOffset(orig.GetOffset());
   mEnvelope->SetTrackLen(orig.mSequence->GetNumSamples() / orig.mRate);
   mWaveCache = new WaveCache(1);
   mSpecCache = new SpecCache(1, 1, false);
}

// Copy the track metadata but not the contents.
void WaveTrack::Init(const WaveTrack &orig)
{
   Track::Init(orig);
   mRate = orig.mRate;
   mGain = orig.mGain;
   mPan = orig.mPan;
   SetName(orig.GetName());
   mDisplay = orig.mDisplay;
   mDisplayMin = orig.mDisplayMin;
   mDisplayMax = orig.mDisplayMax;
}

WaveTrack::~WaveTrack()
{
   if (mAppendBuffer)
      DeleteSamples(mAppendBuffer);
   delete mSequence;
   delete mEnvelope;
   delete mWaveCache;
   delete mSpecCache;
}

void WaveTrack::GetDisplayBounds(float *min, float *max)
{
   *min = mDisplayMin;
   *max = mDisplayMax;
}

void WaveTrack::SetDisplayBounds(float min, float max)
{
   mDisplayMin = min;
   mDisplayMax = max;
}

Track *WaveTrack::Duplicate()
{
   return new WaveTrack(*this);
}

double WaveTrack::GetRate() const
{
   return mRate;
}

void WaveTrack::SetRate(double newRate)
{
   mRate = newRate;
   MarkChanged();
}

float WaveTrack::GetGain() const
{
   return mGain;
}

void WaveTrack::SetGain(float newGain)
{
   mGain = newGain;
}

float WaveTrack::GetPan() const
{
   return mPan;
}

void WaveTrack::SetPan(float newPan)
{
   if (newPan > 1.0)
      mPan = 1.0;
   else if (newPan < -1.0)
      mPan = -1.0;
   else
      mPan = newPan;
}

float WaveTrack::GetChannelGain(int channel)
{
   float left = 1.0;
   float right = 1.0;

   if (mPan < 0)
      right = (mPan + 1.0);
   else if (mPan > 0)
      left = 1.0 - mPan;

   if ((channel%2) == 0)
      return left*mGain;
   else
      return right*mGain;
}

double WaveTrack::GetOffset() const
{
   return mOffset;
}

void WaveTrack::SetOffset(double t)
{
   Track::SetOffset(t);
   mEnvelope->SetOffset(t);
   MarkChanged();
}

double WaveTrack::GetStartTime()
{
   // JS: mOffset is the minimum value and it is returned; no clipping to 0
   return mOffset;
}

double WaveTrack::GetEndTime()
{
   longSampleCount numSamples = mSequence->GetNumSamples() + mAppendBufferLen;
   
   double maxLen = mOffset + numSamples/mRate;
   // JS: calculated value is not the length;
   // it is a maximum value and can be negative; no clipping to 0
   
   return maxLen;
}

bool WaveTrack::ConvertToSampleFormat(sampleFormat format)
{
   MarkChanged();
   
   return mSequence->ConvertToSampleFormat(format);
}

bool WaveTrack::Cut(double t0, double t1, Track **dest)
{
   bool success;
   sampleCount s0, s1;
   WaveTrack *newTrack;

   if (t1 < t0)
      return false;

   TimeToSamplesClip(t0, &s0);
   TimeToSamplesClip(t1, &s1);

   newTrack = new WaveTrack(mDirManager);
   delete newTrack->mSequence;
   newTrack->mSequence = NULL;
   success = mSequence->Copy(s0, s1, &newTrack->mSequence);
   if (success)
      success = mSequence->Delete(s0, s1-s0);
   
   if (!success) {
      *dest = NULL;
      delete newTrack;
      return false;
   }

   newTrack->GetEnvelope()->CopyFrom(GetEnvelope(), t0, t1);
   mEnvelope->CollapseRegion(t0, t1);

   *dest = newTrack;
   MarkChanged();
   return true;
}

bool WaveTrack::Copy(double t0, double t1, Track **dest)
{
   if (t1 < t0)
      return false;

   sampleCount s0, s1;

   TimeToSamplesClip(t0, &s0);
   TimeToSamplesClip(t1, &s1);

   WaveTrack *newTrack = new WaveTrack(mDirManager);
   newTrack->Init(*this);

   delete newTrack->mSequence;
   newTrack->mSequence = NULL;
   
   if (!mSequence->Copy(s0, s1, &newTrack->mSequence)) {
      // Error
      *dest = NULL;
      delete newTrack;
      return false;
   }

   newTrack->GetEnvelope()->CopyFrom(GetEnvelope(), t0, t1);

   *dest = newTrack;
   MarkChanged();
   return true;
}

bool WaveTrack::Paste(double t0, const Track *src)
{
   sampleCount s0;

   if (src->GetKind() != Track::Wave)
      return false;

   TimeToSamplesClip(t0, &s0);

   if (mSequence->Paste(s0, ((WaveTrack *)src)->mSequence)) {
      mEnvelope->Paste(t0, ((WaveTrack *)src)->mEnvelope);
      MarkChanged();
      return true;
   }
   else
      return false;
}

bool WaveTrack::Clear(double t0, double t1)
{
   sampleCount s0, s1;

   if (t1 < t0)
      return false;

   TimeToSamplesClip(t0, &s0);
   TimeToSamplesClip(t1, &s1);
 
   if (mSequence->Delete(s0, s1-s0)) {
      mEnvelope->CollapseRegion(t0, t1);
      MarkChanged();
      return true;
   }
   else
      return false;
}

bool WaveTrack::Silence(double t0, double t1)
{
   sampleCount s0, s1;

   if (t1 < t0)
      return false;

   TimeToSamplesClip(t0, &s0);
   TimeToSamplesClip(t1, &s1);

   MarkChanged();
   return mSequence->SetSilence(s0, s1-s0);
}

bool WaveTrack::InsertSilence(double t, double len)
{
   sampleCount s0, slen;

   TimeToSamplesClip(t, &s0);
   slen = (sampleCount)floor(len * mRate + 0.5);

   if (mSequence->InsertSilence(s0, slen)) {
      mEnvelope->InsertSpace(t, len);
      MarkChanged();
      return true;
   }
   else
      return false;
}

//
// Getting high-level data from the track for screen display and
// clipping calculations
//

bool WaveTrack::GetWaveDisplay(float *min, float *max, float *rms,
                               sampleCount *where,
                               int numPixels, double t0,
                               double pixelsPerSecond)
{
   if (mWaveCache &&
       mWaveCache->dirty == mDirty &&
       mWaveCache->start == t0 &&
       mWaveCache->len >= numPixels &&
       mWaveCache->pps == pixelsPerSecond) {

      memcpy(min, mWaveCache->min, numPixels*sizeof(float));
      memcpy(max, mWaveCache->max, numPixels*sizeof(float));
      memcpy(rms, mWaveCache->rms, numPixels*sizeof(float));
      memcpy(where, mWaveCache->where, (numPixels+1)*sizeof(sampleCount));
      return true;
   }

   WaveCache *oldCache = mWaveCache;

   mWaveCache = new WaveCache(numPixels);
   mWaveCache->pps = pixelsPerSecond;
   mWaveCache->start = t0;

   sampleCount x;

   for (x = 0; x < mWaveCache->len + 1; x++) {
      mWaveCache->where[x] =
         (sampleCount) floor(t0 * mRate +
                             ((double) x) * mRate / pixelsPerSecond + 0.5);
   }

   sampleCount s0 = mWaveCache->where[0];
   sampleCount s1 = mWaveCache->where[mWaveCache->len];
   int p0 = 0;
   int p1 = mWaveCache->len;

   // Optimization: if the old cache is good and overlaps
   // with the current one, re-use as much of the cache as
   // possible
   if (oldCache->dirty == mDirty &&
       oldCache->pps == pixelsPerSecond &&
       oldCache->where[0] < mWaveCache->where[mWaveCache->len] &&
       oldCache->where[oldCache->len] > mWaveCache->where[0]) {

      s0 = mWaveCache->where[mWaveCache->len];
      s1 = mWaveCache->where[0];
      p0 = mWaveCache->len;
      p1 = 0;

      for (x = 0; x < mWaveCache->len; x++)

         if (mWaveCache->where[x] >= oldCache->where[0] &&
             mWaveCache->where[x] <= oldCache->where[oldCache->len - 1]) {

            int ox =
                int ((double (oldCache->len) *
                      (mWaveCache->where[x] -
                       oldCache->where[0])) /(oldCache->where[oldCache->len] -
                                             oldCache->where[0]) + 0.5);

            mWaveCache->min[x] = oldCache->min[ox];
            mWaveCache->max[x] = oldCache->max[ox];
            mWaveCache->rms[x] = oldCache->rms[ox];
         } else {
            if (mWaveCache->where[x] < s0) {
               s0 = mWaveCache->where[x];
               p0 = x;
            }
            if (mWaveCache->where[x + 1] > s1) {
               s1 = mWaveCache->where[x + 1];
               p1 = x + 1;
            }
         }
   }

   if (p1 > p0) {

      /* handle values in the append buffer */

      int numSamples = mSequence->GetNumSamples();
      int a;

      for(a=p0; a<p1; a++)
         if (mWaveCache->where[a+1] > numSamples)
            break;

      if (a < p1) {
         int i;

         sampleFormat seqFormat = mSequence->GetSampleFormat();

         for(i=a; i<p1; i++) {
            sampleCount left = mWaveCache->where[i] - numSamples;
            sampleCount right = mWaveCache->where[i+1] - numSamples;

            //wxCriticalSectionLocker locker(mAppendCriticalSection);

            if (left < 0)
               left = 0;
            if (right > mAppendBufferLen)
               right = mAppendBufferLen;

            if (right > left) {
               float *b;
               sampleCount len = right-left;
               sampleCount j;

               if (seqFormat == floatSample)
                  b = &((float *)mAppendBuffer)[left];
               else {
                  b = new float[len];
                  CopySamples(mAppendBuffer + left*SAMPLE_SIZE(seqFormat),
                              seqFormat,
                              (samplePtr)b, floatSample, len);
               }

               float max = b[0];
               float min = b[0];
               float sumsq = b[0] * b[0];

               for(j=1; j<len; j++) {
                  if (b[j] > max)
                     max = b[j];
                  if (b[j] < min)
                     min = b[j];
                  sumsq += b[j]*b[j];
               }

               mWaveCache->min[i] = min;
               mWaveCache->max[i] = max;
               mWaveCache->rms[i] = (float)sqrt(sumsq / len);

               if (seqFormat != floatSample)
                  delete[] b;
            }
         }         

         // So that the sequence doesn't try to write any
         // of these values
         p1 = a;
      }

      if (p1 > p0) {
         if (!mSequence->GetWaveDisplay(&mWaveCache->min[p0],
                                        &mWaveCache->max[p0],
                                        &mWaveCache->rms[p0],
                                        p1-p0,
                                        &mWaveCache->where[p0],
                                        mRate / pixelsPerSecond))
            return false;
      }
   }

   mWaveCache->dirty = mDirty;
   delete oldCache;

   memcpy(min, mWaveCache->min, numPixels*sizeof(float));
   memcpy(max, mWaveCache->max, numPixels*sizeof(float));
   memcpy(rms, mWaveCache->rms, numPixels*sizeof(float));
   memcpy(where, mWaveCache->where, (numPixels+1)*sizeof(sampleCount));

   return true;
}

bool WaveTrack::GetSpectrogram(float *freq, sampleCount *where,
                               int numPixels, int height,
                               double t0, double pixelsPerSecond,
                               bool autocorrelation)
{
   if (mSpecCache &&
       mSpecCache->dirty == mDirty &&
       mSpecCache->start == t0 &&
       mSpecCache->ac == autocorrelation &&
       mSpecCache->height == height &&
       mSpecCache->len >= numPixels &&
       mSpecCache->pps == pixelsPerSecond) {
      memcpy(freq, mSpecCache->freq, numPixels*height*sizeof(float));
      memcpy(where, mSpecCache->where, (numPixels+1)*sizeof(sampleCount));
      return true;
   }

   SpecCache *oldCache = mSpecCache;

   mSpecCache = new SpecCache(numPixels, height, autocorrelation);
   mSpecCache->pps = pixelsPerSecond;
   mSpecCache->start = t0;

   sampleCount x;

   bool *recalc = new bool[mSpecCache->len + 1];

   for (x = 0; x < mSpecCache->len + 1; x++) {
      recalc[x] = true;
      mSpecCache->where[x] =
         (sampleCount)floor((t0*mRate) + (x*mRate/pixelsPerSecond) + 0.5);
   }

   // Optimization: if the old cache is good and overlaps
   // with the current one, re-use as much of the cache as
   // possible
   if (oldCache->dirty == GetDirty() &&
       oldCache->pps == pixelsPerSecond &&
       oldCache->height == height &&
       oldCache->ac == autocorrelation &&
       oldCache->where[0] < mSpecCache->where[mSpecCache->len] &&
       oldCache->where[oldCache->len] > mSpecCache->where[0]) {

      for (x = 0; x < mSpecCache->len; x++)
         if (mSpecCache->where[x] >= oldCache->where[0] &&
             mSpecCache->where[x] <= oldCache->where[oldCache->len - 1]) {

            int ox = (int) ((double (oldCache->len) *
                      (mSpecCache->where[x] - oldCache->where[0]))
                       / (oldCache->where[oldCache->len] -
                                             oldCache->where[0]) + 0.5);
            if (ox >= 0 && ox <= oldCache->len &&
                mSpecCache->where[x] == oldCache->where[ox]) {

               for (sampleCount i = 0; i < (sampleCount)height; i++)
                  mSpecCache->freq[height * x + i] =
                     oldCache->freq[height * ox + i];

               recalc[x] = false;
            }
         }
   }

   int maxFreqPref = gPrefs->Read("/Spectrum/MaxFreq", 8000);
   int windowSize = gPrefs->Read("/Spectrum/FFTSize", 256);
   float *buffer = new float[windowSize];

   for (x = 0; x < mSpecCache->len; x++)
      if (recalc[x]) {

         sampleCount start = mSpecCache->where[x];
         sampleCount len = windowSize;

         sampleCount i;

         if (start >= mSequence->GetNumSamples()) {
            for (i = 0; i < (sampleCount)height; i++)
               mSpecCache->freq[height * x + i] = 0;

         } else {

            if (start + len > mSequence->GetNumSamples()) {
               len = mSequence->GetNumSamples() - start;
               for (i = len; i < (sampleCount)windowSize; i++)
                  buffer[i] = 0;
            }

            mSequence->Get((samplePtr)buffer, floatSample,
                           start, len);

            ComputeSpectrum(buffer, windowSize, height,
                            maxFreqPref, windowSize,
                            mRate, &mSpecCache->freq[height * x],
                            autocorrelation);
         }
      }

   delete[]buffer;
   delete[]recalc;
   delete oldCache;

   mSpecCache->dirty = GetDirty();
   memcpy(freq, mSpecCache->freq, numPixels*height*sizeof(float));
   memcpy(where, mSpecCache->where, (numPixels+1)*sizeof(sampleCount));

   return true;
}

bool WaveTrack::GetMinMax(float *min, float *max,
                          double t0, double t1)
{
   *min = float(0.0);
   *max = float(0.0);

   if (t0 > t1)
      return false;

   if (t0 == t1)
      return true;

   sampleCount s0, s1;

   TimeToSamplesClip(t0, &s0);
   TimeToSamplesClip(t1, &s1);

   return mSequence->GetMinMax(s0, s1-s0, min, max);
}

//
// Getting/setting samples.  The sample counts here are
// expressed relative to t=0.0 at the track's sample rate.
//

bool WaveTrack::Get(samplePtr buffer, sampleFormat format,
                    longSampleCount start, sampleCount len)
{
   longSampleCount startTime = (longSampleCount)floor(mOffset*mRate + 0.5);
   longSampleCount endTime = startTime + mSequence->GetNumSamples();

   if (start+len < startTime || start>=endTime) {
      ClearSamples(buffer, format, 0, len);
      return true;
   }

   sampleCount s0 = (sampleCount)(start - startTime);
   sampleCount soffset = 0;
   sampleCount getlen = len;

   if (s0 < 0) {
      soffset = -s0;
      getlen -= soffset;
      s0 = 0;
   }

   if (s0+getlen > mSequence->GetNumSamples())
      getlen = mSequence->GetNumSamples() - s0;

   if (!mSequence->Get(buffer + soffset*SAMPLE_SIZE(format), format,
                       s0, getlen)) {
      ClearSamples(buffer, format, 0, len);
      return false;
   }

   ClearSamples(buffer, format, 0, soffset);
   ClearSamples(buffer, format, soffset+getlen, len-(soffset+getlen));

   return true;
}

bool WaveTrack::Set(samplePtr buffer, sampleFormat format,
                    longSampleCount start, sampleCount len)
{
   longSampleCount startTime = (longSampleCount)floor(mOffset*mRate + 0.5);

   sampleCount s0 = (sampleCount)(start - startTime);

   if (s0 < 0) {
      len += s0;
      buffer -= s0*SAMPLE_SIZE(format);
      s0 = 0;
   }

   if (s0 + len > mSequence->GetNumSamples())
      len = mSequence->GetNumSamples() - s0;

   MarkChanged();
   return mSequence->Set(buffer, format, s0, len);
}

bool WaveTrack::Append(samplePtr buffer, sampleFormat format,
                       sampleCount len, unsigned int stride /* = 1 */)
{
   //wxCriticalSectionLocker locker(mAppendCriticalSection);

   sampleCount maxBlockSize = mSequence->GetMaxBlockSize();
   sampleCount blockSize = mSequence->GetIdealAppendLen();
   sampleFormat seqFormat = mSequence->GetSampleFormat();

   if (!mAppendBuffer)
      mAppendBuffer = NewSamples(maxBlockSize, seqFormat);

   for(;;) {
      if (mAppendBufferLen >= blockSize) {
         bool success =
            mSequence->Append(mAppendBuffer, seqFormat, blockSize);
         if (!success)
            return false;
         memmove(mAppendBuffer,
                 mAppendBuffer + blockSize * SAMPLE_SIZE(seqFormat),
                 (mAppendBufferLen - blockSize) * SAMPLE_SIZE(seqFormat));
         mAppendBufferLen -= blockSize;
         blockSize = mSequence->GetIdealAppendLen();
      }

      if (len == 0)
         break;

      int toCopy = maxBlockSize - mAppendBufferLen;
      if (toCopy > len)
         toCopy = len;

      CopySamples(buffer, format,
                  mAppendBuffer + mAppendBufferLen * SAMPLE_SIZE(seqFormat),
                  seqFormat,
                  toCopy,
                  true, /* high quality */
                  stride);

      mAppendBufferLen += toCopy;
      buffer += toCopy * SAMPLE_SIZE(format) * stride;
      len -= toCopy;
   }

   mEnvelope->SetTrackLen(mSequence->GetNumSamples() / mRate);
   MarkChanged();

   return true;
}

bool WaveTrack::AppendAlias(wxString fName, sampleCount start,
                            sampleCount len, int channel)
{
   MarkChanged();
   bool ret=mSequence->AppendAlias(fName, start, len, channel);
   if(ret==true)
      mEnvelope->SetTrackLen(mSequence->GetNumSamples() / mRate);

   return ret;
}

sampleCount WaveTrack::GetBestBlockSize(longSampleCount s)
{
   longSampleCount startTime = (longSampleCount)floor(mOffset*mRate + 0.5);
   longSampleCount endTime = startTime + mSequence->GetNumSamples();

   if (s < startTime || s >= endTime)
      return mSequence->GetMaxBlockSize();

   return mSequence->GetBestBlockSize((sampleCount)(s - startTime));
}

sampleCount WaveTrack::GetMaxBlockSize() const
{
   return mSequence->GetMaxBlockSize();
}

sampleCount WaveTrack::GetIdealBlockSize() const
{
   return mSequence->GetIdealBlockSize();
}

bool WaveTrack::Flush()
{
   //wxCriticalSectionLocker locker(mFlushCriticalSection);

   bool success = true;
   sampleFormat seqFormat = mSequence->GetSampleFormat();

   if (mAppendBufferLen > 0) {
      success = mSequence->Append(mAppendBuffer, seqFormat, mAppendBufferLen);
      if (success) {
         mAppendBufferLen = 0;
         mEnvelope->SetTrackLen(mSequence->GetNumSamples() / mRate);
      }
   }

   return success;
}

bool WaveTrack::HandleXMLTag(const char *tag, const char **attrs)
{
   if (!strcmp(tag, "wavetrack")) {
      while(*attrs) {
         const char *attr = *attrs++;
         const char *value = *attrs++;
         
         if (!value)
            break;
         
         if (!strcmp(attr, "rate"))
            Internat::CompatibleToDouble(wxString(value), &mRate);
         else if (!strcmp(attr, "offset")) {
            Internat::CompatibleToDouble(wxString(value), &mOffset);
            mEnvelope->SetOffset(mOffset);
         }
         else if (!strcmp(attr, "gain")) {
            double d;
            Internat::CompatibleToDouble(wxString(value), &d);
            mGain = d;
         }
         else if (!strcmp(attr, "pan")) {
            double d;
            Internat::CompatibleToDouble(wxString(value), &d);
            if (d >= -1.0 && d <= 1.0)
               mPan = d;
         }
         else if (!strcmp(attr, "name"))
            mName = value;
         else if (!strcmp(attr, "channel"))
            mChannel = atoi(value);
         else if (!strcmp(attr, "linked"))
            mLinked = atoi(value);
         
      } // while
      return true;
   }

   return false;
}

void WaveTrack::HandleXMLEndTag(const char *tag)
{
   if (!strcmp(tag, "wavetrack")) {
      mEnvelope->SetTrackLen(mSequence->GetNumSamples() / mRate);
   }   
}

XMLTagHandler *WaveTrack::HandleXMLChild(const char *tag)
{
   if (!strcmp(tag, "sequence"))
      return mSequence;
   else if (!strcmp(tag, "envelope"))
      return mEnvelope;
   else
      return NULL;
}

void WaveTrack::WriteXML(int depth, FILE *fp)
{
   int i;

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<wavetrack ");
   fprintf(fp, "name=\"%s\" ", XMLEsc(mName).c_str());
   fprintf(fp, "channel=\"%d\" ", mChannel);
   fprintf(fp, "linked=\"%d\" ", mLinked);
   fprintf(fp, "offset=\"%s\" ", Internat::ToString(mOffset, 8).c_str());
   fprintf(fp, "rate=\"%s\" ", Internat::ToString(mRate).c_str());
   fprintf(fp, "gain=\"%s\" ", Internat::ToString((double)mGain).c_str());
   fprintf(fp, "pan=\"%s\" ", Internat::ToString((double)mPan).c_str());
   fprintf(fp, ">\n");

   mSequence->WriteXML(depth+1, fp);

   mEnvelope->WriteXML(depth+1, fp);

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "</wavetrack>\n");
}

bool WaveTrack::GetErrorOpening()
{
   return mSequence->GetErrorOpening();
}

bool WaveTrack::Lock()
{
   return mSequence->Lock();
}

bool WaveTrack::Unlock()
{
   return mSequence->Unlock();
}

longSampleCount WaveTrack::TimeToLongSamples(double t0)
{
   return (longSampleCount)floor(t0 * mRate + 0.5);
}

bool WaveTrack::TimeToSamples(double t0, sampleCount *s0)
{
   if ((t0 < mOffset) ||
       (t0 > mOffset + mSequence->GetNumSamples()/mRate)) {
      *s0 = -1;
      return false;
   }

   *s0 = (sampleCount)floor(((t0 - mOffset) * mRate) + 0.5);
   return true;
}

void WaveTrack::TimeToSamplesClip(double t0, sampleCount *s0)
{
   if (t0 < mOffset)
      *s0 = 0;
   else if (t0 > mOffset + mSequence->GetNumSamples()/mRate)
      *s0 = mSequence->GetNumSamples();
   else
      *s0 = (sampleCount)floor(((t0 - mOffset) * mRate) + 0.5);
}
