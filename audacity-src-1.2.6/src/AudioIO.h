/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.h

  Dominic Mazzoni

  Use the PortAudio library to play and record sound

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO__
#define __AUDACITY_AUDIO_IO__

#include "portaudio.h"

#include "Audacity.h"

#if USE_PORTMIXER
#include "portmixer.h"
#endif

#include <wx/string.h>
#include <wx/thread.h>

#include "WaveTrack.h"
#include "SampleFormat.h"

class AudioIO;
class RingBuffer;
class Mixer;
class TimeTrack;
class AudioThread;
class Meter;

extern AudioIO *gAudioIO;

void InitAudioIO();
void DeinitAudioIO();

class AudioIO {

 public:
   AudioIO();
   ~AudioIO();

   void StartMonitoring(double sampleRate);

   /* If successful, returns a token identifying this particular stream
    * instance.  For use with IsStreamActive() below */
   int StartStream(WaveTrackArray playbackTracks, WaveTrackArray captureTracks,
                   TimeTrack *timeTrack, double sampleRate,
                   double t0, double t1, bool playLooped = false);

   void StopStream();

   /* Returns true if audio i/o is busy starting, stopping,
      playing, or recording.  When this is
      false, it's safe to start playing or recording */
   bool IsBusy();

   /* Returns true if the audio i/o is running at all,
      but doesn't return true if the device has been closed
      but some disk i/o or cleanup is still going on.
      If you want to know if it's safe to start a new
      stream, use IsBusy()
   */
   bool IsStreamActive();

   /* Returns true if the audio i/o is still running the stream instance
    * identified by this token, but doesn't return true if the device
    * has been closed but some disk i/o or cleanup is still going on
    */
   bool IsStreamActive(int token);

   /* Returns true if the stream is active, or even if audio I/O is
    * busy cleaning up its data or writing to disk.  This is used
    * by TrackPanel to determine when a track has been completely
    * recorded, and it's safe to flush to disk.
   */
   bool IsAudioTokenActive(int token);

   /* Returns true if we're monitoring input (but not recording or
      playing actual audio) */
   bool IsMonitoring();

   void SetPaused(bool state);
   bool IsPaused();

   /* Mixer services are always available.  If no stream is running, these
    * methods use whatever device is specified by the preferences.  If a
    * stream *is* running, naturally they manipulate the mixer associated
    * with that stream.  If no mixer is available, they are emulated
    * (a gain is applied to input and output samples).
    */
   void SetMixer(int inputSource, float inputVolume,
                 float playbackVolume);
   void GetMixer(int *inputSource, float *inputVolume,
                 float *playbackVolume);
   wxArrayString GetInputSourceNames();
   void HandleDeviceChange();

   /* Set the current VU meters - this should be done once after
      each call to StartStream currently */
   void SetMeters(Meter *inputMeter, Meter *outputMeter);
   
   /* Get a list of sample rates the current input/output device
    * supports. Since there is no concept (yet) for different input/output
    * sample rates, this currently returns only sample rates that are
    * supported on both the output and input device. If no information
    * about available sample rates can be fetched, it returns a default
    * list.
    * You can explicitely give the names of the playDevice/recDevice.
    * If you don't give them, the default devices from the preferences
    * will be used.
    */
   static wxArrayLong GetSupportedSampleRates(wxString playDevice = "",
                                              wxString recDevice = "");

   /* Get a supported sample rate which can be used a an optimal
    * default. Currently, this uses the first supported rate in
    * the list [44100, 48000, highest sample rate].
    */
   static int GetOptimalSupportedSampleRate();

   /* This is given in seconds based on starting at t0
    * When playing looped, this will start from t0 again,
    * too. So the returned time should be always between
    * t0 and t1
    */
   double GetStreamTime();

   sampleFormat GetCaptureFormat() { return mCaptureFormat; }
   int GetNumCaptureChannels() { return mNumCaptureChannels; }

private:

   bool StartPortAudioStream(double sampleRate,
                             unsigned int numPlaybackChannels,
                             unsigned int numCaptureChannels,
                             sampleFormat captureFormat);

   void FillBuffers();

   int GetCommonlyAvailPlayback();
   int GetCommonlyAvailCapture();

   double NormalizeStreamTime(double absoluteTime) const;

   AudioThread        *mThread;
   RingBuffer        **mCaptureBuffers;
   WaveTrackArray      mCaptureTracks;
   RingBuffer        **mPlaybackBuffers;
   WaveTrackArray      mPlaybackTracks;
   Mixer             **mPlaybackMixers;
   int                 mStreamToken;
   int                 mStopStreamCount;
   static int          mNextStreamToken;
   double              mRate;
   double              mT;
   double              mT0;
   double              mT1;
   double              mPlaybackRingBufferSecs;
   double              mCaptureRingBufferSecs;
   double              mMaxPlaybackSecsToCopy;
   double              mMinCaptureSecsToCopy;
   bool                mPaused;
   double              mPausePosition;
#if USE_PORTAUDIO_V19
   PaStream           *mPortStreamV19;
#else
   PortAudioStream    *mPortStreamV18;
   double              mLastIndicator;
   double              mLastStableIndicator;
   volatile double     mPausedSeconds;
   volatile bool       mInCallbackFinishedState;
#endif
   bool                mSoftwarePlaythrough;
#ifdef __WXMAC__
   bool                mNoModifyDevice;
#endif
   double              mLastRecordingOffset;
   unsigned int        mNumCaptureChannels;
   unsigned int        mNumPlaybackChannels;
   sampleFormat        mCaptureFormat;
   float              *mTempFloats;
   int                 mLostSamples;
   volatile bool       mAudioThreadShouldCallFillBuffersOnce;
   volatile bool       mAudioThreadFillBuffersLoopRunning;
   volatile double     mLastBufferAudibleTime;
   volatile double     mTotalSamplesPlayed;
   PaError             mLastPaError;

   Meter              *mInputMeter;
   Meter              *mOutputMeter;
   bool                mUpdateMeters;
   bool                mUpdatingMeters;

   #if USE_PORTMIXER
   PxMixer            *mPortMixer;
   float               mPreviousHWPlaythrough;
   #endif

   bool                mEmulateMixerOutputVol;
   bool                mEmulateMixerInputVol;
   float               mMixerOutputVol;
   float               mMixerInputVol;

   bool                mPlayLooped;

   friend class AudioThread;

   friend void InitAudioIO();
   friend void DeinitAudioIO();

#if USE_PORTAUDIO_V19
   friend int audacityAudioCallback(
                const void *inputBuffer, void *outputBuffer,
                unsigned long framesPerBuffer,
                const PaStreamCallbackTimeInfo *timeInfo,
                PaStreamCallbackFlags statusFlags, void *userData );
#else
   friend int audacityAudioCallback(
                void *inputBuffer, void *outputBuffer,
                unsigned long framesPerBuffer,
                PaTimestamp outTime, void *userData );
#endif

};

#endif
