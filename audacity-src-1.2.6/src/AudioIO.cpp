/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Dominic Mazzoni
  Joshua Haberman
  Markus Meyer

  Use the PortAudio library to play and record sound

  Great care and attention to detail are necessary for understanding and
  modifying this system.  The code in this file is run from three
  different thread contexts: the UI thread, the disk thread (which
  this file creates and maintains) and the PortAudio callback thread.
  To highlight this deliniation, the file is divided into three parts
  based on what thread context each function is intended to run in.

  TODO: run through all functions called from audio and portaudio threads
  to verify they are thread-safe.

**********************************************************************/

#include <math.h>
#include <stdlib.h>

#ifdef __WXMSW__
#include <malloc.h>
#endif

#ifdef __WXMAC__
extern int PaOSX_Leave_Device_Alone_Mode;
#endif

#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>

#include "AudioIO.h"
#include "WaveTrack.h"
#include "Mix.h"
#include "RingBuffer.h"
#include "Prefs.h"
#include "TimeTrack.h"

#include "widgets/Meter.h"

#if USE_PORTMIXER
#include "MixerToolBar.h"
#endif

AudioIO *gAudioIO;

// static
int AudioIO::mNextStreamToken = 0;

#if USE_PORTAUDIO_V19
int audacityAudioCallback(const void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo *timeInfo,
                          const PaStreamCallbackFlags statusFlags, void *userData );
#else
int audacityAudioCallback(void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          PaTimestamp outTime, void *userData );
#endif

//////////////////////////////////////////////////////////////////////
//
//     class AudioThread - declaration and glue code
//
//////////////////////////////////////////////////////////////////////

#ifdef __WXMAC__

// On Mac OS X, it's better not to use the wxThread class.
// We use our own implementation based on pthreads instead.

#include <pthread.h>
#include <time.h>

class AudioThread {
 public:
   typedef int ExitCode;
   AudioThread() { mDestroy = false; mThread = NULL; }
   ExitCode Entry();
   void Create() {}
   void Delete() {
      mDestroy = true;
      pthread_join(mThread, NULL);
   }
   bool TestDestroy() { return mDestroy; }
   void Sleep(int ms) {
      struct timespec spec;
      spec.tv_sec = 0;
      spec.tv_nsec = ms * 1000 * 1000;
      nanosleep(&spec, NULL);
   }
   static void *callback(void *p) {
      AudioThread *th = (AudioThread *)p;
      return (void *)th->Entry();
   }
   void Run() {
      pthread_create(&mThread, NULL, callback, this);
   }
 private:
   bool mDestroy;
   pthread_t mThread;

};

#else

// The normal wxThread-derived AudioThread class for all other
// platforms:
class AudioThread : public wxThread {
 public:
   AudioThread():wxThread(wxTHREAD_JOINABLE) {}
   virtual ExitCode Entry();
};

#endif

//////////////////////////////////////////////////////////////////////
//
//     UI Thread Context
//
//////////////////////////////////////////////////////////////////////

void InitAudioIO()
{
   gAudioIO = new AudioIO();
   gAudioIO->mThread->Run();
}

void DeinitAudioIO()
{
   delete gAudioIO;
}

AudioIO::AudioIO()
{
   mAudioThreadShouldCallFillBuffersOnce = false;
   mAudioThreadFillBuffersLoopRunning = false;
#if USE_PORTAUDIO_V19
   mPortStreamV19 = NULL;
#else
   mPortStreamV18 = NULL;
   mInCallbackFinishedState = false;
#endif
   mStreamToken = 0;
   mStopStreamCount = 0;
   mTempFloats = new float[65536]; // TODO: out channels * PortAudio buffer size

   mLastPaError = paNoError;

   mNumCaptureChannels = 0;
   mPaused = false;
   mPlayLooped = false;

   mUpdateMeters = false;
   mUpdatingMeters = false;

   PaError err = Pa_Initialize();

   if (err != paNoError) {
      wxString errStr = _("There was an error initializing the audio i/o layer.\n");
      errStr += _("You will not be able to play or record audio.\n\n");
      wxString paErrStr = Pa_GetErrorText(err);
      if (paErrStr)
         errStr += _("Error: ")+paErrStr;
      // XXX: we are in libaudacity, popping up dialogs not allowed!  A
      // long-term solution will probably involve exceptions
      wxMessageBox(errStr, _("Error Initializing Audio"), wxICON_ERROR|wxOK);

      // Since PortAudio is not initialized, all calls to PortAudio
      // functions will fail.  This will give reasonable behavior, since
      // the user will be able to do things not relating to audio i/o,
      // but any attempt to play or record will simply fail.
   }

   // Start thread
   mThread = new AudioThread();
   mThread->Create();

#if defined(USE_PORTMIXER)
   mPortMixer = NULL;
   mPreviousHWPlaythrough = -1.0;
   HandleDeviceChange();
#else
   mEmulateMixerOutputVol = true;
   mMixerOutputVol = 1.0;
   mEmulateMixerInputVol = true;
   mMixerInputVol = 1.0;
#endif
}

AudioIO::~AudioIO()
{
#if defined(USE_PORTMIXER)
   if( mPortMixer ) {
      #if __WXMAC__
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
      #endif
      Px_CloseMixer(mPortMixer);
   }
   mPortMixer = NULL;
#endif
   Pa_Terminate();

   /* Delete is a "graceful" way to stop the thread.
      (Kill is the not-graceful way.) */
   wxYield();
   mThread->Delete();

   delete [] mTempFloats;
   delete mThread;
}

void AudioIO::SetMixer(int recordDevice, float recordVolume,
                       float playbackVolume)
{
   mMixerOutputVol = playbackVolume;
   mMixerInputVol = recordVolume;

#if defined(USE_PORTMIXER)

   PxMixer *mixer = mPortMixer;

   if( mixer )
   {
      int oldRecordDevice = Px_GetCurrentInputSource(mixer);
      float oldRecordVolume = Px_GetInputVolume(mixer);
      float oldPlaybackVolume = Px_GetPCMOutputVolume(mixer);

      if( recordDevice != oldRecordDevice )
         Px_SetCurrentInputSource(mixer, recordDevice);
      if( fabs(oldRecordVolume-recordVolume) > 0.05 )
         Px_SetInputVolume(mixer, recordVolume);
      if( fabs(oldPlaybackVolume-playbackVolume) > 0.05 )
         Px_SetPCMOutputVolume(mixer, playbackVolume);

      return;
   }
#endif
}

void AudioIO::GetMixer(int *recordDevice, float *recordVolume,
                       float *playbackVolume)
{
#if defined(USE_PORTMIXER)

   PxMixer *mixer = mPortMixer;

   if( mixer )
   {
      *recordDevice = Px_GetCurrentInputSource(mixer);

      if (mEmulateMixerInputVol)
         *recordVolume = mMixerInputVol;
      else
         *recordVolume = Px_GetInputVolume(mixer);

      if (mEmulateMixerOutputVol)
         *playbackVolume = mMixerOutputVol;
      else
         *playbackVolume = Px_GetPCMOutputVolume(mixer);

      return;
   }

#endif

   *recordDevice = 0;
   *recordVolume = mMixerInputVol;
   *playbackVolume = mMixerOutputVol;
}

wxArrayString AudioIO::GetInputSourceNames()
{
#if defined(USE_PORTMIXER)

   wxArrayString deviceNames;

   if( mPortMixer )
   {
      int numSources = Px_GetNumInputSources(mPortMixer);
      for( int source = 0; source < numSources; source++ )
         deviceNames.Add(Px_GetInputSourceName(mPortMixer, source));
   }

   return deviceNames;

#else

   wxArrayString blank;

   return blank;

#endif
}

void AudioIO::HandleDeviceChange()
{
   // This should not happen, but it would screw things up if it did.
   if( IsStreamActive() )
      return;

#if defined(USE_PORTMIXER)
   if( mPortMixer )
      Px_CloseMixer(mPortMixer);
   mPortMixer = NULL;

   int recDeviceNum = Pa_GetDefaultInputDeviceID();
   int playDeviceNum = Pa_GetDefaultOutputDeviceID();
   wxString recDevice = gPrefs->Read("/AudioIO/RecordingDevice", "");
   wxString playDevice = gPrefs->Read("/AudioIO/PlaybackDevice", "");
   int j;

   // msmeyer: This tries to open the device with the highest samplerate
   // available on this device, using 44.1kHz as the default, if the info
   // cannot be fetched.

   for(j=0; j<Pa_CountDevices(); j++) {
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);

      if (info->name == playDevice && info->maxOutputChannels > 0)
         playDeviceNum = j;

      if (info->name == recDevice && info->maxInputChannels > 0)
         recDeviceNum = j;
   }
   
   wxArrayLong supportedSampleRates = GetSupportedSampleRates(playDevice, recDevice);
   int highestSampleRate = supportedSampleRates[supportedSampleRates.GetCount() - 1];

   mEmulateMixerInputVol = true;
   mEmulateMixerOutputVol = true;
   mMixerInputVol = 1.0;
   mMixerOutputVol = 1.0;

   // dmazzoni: this is a hack to tell PortAudio to not actually
   // modify the device settings, but to just open as-is.
   #ifdef __WXMAC__
   PaOSX_Leave_Device_Alone_Mode = 1;
   #endif

   PortAudioStream *stream;
   int error;
   error = Pa_OpenStream(&stream, recDeviceNum, 2, paFloat32, NULL,
                         playDeviceNum, 2, paFloat32, NULL,
                         highestSampleRate, 512, 1, paClipOff | paDitherOff,
                         audacityAudioCallback, NULL);

   if( error ) {
      error = Pa_OpenStream(&stream, recDeviceNum, 2, paFloat32, NULL,
                            paNoDevice, 0, paFloat32, NULL,
                            highestSampleRate, 512, 1, paClipOff | paDitherOff,
                            audacityAudioCallback, NULL);
   }

   #ifdef __WXMAC__
   PaOSX_Leave_Device_Alone_Mode = 0;
   #endif

   if( error )
      return;

   mPortMixer = Px_OpenMixer(stream, 0);

   if (!mPortMixer) {
      Pa_CloseStream(stream);
      return;
   }

   // Determine mixer capabilities - it it doesn't support either
   // input or output, we emulate them (by multiplying this value
   // by all incoming/outgoing samples)

   mMixerOutputVol = Px_GetPCMOutputVolume(mPortMixer);
   mEmulateMixerOutputVol = false;
   Px_SetPCMOutputVolume(mPortMixer, 0.0);
   if (Px_GetPCMOutputVolume(mPortMixer) > 0.1)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, 0.2);
   if (Px_GetPCMOutputVolume(mPortMixer) < 0.1 ||
       Px_GetPCMOutputVolume(mPortMixer) > 0.3)
      mEmulateMixerOutputVol = true;
   Px_SetPCMOutputVolume(mPortMixer, mMixerOutputVol);

   mMixerInputVol = Px_GetInputVolume(mPortMixer);
   mEmulateMixerInputVol = false;
   Px_SetInputVolume(mPortMixer, 0.0);
   if (Px_GetInputVolume(mPortMixer) > 0.1)
      mEmulateMixerInputVol = true;
   Px_SetInputVolume(mPortMixer, 0.2);
   if (Px_GetInputVolume(mPortMixer) < 0.1 ||
       Px_GetInputVolume(mPortMixer) > 0.3)
      mEmulateMixerInputVol = true;
   Px_SetInputVolume(mPortMixer, mMixerInputVol);

   Pa_CloseStream(stream);

   #if 0
   printf("PortMixer: Output: %s Input: %s\n",
          mEmulateMixerOutputVol? "emulated": "native",
          mEmulateMixerInputVol? "emulated": "native");
   #endif

   mMixerInputVol = 1.0;
   mMixerOutputVol = 1.0;

#endif
}

PaSampleFormat AudacityToPortAudioSampleFormat(sampleFormat format)
{
   switch(format) {
   case int16Sample:
      return paInt16;
   case int24Sample:
      return paInt24;
   case floatSample:
   default:
      return paFloat32;
   }
}

bool AudioIO::StartPortAudioStream(double sampleRate,
                                   unsigned int numPlaybackChannels,
                                   unsigned int numCaptureChannels,
                                   sampleFormat captureFormat)
{
   mLastPaError = paNoError;
   mRate = sampleRate;

   mNumPlaybackChannels = numPlaybackChannels;
   mNumCaptureChannels = numCaptureChannels;

#if USE_PORTAUDIO_V19
   PaStreamParameters *playbackParameters = NULL;
   PaStreamParameters *captureParameters = NULL;

   if( numPlaybackChannels > 0)
   {
      playbackParameters = new PaStreamParameters;
      wxString playbackDeviceName = gPrefs->Read("/AudioIO/PlaybackDevice", "");
      const PaDeviceInfo *playbackDeviceInfo;
      
      playbackParameters->device = Pa_GetDefaultOutputDevice();
      
      if( playbackDeviceName != "" )
      {
         for( int i = 0; i < Pa_GetDeviceCount(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info->name == playbackDeviceName && info->maxOutputChannels > 0)
               playbackParameters->device = i;
         }
      }
      
      playbackDeviceInfo = Pa_GetDeviceInfo( playbackParameters->device );
      
      if( playbackDeviceInfo == NULL )
         return false;
      
      // regardless of source formats, we always mix to float
      playbackParameters->sampleFormat = paFloat32;
      playbackParameters->hostApiSpecificStreamInfo = NULL;
      playbackParameters->channelCount = mNumPlaybackChannels;
      playbackParameters->suggestedLatency =
         playbackDeviceInfo->defaultLowOutputLatency;
   }

   if( numCaptureChannels > 0)
   {
      mCaptureFormat = captureFormat;
      captureParameters = new PaStreamParameters;
      const PaDeviceInfo *captureDeviceInfo;
      wxString captureDeviceName = gPrefs->Read("/AudioIO/RecordingDevice", "");

      captureParameters->device = Pa_GetDefaultInputDevice();

      if( captureDeviceName != "" )
      {
         for( int i = 0; i < Pa_GetDeviceCount(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info->name == captureDeviceName && info->maxInputChannels > 0)
               captureParameters->device = i;
         }
      }

      captureDeviceInfo = Pa_GetDeviceInfo( captureParameters->device );

      if( captureDeviceInfo == NULL )
         return false;

      captureParameters->sampleFormat =
         AudacityToPortAudioSampleFormat(mCaptureFormat);

      captureParameters->hostApiSpecificStreamInfo = NULL;
      captureParameters->channelCount = mNumCaptureChannels;
      captureParameters->suggestedLatency =
         captureDeviceInfo->defaultHighInputLatency;
   }

   mLastPaError = Pa_OpenStream( &mPortStreamV19,
                                 captureParameters, playbackParameters,
                                 mRate, paFramesPerBufferUnspecified,
                                 paNoFlag,
                                 audacityAudioCallback, NULL );

#if 0 //USE_PORTMIXER  TODO: support PortMixer with v19
   if (mPortMixer)
      Px_CloseMixer(mPortMixer);         
   mPortMixer = NULL;
   if (mPortStream != NULL && mLastPaError == paNoError) {
      mPortMixer = Px_OpenMixer(mPortStream, 0);
      if (mPortMixer)
         AdjustMixer();
   }
#endif

   // these may be null, but deleting a null pointer should never crash.
   delete captureParameters;
   delete playbackParameters;

#else

   PaDeviceID captureDevice = paNoDevice,
              playbackDevice = paNoDevice;
   PaSampleFormat paCaptureFormat = 0;

   if( numPlaybackChannels > 0 )
   {
      playbackDevice =  Pa_GetDefaultOutputDeviceID();
      wxString playbackDeviceName = gPrefs->Read("/AudioIO/PlaybackDevice", "");

      if( playbackDeviceName != "" )
      {
         for( int i = 0; i < Pa_CountDevices(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info->name == playbackDeviceName && info->maxOutputChannels > 0)
               playbackDevice = i;
         }
      }
   }

   if( numCaptureChannels > 0 )
   {
      // For capture, every input channel gets its own track
      mCaptureFormat = captureFormat;
      captureDevice =  Pa_GetDefaultInputDeviceID();
      wxString captureDeviceName = gPrefs->Read("/AudioIO/RecordingDevice", "");
      
      if( captureDeviceName != "" )
      {
         for( int i = 0; i < Pa_CountDevices(); i++)
         {
            const PaDeviceInfo* info = Pa_GetDeviceInfo(i);
            if (info->name == captureDeviceName && info->maxInputChannels > 0)
               captureDevice = i;
         }
      }
      
      paCaptureFormat =
         AudacityToPortAudioSampleFormat(mCaptureFormat);
   }

   mPortStreamV18 = NULL;

   #ifdef __WXMAC__
   PaOSX_Leave_Device_Alone_Mode = mNoModifyDevice;
   #endif

   mLastPaError = Pa_OpenStream( &mPortStreamV18,
                                 /* capture parameters */
                                 captureDevice,
                                 mNumCaptureChannels,
                                 paCaptureFormat,
                                 NULL,
                                 /* playback parameters */
                                 playbackDevice,
                                 mNumPlaybackChannels,
                                 paFloat32,
                                 NULL,
                                 /* general parameters */
                                 mRate, 256, 0,
                                 paClipOff | paDitherOff,
                                 audacityAudioCallback, NULL );

#if USE_PORTMIXER

   if (mPortMixer) {
      #if __WXMAC__
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
      #endif
      Px_CloseMixer(mPortMixer);
   }
   mPortMixer = NULL;
   if (mPortStreamV18 != NULL && mLastPaError == paNoError) {
      mPortMixer = Px_OpenMixer(mPortStreamV18, 0);

      #ifdef __MACOSX__
      if (mPortMixer) {
         if (Px_SupportsPlaythrough(mPortMixer)) {
            bool playthrough;

            mPreviousHWPlaythrough = Px_GetPlaythrough(mPortMixer);

            gPrefs->Read("/AudioIO/Playthrough", &playthrough, false);
            if (playthrough)
               Px_SetPlaythrough(mPortMixer, 1.0);
            else
               Px_SetPlaythrough(mPortMixer, 0.0);
         }
      }
      #endif
   }

#endif

   mInCallbackFinishedState = false; // v18 only

#endif   

   return (mLastPaError == paNoError);
}

void AudioIO::StartMonitoring(double sampleRate)
{
#if USE_PORTAUDIO_V19
   if ( mPortStreamV19 || mStreamToken )
      return;
#else
   if ( mPortStreamV18 || mStreamToken )
      return;
#endif

   bool success;
   long captureChannels;
   sampleFormat captureFormat = (sampleFormat)
      gPrefs->Read("/SamplingRate/DefaultProjectSampleFormat", floatSample);
   gPrefs->Read("/AudioIO/RecordChannels", &captureChannels, 1L);
   gPrefs->Read("/AudioIO/SWPlaythrough", &mSoftwarePlaythrough, false);
   #if __WXMAC__
   gPrefs->Read("/AudioIO/NoModifyDevice", &mNoModifyDevice, false);
   #endif
   int playbackChannels = 0;

   if (mSoftwarePlaythrough)
      playbackChannels = 2;

   success = StartPortAudioStream(sampleRate, (unsigned int)playbackChannels,
                                  (unsigned int)captureChannels,
                                  captureFormat);

   // Now start the PortAudio stream!
#if USE_PORTAUDIO_V19
   mLastPaError = Pa_StartStream( mPortStreamV19 );
#else
   mLastPaError = Pa_StartStream( mPortStreamV18 );
#endif
}

int AudioIO::StartStream(WaveTrackArray playbackTracks,
                         WaveTrackArray captureTracks,
                         TimeTrack *timeTrack, double sampleRate,
                         double t0, double t1, bool playLooped /* = false */)
{
   if( IsBusy() )
      return 0;

   // We just want to set mStreamToken to -1 - this way avoids
   // an extremely rare but possible race condition, if two functions
   // somehow called StartStream at the same time...
   mStreamToken--;
   if (mStreamToken != -1)
      return 0;

   // TODO: we don't really need to close and reopen stream if the
   // format matches; however it's kind of tricky to keep it open...
   //
   //   if (sampleRate == mRate &&
   //       playbackChannels == mNumPlaybackChannels &&
   //       captureChannels == mNumCaptureChannels &&
   //       captureFormat == mCaptureFormat) {

   #if USE_PORTAUDIO_V19
   if (mPortStreamV19) {
      StopStream();
      while(mPortStreamV19)
         wxUsleep( 50 );            
   }
   #else
   if (mPortStreamV18) {
      StopStream();
      while(mPortStreamV18)
         wxUsleep( 50 );
   }
   #endif

   gPrefs->Read("/AudioIO/SWPlaythrough", &mSoftwarePlaythrough, false);
   #ifdef __WXMAC__
   gPrefs->Read("/AudioIO/NoModifyDevice", &mNoModifyDevice, false);
   #endif

   mInputMeter = NULL;
   mOutputMeter = NULL;
   mRate    = sampleRate;
   mT0      = t0;
   mT       = t0;
   mT1      = t1;
   mPlaybackTracks = playbackTracks;
   mCaptureTracks  = captureTracks;
   mTotalSamplesPlayed = 0;
   mPlayLooped = playLooped;

#ifndef USE_PORTAUDIO_V19
   mPausedSeconds = 0;
#endif

   //
   // The RingBuffer sizes, and the max amount of the buffer to
   // fill at a time, both grow linearly with the number of
   // tracks.  This allows us to scale up to many tracks without
   // killing performance.
   //

   mPlaybackRingBufferSecs = 4.5 + (0.5 * mPlaybackTracks.GetCount());
   mMaxPlaybackSecsToCopy = 0.75 + (0.25 * mPlaybackTracks.GetCount());

   mCaptureRingBufferSecs = 4.5 + 0.5 * mCaptureTracks.GetCount();   
   mMinCaptureSecsToCopy = 0.2 + (0.2 * mCaptureTracks.GetCount());

   unsigned int playbackChannels = 0;
   unsigned int captureChannels = 0;
   sampleFormat captureFormat = floatSample;

   if( playbackTracks.GetCount() > 0 )
      playbackChannels = 2;

   if (mSoftwarePlaythrough)
      playbackChannels = 2;

   if( captureTracks.GetCount() > 0 )
   {
      // For capture, every input channel gets its own track
      captureChannels = mCaptureTracks.GetCount();
      // I don't deal with the possibility of the capture tracks
      // having different sample formats, since it will never happen
      // with the current code.  This code wouldn't *break* if this
      // assumption was false, but it would be sub-optimal.  For example,
      // if the first track was 16-bit and the second track was 24-bit,
      // we would set the sound card to capture in 16 bits and the second
      // track wouldn't get the benefit of all 24 bits the card is capable
      // of.
      captureFormat = mCaptureTracks[0]->GetSampleFormat();
   }   

   bool success = StartPortAudioStream(sampleRate, playbackChannels,
                                       captureChannels, captureFormat);
   
   if (!success) {
      mStreamToken = 0;
      return 0;
   }

   //
   // The stream has successfully been opened.  We now proceed in allocating
   // the memory structures the stream will need.
   //

   if( mNumPlaybackChannels > 0 )
   {
      // Allocate output buffers.  For every output track we allocate
      // a ring buffer of five seconds
      sampleCount playbackBufferSize =
         (sampleCount)(mRate * mPlaybackRingBufferSecs + 0.5);
      sampleCount playbackMixBufferSize = 
         (sampleCount)(mRate * mMaxPlaybackSecsToCopy + 0.5);
      mPlaybackBuffers = new RingBuffer* [mPlaybackTracks.GetCount()];
      mPlaybackMixers  = new Mixer*      [mPlaybackTracks.GetCount()];

      for( unsigned int i = 0; i < mPlaybackTracks.GetCount(); i++ )
      {
         mPlaybackBuffers[i] = new RingBuffer(floatSample, playbackBufferSize);

         mPlaybackMixers[i]  = new Mixer(1, &mPlaybackTracks[i],
                                         timeTrack, mT0, mT1, 1,
                                         playbackMixBufferSize, false,
                                         mRate, floatSample, false);
         mPlaybackMixers[i]->ApplyTrackGains(false);
      }
   }

   if( mNumCaptureChannels > 0 )
   {
      // Allocate input buffers.  For every input track we allocate
      // a ring buffer of five seconds
      sampleCount captureBufferSize =
         (sampleCount)(mRate * mCaptureRingBufferSecs + 0.5);
      mCaptureBuffers = new RingBuffer* [mCaptureTracks.GetCount()];

      for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
         mCaptureBuffers[i] = new RingBuffer( mCaptureTracks[i]->GetSampleFormat(),
                                              captureBufferSize );
   }

   // We signal the audio thread to call FillBuffers, to prime the RingBuffers
   // so that they will have data in them when the stream starts.  Having the
   // audio thread call FillBuffers here makes the code more predictable, since
   // FillBuffers will ALWAYS get called from the Audio thread.
   mAudioThreadShouldCallFillBuffersOnce = true;

   while( mAudioThreadShouldCallFillBuffersOnce == true )
      wxUsleep( 50 );

   // Now start the PortAudio stream!
   PaError err;
#if USE_PORTAUDIO_V19
   err = Pa_StartStream( mPortStreamV19 );
#else
   err = Pa_StartStream( mPortStreamV18 );
#endif

   if( err != paNoError )
   {
      // TODO
      // we'll need a more complete way to indicate error.
      // AND we need to delete the ring buffers and mixers, etc.
      printf("%s\n", Pa_GetErrorText(err));
      mStreamToken = 0;
      return 0;
   }

   mAudioThreadFillBuffersLoopRunning = true;

   //
   // Generate an unique value each time, to be returned to
   // clients accessing the AudioIO API, so they can query if
   // are the ones who have reserved AudioIO or not.
   //
   mStreamToken = (++mNextStreamToken);

#if USE_PORTAUDIO_V19
   // To make GetStreamTime behave correctly before the callback sets
   // mLastBufferAudibleTime the first time, we make a rough guess
   // TODO: guess better
   mLastBufferAudibleTime = Pa_GetStreamTime( mPortStreamV19 ) + (2048 / mRate);
#endif

   return mStreamToken;
}

void AudioIO::SetMeters(Meter *inputMeter, Meter *outputMeter)
{
   mInputMeter = inputMeter;
   mOutputMeter = outputMeter;

   if (mInputMeter)
      mInputMeter->Reset(mRate, true);
   if (mOutputMeter)
      mOutputMeter->Reset(mRate, true);

   mUpdateMeters = true;
}

void AudioIO::StopStream()
{
 #if USE_PORTAUDIO_V19
   if( mPortStreamV19 == NULL )
      return;

   if( Pa_IsStreamStopped( mPortStreamV19 ) )
      return;
 #else
   if( mPortStreamV18 == NULL )
      return;

   if( IsStreamActive() == false && mInCallbackFinishedState == false )
      return;
 #endif

   // Avoid race condition by making sure this function only
   // gets called once at a time
   mStopStreamCount++;
   if (mStopStreamCount != 1)
      return;

   //
   // We got here in one of two ways:
   //
   // 1. The user clicked the stop button and we therefore want to stop
   //    as quickly as possible.  So we use AbortStream().  If this is
   //    the case the portaudio stream is still in the Running state
   //    (see PortAudio state machine docs).
   //
   // 2. The callback told PortAudio to stop the stream since it had
   //    reached the end of the selection.  The UI thread discovered
   //    this by noticing that AudioIO::IsActive() returned false.
   //    IsActive() (which calls Pa_GetStreamActive()) will not return
   //    false until all buffers have finished playing, so we can call
   //    AbortStream without losing any samples.  If this is the case
   //    we are in the "callback finished state" (see PortAudio state
   //    machine docs).
   //
   // The moral of the story: We can call AbortStream safely, without
   // losing samples.
   //
   // DMM: This doesn't seem to be true; it seems to be necessary to
   // call StopStream if the callback brought us here, and AbortStream
   // if the user brought us here.
   //

   mAudioThreadFillBuffersLoopRunning = false;

   // Audacity can deadlock if it tries to update meters while
   // we're stopping PortAudio (because the meter updating code
   // tries to grab a UI mutex while PortAudio tries to join a
   // pthread).  So we tell the callback to stop updating meters,
   // and wait until the callback has left this part of the code
   // if it was already there.
   mUpdateMeters = false;
   while(mUpdatingMeters) {
      wxYield();
      wxUsleep( 50 );
   }

   // Turn off HW playthrough if PortMixer is being used

  #if defined(USE_PORTMIXER)
   if( mPortMixer ) {
      #if __WXMAC__
      if (Px_SupportsPlaythrough(mPortMixer) && mPreviousHWPlaythrough >= 0.0)
         Px_SetPlaythrough(mPortMixer, mPreviousHWPlaythrough);
      #endif
   }
  #endif

#if USE_PORTAUDIO_V19
   if (mPortStreamV19) {
      Pa_AbortStream( mPortStreamV19 );
      Pa_CloseStream( mPortStreamV19 );
      mPortStreamV19 = NULL;
   }
#else
   if (mPortStreamV18) {
      if (mInCallbackFinishedState)
         Pa_StopStream( mPortStreamV18 );
      else
         Pa_AbortStream( mPortStreamV18 );
      Pa_CloseStream( mPortStreamV18 );
      mPortStreamV18 = NULL;
      mInCallbackFinishedState = false;
   }
#endif

   // If there's no token, we were just monitoring, so we can
   // skip this next part...
   if (mStreamToken > 0) {
      // In either of the above cases, we want to make sure that any
      // capture data that made it into the PortAudio callback makes it
      // to the target WaveTrack.  To do this, we ask the audio thread to
      // call FillBuffers one last time (it normally would not do so since
      // Pa_GetStreamActive() would now return false
      mAudioThreadShouldCallFillBuffersOnce = true;

      while( mAudioThreadShouldCallFillBuffersOnce == true )
      {
         wxYield();
         wxUsleep( 50 );
      }

      //
      // Everything is taken care of.  Now, just free all the resources
      // we allocated in StartStream()
      //
      
      if( mPlaybackTracks.GetCount() > 0 )
      {
         for( unsigned int i = 0; i < mPlaybackTracks.GetCount(); i++ )
         {
            delete mPlaybackBuffers[i];
            delete mPlaybackMixers[i];
         }
         
         delete[] mPlaybackBuffers;
         delete[] mPlaybackMixers;
      }

      if( mCaptureTracks.GetCount() > 0 )
      {
         for( unsigned int i = 0; i < mCaptureTracks.GetCount(); i++ )
            {
               delete mCaptureBuffers[i];
               mCaptureTracks[i]->Flush();
               mCaptureTracks[i]->Offset(mLastRecordingOffset);
            }
         
         delete[] mCaptureBuffers;
      }
   }

   if (mInputMeter)
      mInputMeter->Reset(mRate, false);
   if (mOutputMeter)
      mOutputMeter->Reset(mRate, false);

   //
   // Only set token to 0 after we're totally finished with everything
   //
   mStreamToken = 0;
   mStopStreamCount = 0;
}

void AudioIO::SetPaused(bool state)
{
   if(state)
   {
      // When we are beginning a pause, we note the time so that GetStreamTime
      // can always return this position while we're paused
      mPausePosition = GetStreamTime();
   }
   else
   {
#if USE_PORTAUDIO_V19
      // When we're coming out of a pause, we guess mLastBufferAudibleTime so
      // that the indicator won't be erratic between now and when the callback
      // sets mLastBufferAudibleTime again.  This is similar to what happens
      // in StartStream
      mLastBufferAudibleTime = Pa_GetStreamTime( mPortStreamV19 ) + (2048 / mRate);
#endif
   }

   mPaused = state;
}

bool AudioIO::IsPaused()
{
   return mPaused;
}

bool AudioIO::IsBusy()
{
   // dmazzoni: Old code...now it's safe to call StartStream if
   // PortAudio is open, just not if there's a token active
 #if 0
   if (IsStreamActive())
      return true;
  #if USE_PORTAUDIO_V19
   if (mPortStreamV19)
      return true;
  #else
   if (mPortStreamV18)
      return true;
  #endif
#endif

   if (mStreamToken != 0)
      return true;

   return false;
}

bool AudioIO::IsStreamActive()
{
#if USE_PORTAUDIO_V19
   if( mPortStreamV19 )
      return Pa_IsStreamActive( mPortStreamV19 );
   else
      return false;
#else
   if( mPortStreamV18 &&
       Pa_StreamActive( mPortStreamV18 ) &&
       mInCallbackFinishedState == false )
      return true;
   else
      return false;
#endif
}

bool AudioIO::IsStreamActive(int token)
{
   if( IsStreamActive() && token > 0 && token == mStreamToken )
      return true;
   else
      return false;
}

bool AudioIO::IsAudioTokenActive(int token)
{
   return ( token > 0 && token == mStreamToken );
}

bool AudioIO::IsMonitoring()
{
#if USE_PORTAUDIO_V19
   return ( mPortStreamV19 && mStreamToken==0 );
#else
   return ( mPortStreamV18 && mStreamToken==0 );
#endif
}

double AudioIO::NormalizeStreamTime(double absoluteTime) const
{
   // dmazzoni: This function is needed for two reasons:
   // One is for looped-play mode - this function makes sure that the
   // position indicator keeps wrapping around.  The other reason is
   // more subtle - it's because PortAudio can query the hardware for
   // the current stream time, and this query is not always accurate.
   // Sometimes it's a little behind or ahead, and so this function
   // makes sure that at least we clip it to the selection.

   // msmeyer: Just to be sure, the returned stream time should
   //          never be smaller than the actual start time.
   if (absoluteTime < mT0)
      absoluteTime = mT0;

   // dmazzoni: If we're looping, we should wrap around when the
   // stream time is past the end.  Otherwise the returned stream
   // time is clipped to the end time.
   if (mPlayLooped) {
      while (absoluteTime > mT1)
         absoluteTime -= mT1 - mT0;
   }
   else {
      if (absoluteTime > mT1)
         absoluteTime = mT1;
   }
      
   return absoluteTime;
}

double AudioIO::GetStreamTime()
{
   if( !IsStreamActive() )
      return -1000000000;

   if( mPaused )
      return NormalizeStreamTime(mPausePosition);

#if USE_PORTAUDIO_V19

   PaStream *stream = mPortStreamV19;

   // Based on the number of samples we have written, this is the value
   // in time that represents how far we are through the source data
   double lastBufferTime =  mT0 + (mTotalSamplesPlayed / mRate);

   // This is the number of seconds ago that the last buffer started
   // being heard.  It will be negative if the last buffer is not
   // yet audible.
   double deltat = Pa_GetStreamTime(stream) - mLastBufferAudibleTime;

   // [JH]: I need to diagram this, but I'm pretty sure this calculation
   // is off by one buffer size
   double time = lastBufferTime + deltat;

   return NormalizeStreamTime(time);
#else
   PaStream *stream = mPortStreamV18;

   double streamTime = Pa_StreamTime(stream);
   double indicator = mT0 + (streamTime / mRate) - mPausedSeconds;

   // Pa_StreamTime can sometimes return wacky results, so we
   // try to filter those out...
   if (fabs(indicator - mLastIndicator) > 0.1) {
      mLastIndicator = indicator;
      return NormalizeStreamTime(mLastStableIndicator);
   }
   mLastIndicator = indicator;
   mLastStableIndicator = indicator;
   return NormalizeStreamTime(indicator);
#endif
}


wxArrayLong AudioIO::GetSupportedSampleRates(wxString playDevice, wxString recDevice)
{
   int numDefaultRates = 7;
   int defaultRates[] = {
      8000,
      11025,
      16000,
      22050,
      44100,
      48000,
      96000
   };

   const PaDeviceInfo* playInfo = NULL;
   const PaDeviceInfo* recInfo = NULL;

   if (playDevice.IsEmpty())
      playDevice = gPrefs->Read("/AudioIO/PlaybackDevice", "");
   if (recDevice.IsEmpty())
      recDevice = gPrefs->Read("/AudioIO/RecordingDevice", "");

   int i;

   // msmeyer: Find info structs for playing/recording devices
#if USE_PORTAUDIO_V19
   for (i = 0; i < Pa_GetDeviceCount(); i++) {
#else
   for (i = 0; i < Pa_CountDevices(); i++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(i);

      if (info->name == playDevice && info->maxOutputChannels > 0)
         playInfo = info;
      if (info->name == recDevice && info->maxInputChannels > 0)
         recInfo = info;
   }

   // msmeyer: Check which sample rates the play device supports
   wxArrayLong playSampleRates;

   if (playInfo)
   {
#if USE_PORTAUDIO_V19
      // TODO: implement using IsFormatSupported()
      for (i = 0; i < numDefaultRates; i++)
         playSampleRates.Add(defaultRates[i]);
#else
      if (playInfo->numSampleRates == -1)
      {
         for (i = 0; i < numDefaultRates; i++)
            if (defaultRates[i] >= playInfo->sampleRates[0] &&
                defaultRates[i] <= playInfo->sampleRates[1])
               playSampleRates.Add(defaultRates[i]);
      } else
      {
         for (i = 0; i < playInfo->numSampleRates; i++)
            playSampleRates.Add((int)playInfo->sampleRates[i]);
      }
#endif
   }

   if (playSampleRates.IsEmpty())
   {
      for (i = 0; i < numDefaultRates; i++)
         playSampleRates.Add(defaultRates[i]);
   }

   // msmeyer: Check which sample rates the record device supports
   wxArrayLong recSampleRates;

   if (recInfo)
   {
#if USE_PORTAUDIO_V19
      // TODO: implement using IsFormatSupported()
      for (i = 0; i < numDefaultRates; i++)
         recSampleRates.Add(defaultRates[i]);
#else
      if (recInfo->numSampleRates == -1)
      {
         for (i = 0; i < numDefaultRates; i++)
            if (defaultRates[i] >= recInfo->sampleRates[0] &&
                defaultRates[i] <= recInfo->sampleRates[1])
               recSampleRates.Add(defaultRates[i]);
      } else
      {
         for (i = 0; i < recInfo->numSampleRates; i++)
            recSampleRates.Add((int)recInfo->sampleRates[i]);
      }
#endif
   }

   if (recSampleRates.IsEmpty())
   {
      for (i = 0; i < numDefaultRates; i++)
         recSampleRates.Add(defaultRates[i]);
   }

   // Return only sample rates which are in both arrays
   wxArrayLong result;

   for (i = 0; i < (int)playSampleRates.GetCount(); i++)
      if (recSampleRates.Index(playSampleRates[i]) != wxNOT_FOUND)
         result.Add(playSampleRates[i]);

   // If this yields no results, use the default sample rates nevertheless
   if (result.IsEmpty())
   {
      for (i = 0; i < numDefaultRates; i++)
         result.Add(defaultRates[i]);
   }

   return result;
}


int AudioIO::GetOptimalSupportedSampleRate()
{
   wxArrayLong rates = GetSupportedSampleRates();

   if (rates.Index(44100) != wxNOT_FOUND)
      return 44100;

   if (rates.Index(48000) != wxNOT_FOUND)
      return 48000;

   return rates[rates.GetCount() - 1];
}



//////////////////////////////////////////////////////////////////////
//
//     Audio Thread Context
//
//////////////////////////////////////////////////////////////////////

AudioThread::ExitCode AudioThread::Entry()
{
   while( !TestDestroy() )
   {
      if( gAudioIO->mAudioThreadShouldCallFillBuffersOnce )
      {
         gAudioIO->FillBuffers();
         gAudioIO->mAudioThreadShouldCallFillBuffersOnce = false;
      }
      else if( gAudioIO->mAudioThreadFillBuffersLoopRunning )
      {
         gAudioIO->FillBuffers();
      }

      Sleep(10);
   }

   return 0;
}

int AudioIO::GetCommonlyAvailPlayback()
{
   int commonlyAvail = mPlaybackBuffers[0]->AvailForPut();
   unsigned int i;

   for( i = 1; i < mPlaybackTracks.GetCount(); i++ )
   {
      int thisBlockAvail = mPlaybackBuffers[i]->AvailForPut();

      if( thisBlockAvail < commonlyAvail )
         commonlyAvail = thisBlockAvail;
   }

   return commonlyAvail;
}

int AudioIO::GetCommonlyAvailCapture()
{
   int commonlyAvail = mCaptureBuffers[0]->AvailForGet();
   unsigned int i;

   for( i = 1; i < mCaptureTracks.GetCount(); i++ )
   {
      int avail = mCaptureBuffers[i]->AvailForGet();
      if( avail < commonlyAvail )
         commonlyAvail = avail;
   }

   return commonlyAvail;
}

// This method is the data gateway between the audio thread (which
// communicates with the disk) and the PortAudio callback thread
// (which communicates with the audio device.
void AudioIO::FillBuffers()
{
   unsigned int i;

   if( mPlaybackTracks.GetCount() > 0 )
   {
      // Though extremely unlikely, it is possible that some buffers
      // will have more samples available than others.  This could happen
      // if we hit this code during the PortAudio callback.  To keep
      // things simple, we only write as much data as is vacant in
      // ALL buffers, and advance the global time by that much.
      int commonlyAvail = GetCommonlyAvailPlayback();

      //
      // Determine how much this will globally advance playback time
      //
      double secsAvail = commonlyAvail / mRate;

      //
      // Don't fill the buffers at all unless we can do the
      // full mMaxPlaybackSecsToCopy.  This improves performance
      // by not always trying to process tiny chunks, eating the
      // CPU unnecessarily.
      //
      // The exception is if we're at the end of the selected
      // region - then we should just fill the buffer.
      //
      if (secsAvail >= mMaxPlaybackSecsToCopy ||
          (!mPlayLooped && (secsAvail > 0 && mT+secsAvail >= mT1)))
      {
         // Limit maximum buffer size (increases performance)
         if (secsAvail > mMaxPlaybackSecsToCopy)
            secsAvail = mMaxPlaybackSecsToCopy;

         double deltat;

         // msmeyer: When playing a very short selection in looped
         // mode, the selection must be copied to the buffer multiple
         // times, to ensure, that the buffer has a reasonable size
         // This is the purpose of this loop.
         do {
            deltat = secsAvail;

            if( mT + deltat > mT1 )
            {
               deltat = mT1 - mT;
               if( deltat < 0.0 )
                  deltat = 0.0;
            }
            mT += deltat;
            secsAvail -= deltat;

            for( i = 0; i < mPlaybackTracks.GetCount(); i++ )
            {
               // The mixer here isn't actually mixing: it's just doing
               // resampling, format conversion, and possibly time track
               // warping
               int processed =
                  mPlaybackMixers[i]->Process((int)(deltat * mRate + 0.5));
               samplePtr warpedSamples = mPlaybackMixers[i]->GetBuffer();
               mPlaybackBuffers[i]->Put(warpedSamples, floatSample, processed);
            }

            // msmeyer: If playing looped, check if we are at the end of the buffer
            // and if yes, restart from the beginning.
            if (mPlayLooped && mT >= mT1)
            {
               for (i = 0; i < mPlaybackTracks.GetCount(); i++)
                  mPlaybackMixers[i]->Restart();
               mT = mT0;
            }
         } while (mPlayLooped && secsAvail > 0 && deltat > 0);
      }
   }

   if( mCaptureTracks.GetCount() > 0 )
   {
      int commonlyAvail = GetCommonlyAvailCapture();

      //
      // Determine how much this will add to captured tracks
      //
      double deltat = commonlyAvail / mRate;

      if (mAudioThreadShouldCallFillBuffersOnce ||
          deltat >= mMinCaptureSecsToCopy)
      {
         // Append captured samples to the end of the WaveTracks.
         // The WaveTracks have their own buffering for efficiency.
         for( i = 0; i < mCaptureTracks.GetCount(); i++ )
         {
            int avail = commonlyAvail;
            sampleFormat trackFormat = mCaptureTracks[i]->GetSampleFormat();
            samplePtr temp = NewSamples(avail, trackFormat);

            mCaptureBuffers[i]->Get   (temp, trackFormat, avail);
            mCaptureTracks[i]-> Append(temp, trackFormat, avail);

            DeleteSamples(temp);
         }
      }
   }
}

//////////////////////////////////////////////////////////////////////
//
//    PortAudio callback thread context
//
//////////////////////////////////////////////////////////////////////

#define MAX(a,b) ((a) > (b) ? (a) : (b))

void DoSoftwarePlaythrough(const void *inputBuffer,
                           sampleFormat inputFormat,
                           int inputChannels,
                           float *outputBuffer,
                           int len,
                           float gain)
{
   float *tempBuffer = (float *)alloca(len * sizeof(float));
   int i, j;

   for(j=0; j<inputChannels; j++) {
      samplePtr inputPtr = ((samplePtr)inputBuffer) + (j * SAMPLE_SIZE(inputFormat));
      
      CopySamples(inputPtr, inputFormat,
                  (samplePtr)tempBuffer, floatSample,
                  len, true, inputChannels);

      for(i=0; i<len; i++)
         outputBuffer[2*i + (j%2)] = tempBuffer[i];

      // One mono input channel goes to both output channels...
      if (inputChannels == 1)
         for(i=0; i<len; i++)
            outputBuffer[2*i + 1] = tempBuffer[i];
   }
   
}

#if USE_PORTAUDIO_V19
int audacityAudioCallback(const void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          const PaStreamCallbackTimeInfo *timeInfo,
                          const PaStreamCallbackFlags statusFlags, void *userData )
#else

#define paContinue 0

int audacityAudioCallback(void *inputBuffer, void *outputBuffer,
                          unsigned long framesPerBuffer,
                          PaTimestamp outTime, void *userData )
#endif
{
   int numPlaybackChannels = gAudioIO->mNumPlaybackChannels;
   int numPlaybackTracks = gAudioIO->mPlaybackTracks.GetCount();
   int numCaptureChannels = gAudioIO->mNumCaptureChannels;
   int callbackReturn = paContinue;
   void *tempBuffer = alloca(framesPerBuffer*sizeof(float)*
                             MAX(numCaptureChannels,numPlaybackChannels));
   float *tempFloats = (float*)tempBuffer;
   unsigned int i;
   int t;

   if( gAudioIO->mPaused )
   {
      if (outputBuffer && numPlaybackChannels > 0)
      {
         ClearSamples((samplePtr)outputBuffer, floatSample,
                      0, framesPerBuffer * numPlaybackChannels);

         if (inputBuffer && gAudioIO->mSoftwarePlaythrough) {
            float gain = 1.0;
            if (gAudioIO->mEmulateMixerInputVol)
               gain = gAudioIO->mMixerInputVol;

            DoSoftwarePlaythrough(inputBuffer, gAudioIO->mCaptureFormat,
                                  numCaptureChannels,
                                  (float *)outputBuffer, (int)framesPerBuffer, gain);
         }
      }

#ifndef USE_PORTAUDIO_V19
      gAudioIO->mPausedSeconds += (float)framesPerBuffer / gAudioIO->mRate;
#endif
      return paContinue;
   }

   if (gAudioIO->mStreamToken > 0)
   {

      //
      // Mix and copy to PortAudio's output buffer
      //
      
      if( outputBuffer && (numPlaybackChannels > 0) )
      {
         bool cut = false;
         bool linkFlag = false;
         
         float *outputFloats = (float *)outputBuffer;
         for( i = 0; i < framesPerBuffer*numPlaybackChannels; i++)
            outputFloats[i] = 0.0;

         if (inputBuffer && gAudioIO->mSoftwarePlaythrough) {
            float gain = 1.0;
            if (gAudioIO->mEmulateMixerInputVol)
               gain = gAudioIO->mMixerInputVol;

            DoSoftwarePlaythrough(inputBuffer, gAudioIO->mCaptureFormat,
                                  numCaptureChannels,
                                  (float *)outputBuffer, (int)framesPerBuffer, gain);
         }
         
         int numSolo = 0;
         for( t = 0; t < numPlaybackTracks; t++ )
            if( gAudioIO->mPlaybackTracks[t]->GetSolo() )
               numSolo++;
         
         for( t = 0; t < numPlaybackTracks; t++)
         {
            WaveTrack *vt = gAudioIO->mPlaybackTracks[t];
            
            if (linkFlag)
               linkFlag = false;
            else {
               cut = false;
               
               // Cut if somebody else is soloing
               if (numSolo>0 && !vt->GetSolo())
                  cut = true;
               
               // Cut if we're muted (unless we're soloing)
               if (vt->GetMute() && !vt->GetSolo())
                  cut = true;
               
               linkFlag = vt->GetLinked();
            }
            
            if (cut)
               {
                  gAudioIO->mPlaybackBuffers[t]->Discard(framesPerBuffer);
                  continue;
               }
            
            unsigned int len = (unsigned int)
               gAudioIO->mPlaybackBuffers[t]->Get((samplePtr)tempFloats,
                                                  floatSample,
                                                  (int)framesPerBuffer);

            // If our buffer is empty and the time indicator is past
            // the end, then we've actually finished playing the entire
            // selection.
            // msmeyer: We never finish if we are playing looped
            if (len == 0 && gAudioIO->mT >= gAudioIO->mT1 &&
                !gAudioIO->mPlayLooped)
            {
             #if USE_PORTAUDIO_V19
               callbackReturn = paComplete;
             #else
               callbackReturn = 1;
               gAudioIO->mInCallbackFinishedState = true;
             #endif
            }

            if (vt->GetChannel() == Track::LeftChannel ||
                vt->GetChannel() == Track::MonoChannel)
            {
               float gain = vt->GetChannelGain(0);
               
               if (gAudioIO->mEmulateMixerOutputVol)
                  gain *= gAudioIO->mMixerOutputVol;
               
               for(i=0; i<len; i++)
                  outputFloats[numPlaybackChannels*i] += gain*tempFloats[i];
            }
            
            if (vt->GetChannel() == Track::RightChannel ||
                vt->GetChannel() == Track::MonoChannel)
            {
               float gain = vt->GetChannelGain(1);
               
               if (gAudioIO->mEmulateMixerOutputVol)
                  gain *= gAudioIO->mMixerOutputVol;
               
               for(i=0; i<len; i++)
                  outputFloats[numPlaybackChannels*i+1] += gain*tempFloats[i];
            }
         }
         
         //
         // Clip output to [-1.0,+1.0] range (msmeyer)
         //
         for( i = 0; i < framesPerBuffer*numPlaybackChannels; i++)
         {
            float f = outputFloats[i];
            if (f > 1.0)
               outputFloats[i] = 1.0;
            else if (f < -1.0)
               outputFloats[i] = -1.0;
         }
      }

      //
      // Copy from PortAudio to our input buffers.
      //
      
      if( inputBuffer && (numCaptureChannels > 0) )
      {
         unsigned int len = framesPerBuffer;
         for( t = 0; t < numCaptureChannels; t++) {
            unsigned int avail =
               (unsigned int)gAudioIO->mCaptureBuffers[t]->AvailForPut();
            if (avail < len)
               len = avail;
         }
         
         if (len < framesPerBuffer)
         {
            gAudioIO->mLostSamples += (framesPerBuffer - len);
            printf("lost %d samples\n", (int)(framesPerBuffer - len));
         }

         float gain = 1.0;
         
         if (gAudioIO->mEmulateMixerInputVol)
            gain = gAudioIO->mMixerInputVol;

         if (len > 0) {
            for( t = 0; t < numCaptureChannels; t++) {
               
               // dmazzoni:
               // Un-interleave.  Ugly special-case code required because the
               // capture channels could be in three different sample formats;
               // it'd be nice to be able to call CopySamples, but it can't
               // handle multiplying by the gain and then clipping.  Bummer.

               switch(gAudioIO->mCaptureFormat) {
               case floatSample: {
                  float *inputFloats = (float *)inputBuffer;
                  for( i = 0; i < len; i++)
                     tempFloats[i] =
                        inputFloats[numCaptureChannels*i+t] * gain;
               } break;
               case int24Sample: {
                  int *inputInts = (int *)inputBuffer;
                  int *tempInts = (int *)tempBuffer;
                  for( i = 0; i < len; i++) {
                     float tmp = inputInts[numCaptureChannels*i+t] * gain;
                     if (tmp > 8388607)
                        tmp = 8388607;
                     if (tmp < -8388608)
                        tmp = -8388608;
                     tempInts[i] = (int)(tmp);
                  }
               } break;
               case int16Sample: {
                  short *inputShorts = (short *)inputBuffer;
                  short *tempShorts = (short *)tempBuffer;
                  for( i = 0; i < len; i++) {
                     float tmp = inputShorts[numCaptureChannels*i+t] * gain;
                     if (tmp > 32767)
                        tmp = 32767;
                     if (tmp < -32768)
                        tmp = -32768;
                     tempShorts[i] = (short)(tmp);
                  }
               } break;
               } // switch
               
               gAudioIO->mCaptureBuffers[t]->Put((samplePtr)tempBuffer,
                                                 gAudioIO->mCaptureFormat,
                                                 len);
            }
         }
      }
      
     #if USE_PORTAUDIO_V19
      gAudioIO->mTotalSamplesPlayed += framesPerBuffer;
      if (numPlaybackChannels > 0)
         gAudioIO->mLastBufferAudibleTime = timeInfo->outputBufferDacTime;
      else
         gAudioIO->mLastBufferAudibleTime = timeInfo->inputBufferAdcTime;
     #endif

      // Record the reported latency from PortAudio.
      // TODO: Don't recalculate this with every callback?
     #if USE_PORTAUDIO_V19
      if (numCaptureChannels > 0 && numPlaybackChannels > 0)
         gAudioIO->mLastRecordingOffset = timeInfo->inputBufferAdcTime - timeInfo->outputBufferDacTime;
      else
         gAudioIO->mLastRecordingOffset = 0;
     #else
      if (numCaptureChannels > 0 && numPlaybackChannels > 0)
         gAudioIO->mLastRecordingOffset = (Pa_StreamTime(gAudioIO->mPortStreamV18) - outTime) / gAudioIO->mRate;
      else
         gAudioIO->mLastRecordingOffset = 0;
     #endif

   } // if mStreamToken > 0
   else {
      // No tracks to play, but we should clear the output, and
      // possibly do software playthrough...
      
      if( outputBuffer && (numPlaybackChannels > 0) ) {
         float *outputFloats = (float *)outputBuffer;
         for( i = 0; i < framesPerBuffer*numPlaybackChannels; i++)
            outputFloats[i] = 0.0;
         
         if (inputBuffer && gAudioIO->mSoftwarePlaythrough) {
            float gain = 1.0;
            if (gAudioIO->mEmulateMixerInputVol)
               gain = gAudioIO->mMixerInputVol;
            
            DoSoftwarePlaythrough(inputBuffer, gAudioIO->mCaptureFormat,
                                  numCaptureChannels,
                                  (float *)outputBuffer, (int)framesPerBuffer, gain);
         }
      }
   }

   // Send data to VU meters

   // It's critical that we don't update the meters while
   // StopStream is trying to stop PortAudio, otherwise it can
   // lead to a freeze.  We use two variables to synchronize:
   // mUpdatingMeters tells StopStream when the callback is about
   // to enter the code where it might update the meters, and
   // mUpdateMeters is how the rest of the code tells the callback
   // when it is allowed to actually do the updating.  Note that
   // mUpdatingMeters must be set first to avoid a race condition.
   gAudioIO->mUpdatingMeters = true;
   if (gAudioIO->mUpdateMeters) {

      if (gAudioIO->mOutputMeter && outputBuffer)
         gAudioIO->mOutputMeter->UpdateDisplay(numPlaybackChannels,
                                               framesPerBuffer,
                                               (float *)outputBuffer);
      if (gAudioIO->mInputMeter && inputBuffer) {
         if (gAudioIO->mCaptureFormat == floatSample)
            gAudioIO->mInputMeter->UpdateDisplay(numCaptureChannels,
                                                 framesPerBuffer,
                                                 (float *)inputBuffer);
         else {
            CopySamples((samplePtr)inputBuffer, gAudioIO->mCaptureFormat,
                        (samplePtr)tempFloats, floatSample,
                        framesPerBuffer * numCaptureChannels);
            gAudioIO->mInputMeter->UpdateDisplay(numCaptureChannels,
                                                 framesPerBuffer,
                                                 tempFloats);
         }
      }
   }
   gAudioIO->mUpdatingMeters = false;

   return callbackReturn;
}

