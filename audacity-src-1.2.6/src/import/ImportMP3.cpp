/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3.cpp

  Joshua Haberman

  Audacity has finally moved to using a single mp3 library on all
  platforms! It is the high performance, beautifully written libmad
  (mpeg audio decoder). Finally there is harmony in the mp3 universe.

  Much of this source code is based on 'minimad.c' as distributed
  with libmad.

**********************************************************************/

#include <wx/defs.h>
#include "../Audacity.h"

#include "Import.h"
#include "ImportMP3.h"
#include "ImportPlugin.h"
#include "../Internat.h"

#ifndef USE_LIBMAD

void GetMP3ImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   UnusableImportPlugin* mp3IsUnsupported =
      new UnusableImportPlugin("MP3",
                               wxStringList("mp3", "mp2", "mpg", "mpeg", NULL));

   unusableImportPluginList->Append(mp3IsUnsupported);
}

#else /* USE_LIBMAD */

#include <wx/textctrl.h>
#include <wx/file.h>
#include <wx/thread.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/timer.h>
#include <wx/intl.h>

extern "C" {
#include "mad.h"
}

#include "../WaveTrack.h"

#define INPUT_BUFFER_SIZE 65535
#define PROGRESS_SCALING_FACTOR 100000

/* this is a private structure we can use for whatever we like, and it will get
 * passed to each of the callback routines, allowing us to keep track of
 * things. */
struct private_data {
   wxFile *file;            /* the file containing the mp3 data we're feeding the encoder */
   unsigned char *inputBuffer;
   TrackFactory *trackFactory;
   WaveTrack **channels;
   int numChannels;
   progress_callback_t progressCallback;
   void *userData;
   bool cancelled;
};

class MP3ImportPlugin : public ImportPlugin
{
public:
   MP3ImportPlugin():
      ImportPlugin(wxStringList("mp3", "mp2", "mpg", "mpeg", NULL))
   {
   }

   ~MP3ImportPlugin() { }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};

class MP3ImportFileHandle : public ImportFileHandle
{
public:
   MP3ImportFileHandle(wxFile *file):
      mFile(file),
      mUserData(NULL)
   {
      mPrivateData.progressCallback = NULL;
   }

   ~MP3ImportFileHandle();

   void SetProgressCallback(progress_callback_t function,
                            void *userData);
   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   bool Import(TrackFactory *trackFactory, Track ***outTracks,
               int *outNumTracks);
private:
   wxFile *mFile;
   void *mUserData;
   struct private_data mPrivateData;
   mad_decoder mDecoder;
};

void GetMP3ImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new MP3ImportPlugin);
}

/* The MAD callbacks */
enum mad_flow input_cb(void *_data, struct mad_stream *stream);
enum mad_flow output_cb(void *_data,
                        struct mad_header const *header,
                        struct mad_pcm *pcm);
enum mad_flow error_cb(void *_data, struct mad_stream *stream,
                       struct mad_frame *frame);

/* convert libmad's fixed point representation to 16 bit signed integers. This
 * code is taken verbatim from minimad.c. */

inline float scale(mad_fixed_t sample)
{
   return (float) (sample / (float) (1L << MAD_F_FRACBITS));
}


wxString MP3ImportPlugin::GetPluginFormatDescription()
{
   return "MP3";
}

ImportFileHandle *MP3ImportPlugin::Open(wxString Filename)
{
   wxFile *file = new wxFile((const char *)FILENAME(Filename));

   if (!file->IsOpened()) {
      delete file;
      return NULL;
   }

   /* There's no way to tell if this is a valid mp3 file before actually
    * decoding, so we return a valid FileHandle. */

   return new MP3ImportFileHandle(file);
}


void MP3ImportFileHandle::SetProgressCallback(progress_callback_t function,
                                          void *userData)
{
   mPrivateData.progressCallback = function;
   mPrivateData.userData = userData;
}

wxString MP3ImportFileHandle::GetFileDescription()
{
   return "MP3";
}

int MP3ImportFileHandle::GetFileUncompressedBytes()
{
   // TODO
   return 0;
}

bool MP3ImportFileHandle::Import(TrackFactory *trackFactory, Track ***outTracks,
                                 int *outNumTracks)
{
   int chn;

   /* Prepare decoder data, initialize decoder */

   mPrivateData.file        = mFile;
   mPrivateData.inputBuffer = new unsigned char [INPUT_BUFFER_SIZE];
   mPrivateData.channels   = NULL;
   mPrivateData.cancelled   = false;
   mPrivateData.numChannels = 0;
   mPrivateData.trackFactory= trackFactory;

   mad_decoder_init(&mDecoder, &mPrivateData, input_cb, 0, 0, output_cb, error_cb, 0);

   /* and send the decoder on its way! */

   if(mad_decoder_run(&mDecoder, MAD_DECODER_MODE_SYNC) == 0)
   {
      /* success */
      /* printf("success\n"); */

      mad_decoder_finish(&mDecoder);

      /* copy the WaveTrack pointers into the Track pointer list that
       * we are expected to fill */
      *outTracks = new Track* [mPrivateData.numChannels];
      for(chn = 0; chn < mPrivateData.numChannels; chn++) {
         mPrivateData.channels[chn]->Flush();
         (*outTracks)[chn] = mPrivateData.channels[chn];
      }
      *outNumTracks = mPrivateData.numChannels;

      delete mPrivateData.inputBuffer;
      delete[] mPrivateData.channels;

      return true;
   }
   else {

      /* failure */
      /* printf("failure\n"); */

      mad_decoder_finish(&mDecoder);

      /* delete everything */
      for(chn = 0; chn < mPrivateData.numChannels; chn++)
         delete mPrivateData.channels[chn];

      delete[] mPrivateData.channels;
      delete mPrivateData.inputBuffer;

      return false;
   }
}

MP3ImportFileHandle::~MP3ImportFileHandle()
{
   if(mFile) {
      if (mFile->IsOpened())
         mFile->Close();
      delete mFile;
   }
}

//
//   MAD Callbacks
//

/* The input callback is called when the decoder wants more data. */

enum mad_flow input_cb(void *_data, struct mad_stream *stream)
{
   struct private_data *data = (struct private_data *)_data;

   if(data->progressCallback) {
      data->cancelled = data->progressCallback(data->userData,
                                               (float)data->file->Tell() /
                                               data->file->Length());
      if(data->cancelled)
         return MAD_FLOW_STOP;
   }

   if(data->file->Eof()) {
      data->cancelled = false;
      return MAD_FLOW_STOP;
   }

   /* "Each time you refill your buffer, you need to preserve the data in
    *  your existing buffer from stream.next_frame to the end.
    *
    *  This usually amounts to calling memmove() on this unconsumed portion
    *  of the buffer and appending new data after it, before calling
    *  mad_stream_buffer()"
    *           -- Rob Leslie, on the mad-dev mailing list */

   unsigned int unconsumedBytes;
   if(stream->next_frame) {
      unconsumedBytes = data->inputBuffer + INPUT_BUFFER_SIZE - stream->next_frame;
      memmove(data->inputBuffer, stream->next_frame, unconsumedBytes);
   }
   else
      unconsumedBytes = 0;


   off_t read = data->file->Read(data->inputBuffer + unconsumedBytes,
                                 INPUT_BUFFER_SIZE - unconsumedBytes);

   mad_stream_buffer(stream, data->inputBuffer, read + unconsumedBytes);

   return MAD_FLOW_CONTINUE;
}

/* The output callback is called every time the decoder has finished decoding
 * a frame, allowing us to use the decoded data */

enum mad_flow output_cb(void *_data,
                        struct mad_header const *header,
                        struct mad_pcm *pcm)
{
   int channels, samplerate;
   sampleCount samples;
   struct private_data *data = (struct private_data *)_data;
   int chn, smpl;

   samplerate= pcm->samplerate;
   channels  = pcm->channels;
   samples   = pcm->length;

   /* If this is the first run, we need to create the WaveTracks that
    * will hold the data.  We do this now because now is the first
    * moment when we know how many channels there are. */

   if(!data->channels) {
      data->channels = new WaveTrack* [channels];

      for(chn = 0; chn < channels; chn++) {
         data->channels[chn] = data->trackFactory->NewWaveTrack(floatSample);
         data->channels[chn]->SetRate(samplerate);
         data->channels[chn]->SetChannel(Track::MonoChannel);
      }

      /* special case: 2 channels is understood to be stereo */
      if(channels == 2) {
         data->channels[0]->SetChannel(Track::LeftChannel);
         data->channels[1]->SetChannel(Track::RightChannel);
         data->channels[0]->SetLinked(true);
      }
      data->numChannels = channels;
   }
   else {  
      // This is not the first run, protect us from libmad glitching
      // on the number of channels
      channels = data->numChannels;
   }

   /* TODO: get rid of this by adding fixed-point support to SampleFormat.
    * For now, we allocate temporary float buffers to convert the fixed
    * point samples into something we can feed to the WaveTrack.  Allocating
    * big blocks of data like this isn't a great idea, but it's temporary.
    */
   float **channelBuffers = new float* [channels];
   for(chn = 0; chn < channels; chn++)
      channelBuffers[chn] = new float [samples];

   for(smpl = 0; smpl < samples; smpl++)
      for(chn = 0; chn < channels; chn++)
         channelBuffers[chn][smpl] = scale(pcm->samples[chn][smpl]);

   for(chn = 0; chn < channels; chn++)
      data->channels[chn]->Append((samplePtr)channelBuffers[chn],
                                  floatSample,
                                  samples);

   for(chn = 0; chn < channels; chn++)
      delete[] channelBuffers[chn];
   delete[] channelBuffers;

   return MAD_FLOW_CONTINUE;
}

enum mad_flow error_cb(void *_data, struct mad_stream *stream, 
                       struct mad_frame *frame)
{
/* enum mad_flow {
     MAD_FLOW_CONTINUE = 0x0000,
     MAD_FLOW_STOP     = 0x0010,
     MAD_FLOW_BREAK    = 0x0011,
     MAD_FLOW_IGNORE   = 0x0020
   }; */
   /*
   printf("decoding error 0x%04x (%s)\n",
      stream->error, mad_stream_errorstr(stream));
   */

   return MAD_FLOW_CONTINUE;

   /* return MAD_FLOW_BREAK; */
}


#endif                          /* defined(USE_LIBMAD) */

