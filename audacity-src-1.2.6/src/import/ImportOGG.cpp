/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportOGG.cpp

  Joshua Haberman

  The Ogg format supports multiple logical bitstreams that can be chained
  within the physical bitstream. The sampling rate and number of channels
  can vary between these logical bitstreams. For the moment, we'll ignore
  all but the first logical bitstream.

  Ogg also allows for an arbitrary number of channels. Luckily, so does
  Audacity. We'll call the first channel LeftChannel, the second
  RightChannel, and all others after it MonoChannel.

**********************************************************************/

#include "../Audacity.h"
#include "ImportOGG.h"
#include "../Internat.h"

#ifndef USE_LIBVORBIS
/* BPF There is no real reason to compile without LIBVORBIS, but if you do, you will needs this header */
#include "ImportPlugin.h"  

void GetOGGImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   UnusableImportPlugin* oggIsUnsupported =
      new UnusableImportPlugin("Ogg Vorbis",
                               wxStringList("ogg", NULL));

   unusableImportPluginList->Append(oggIsUnsupported);
}

#else /* USE_LIBVORBIS */

#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
/* ffile.h must be included AFTER at least one other wx header that includes
 * wx/setup.h, otherwise #ifdefs erronously collapse it to nothing. This is
 * a bug in wxWindows (ffile.h should itself include wx/setup.h), and it
 * was a bitch to track down. */
#include <wx/ffile.h>

#include <vorbis/vorbisfile.h>

#include "../WaveTrack.h"
#include "ImportPlugin.h"

class OggImportPlugin : public ImportPlugin
{
public:
   OggImportPlugin():
      ImportPlugin(wxStringList("ogg", NULL))
   {
   }

   ~OggImportPlugin() { }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};


class OggImportFileHandle : public ImportFileHandle
{
public:
   OggImportFileHandle(wxFFile *file, OggVorbis_File *vorbisFile):
      mFile(file),
      mVorbisFile(vorbisFile),
      mProgressCallback(NULL),
      mUserData(NULL)
   {
   }
   ~OggImportFileHandle();

   void SetProgressCallback(progress_callback_t function,
                            void *userData);
   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   bool Import(TrackFactory *trackFactory, Track ***outTracks,
               int *outNumTracks);
private:
   wxFFile *mFile;
   OggVorbis_File *mVorbisFile;
   progress_callback_t mProgressCallback;
   void *mUserData;
};

void GetOGGImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new OggImportPlugin);
}

wxString OggImportPlugin::GetPluginFormatDescription()
{
    return "Ogg Vorbis";
}

ImportFileHandle *OggImportPlugin::Open(wxString filename)
{
   OggVorbis_File *vorbisFile = new OggVorbis_File;
   wxFFile *file = new wxFFile(FILENAME(filename), "rb");

   if (!file->IsOpened()) {
      // No need for a message box, it's done automatically (but how?)
      delete vorbisFile;
      delete file;
      return NULL;
   }

   int err = ov_open(file->fp(), vorbisFile, NULL, 0);

   if (err < 0) {
      wxString message;

      switch (err) {
         case OV_EREAD:
            message = _("Media read error");
            break;
         case OV_ENOTVORBIS:
            message = _("Not an Ogg Vorbis file");
            break;
         case OV_EVERSION:
            message = _("Vorbis version mismatch");
            break;
         case OV_EBADHEADER:
            message = _("Invalid Vorbis bitstream header");
            break;
         case OV_EFAULT:
            message = _("Internal logic fault");
            break;
      }

      // what to do with message?
      file->Close();
      delete vorbisFile;
      delete file;
      return NULL;
   }

   return new OggImportFileHandle(file, vorbisFile);
}

void OggImportFileHandle::SetProgressCallback(progress_callback_t progressCallback,
                                      void *userData)
{
   mProgressCallback = progressCallback;
   mUserData = userData;
}

wxString OggImportFileHandle::GetFileDescription()
{
   return "Ogg Vorbis";
}

int OggImportFileHandle::GetFileUncompressedBytes()
{
   // TODO:
   return 0;
}

bool OggImportFileHandle::Import(TrackFactory *trackFactory, Track ***outTracks,
                                 int *outNumTracks)
{
   wxASSERT(mFile->IsOpened());

   /* -1 is for the current logical bitstream */
   vorbis_info *vi = ov_info(mVorbisFile, -1);

   *outNumTracks = vi->channels;
   WaveTrack **channels = new WaveTrack *[*outNumTracks];

   int c;
   for (c = 0; c < *outNumTracks; c++) {
      channels[c] = trackFactory->NewWaveTrack(int16Sample);
      channels[c]->SetRate(vi->rate);

      if (*outNumTracks > 1)
         switch (c) {
         case 0:
            channels[c]->SetChannel(Track::LeftChannel);
            break;
         case 1:
            channels[c]->SetChannel(Track::RightChannel);
            break;
         default:
            channels[c]->SetChannel(Track::MonoChannel);
         }
   }

   if (*outNumTracks == 2)
      channels[0]->SetLinked(true);

/* The number of bytes to get from the codec in each run */
#define CODEC_TRANSFER_SIZE 4096

/* The number of samples to read between calls to the callback.
 * Balance between responsiveness of the GUI and throughput of import. */
#define SAMPLES_PER_CALLBACK 100000

   short *mainBuffer = new short[CODEC_TRANSFER_SIZE];

   /* determine endianness (clever trick courtesy of Nicholas Devillard,
    * (http://www.eso.org/~ndevilla/endian/) */
   int testvar = 1, endian;
   if(*(char *)&testvar)
      endian = 0;  // little endian
   else
      endian = 1;  // big endian

   /* number of samples currently in each channel's buffer */
   bool cancelled = false;
   long bytesRead = 0;
   long samplesRead = 0;
   int bitstream = 0;
   int samplesSinceLastCallback = 0;

   // You would think that the stream would already be seeked to 0, and
   // indeed it is if the file is legit.  But I had several ogg files on
   // my hard drive that have malformed headers, and this added call
   // causes them to be read correctly.  Otherwise they have lots of
   // zeros inserted at the beginning
   ov_pcm_seek(mVorbisFile, 0);

   do {
      /* get data from the decoder */
      bytesRead = ov_read(mVorbisFile, (char *) mainBuffer,
                          CODEC_TRANSFER_SIZE,
                          endian,
                          2,    // word length (2 for 16 bit samples)
                          1,    // signed
                          &bitstream);

      if (bytesRead < 0) {
         /* Malformed Ogg Vorbis file. */
         /* TODO: Return some sort of meaningful error. */
         break;
      }

      samplesRead = bytesRead / *outNumTracks / sizeof(short);

      /* give the data to the wavetracks */
      for(c = 0; c < *outNumTracks; c++)
          channels[c]->Append((char *)(mainBuffer + c),
                              int16Sample,
                              samplesRead,
                              *outNumTracks);

      samplesSinceLastCallback += samplesRead;
      if( samplesSinceLastCallback > SAMPLES_PER_CALLBACK )
      {
          if( mProgressCallback )
             cancelled = mProgressCallback(mUserData,
                                           ov_time_tell(mVorbisFile) /
                                           ov_time_total(mVorbisFile, bitstream));
          samplesSinceLastCallback -= SAMPLES_PER_CALLBACK;
      }

   } while (!cancelled && bytesRead != 0 && bitstream == 0);

   /* Clear out the (partially-full) buffer. */
   if (samplesSinceLastCallback > 0)
      for (c = 0; c < *outNumTracks; c++)
          channels[c]->Append((char *)(mainBuffer + c),
                              int16Sample,
                              samplesRead,
                              *outNumTracks);

   for(c = 0; c < *outNumTracks; c++)
      channels[c]->Flush();

   /* ...the rest is de-allocation */
   delete[]mainBuffer;

   if (cancelled) {
      for (c = 0; c < *outNumTracks; c++)
         delete channels[c];
      delete[] channels;

      return false;
   }
   else {
      *outTracks = new Track *[*outNumTracks];
      for(c = 0; c < *outNumTracks; c++)
         (*outTracks)[c] = channels[c];
      delete[] channels;

      return true;
   }
}

OggImportFileHandle::~OggImportFileHandle()
{
   ov_clear(mVorbisFile);
   mFile->Detach();    // so that it doesn't try to close the file (ov_clear()
                       // did that already)

   delete mVorbisFile;
   delete mFile;
}

#endif                          /* USE_LIBVORBIS */
