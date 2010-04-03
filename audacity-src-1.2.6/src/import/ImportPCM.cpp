/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportPCM.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"
#include "../Internat.h"
#include "ImportPCM.h"

#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
#include <wx/ffile.h>

#include "sndfile.h"

#ifndef SNDFILE_1
#error Requires libsndfile 1.0 or higher
#endif

#include "../FileFormats.h"
#include "../Prefs.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"

class PCMImportPlugin : public ImportPlugin
{
public:
   PCMImportPlugin():
      ImportPlugin(wxStringList())
   {
      sf_get_all_extensions(mExtensions);
   }

   ~PCMImportPlugin() { }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};


class PCMImportFileHandle : public ImportFileHandle
{
public:
   PCMImportFileHandle(wxString name, SNDFILE *file, SF_INFO info);
   ~PCMImportFileHandle();

   void SetProgressCallback(progress_callback_t function,
                            void *userData);
   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   bool Import(TrackFactory *trackFactory, Track ***outTracks,
               int *outNumTracks);
private:
   wxString              mName;
   SNDFILE              *mFile;
   SF_INFO               mInfo;
   sampleFormat          mFormat;
   void                 *mUserData;
   progress_callback_t  mProgressCallback;
};

void GetPCMImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new PCMImportPlugin);
}

wxString PCMImportPlugin::GetPluginFormatDescription()
{
    return "Uncompressed PCM Audio Files";
}

ImportFileHandle *PCMImportPlugin::Open(wxString filename)
{
   SF_INFO info;
   SNDFILE *file;

   memset(&info, 0, sizeof(info));
   file = sf_open(FILENAME(filename), SFM_READ, &info);
   if (!file) {
      // TODO: Handle error
      //char str[1000];
      //sf_error_str((SNDFILE *)NULL, str, 1000);

      return NULL;
   }

   return new PCMImportFileHandle(filename, file, info);
}

PCMImportFileHandle::PCMImportFileHandle(wxString name,
                                         SNDFILE *file, SF_INFO info):
   mName(name),
   mFile(file),
   mInfo(info),
   mUserData(NULL),
   mProgressCallback(NULL)
{
   //
   // Figure out the format to use.
   //
   // In general, go with the user's preferences.  However, if
   // the file is higher-quality, go with a format which preserves
   // the quality of the original file.
   //
   
   mFormat = (sampleFormat)
      gPrefs->Read("/SamplingRate/DefaultProjectSampleFormat", floatSample);

   if (mFormat != floatSample &&
       sf_subtype_more_than_16_bits(mInfo.format))
      mFormat = floatSample;
}

void PCMImportFileHandle::SetProgressCallback(progress_callback_t progressCallback,
                                      void *userData)
{
   mProgressCallback = progressCallback;
   mUserData = userData;
}

wxString PCMImportFileHandle::GetFileDescription()
{
   return sf_header_name(mInfo.format);
}

int PCMImportFileHandle::GetFileUncompressedBytes()
{
   return mInfo.frames * mInfo.channels * SAMPLE_SIZE(mFormat);
}

bool PCMImportFileHandle::Import(TrackFactory *trackFactory,
                                 Track ***outTracks,
                                 int *outNumTracks)
{
   wxASSERT(mFile);

   *outNumTracks = mInfo.channels;
   WaveTrack **channels = new WaveTrack *[*outNumTracks];

   int c;
   for (c = 0; c < *outNumTracks; c++) {
      channels[c] = trackFactory->NewWaveTrack(mFormat);
      channels[c]->SetRate(mInfo.samplerate);

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

   sampleCount fileTotalFrames = (sampleCount)mInfo.frames;
   sampleCount maxBlockSize = channels[0]->GetMaxBlockSize();
   bool cancelled = false;
   
   wxString copyEdit =
       gPrefs->Read("/FileFormats/CopyOrEditUncompressedData", "edit");

   // Fall back to "edit" if it doesn't match anything else
   bool doEdit = true;          
   if (copyEdit.IsSameAs("copy", false))
      doEdit = false;

   // If the format is not seekable, we must use 'copy' mode,
   // because 'edit' mode depends on the ability to seek to an
   // arbitrary location in the file.
   if (!mInfo.seekable)
      doEdit = false;

   if (doEdit) {

      // If this mode has been selected, we form the tracks as
      // aliases to the files we're editing, i.e. ("foo.wav", 12000-18000)
      // instead of actually making fresh copies of the samples.

      for (sampleCount i = 0; i < fileTotalFrames; i += maxBlockSize) {
         sampleCount blockLen = maxBlockSize;
         if (i + blockLen > fileTotalFrames)
            blockLen = fileTotalFrames - i;

         for(c=0; c<(*outNumTracks); c++)
            channels[c]->AppendAlias(mName, i, blockLen, c);

         if( mProgressCallback )
            cancelled = mProgressCallback(mUserData,
                                          i*1.0 / fileTotalFrames);
         if (cancelled)
            break;
      }
   }
   else {
      // Otherwise, we're in the "copy" mode, where we read in the actual
      // samples from the file and store our own local copy of the
      // samples in the tracks.
      
      samplePtr srcbuffer = NewSamples(maxBlockSize * (*outNumTracks),
                                       mFormat);
      samplePtr buffer = NewSamples(maxBlockSize, mFormat);

      unsigned long framescompleted = 0;
      
      long block;
      do {
         block = maxBlockSize;
         
         if (mFormat == int16Sample)
            block = sf_readf_short(mFile, (short *)srcbuffer, block);
         else
            block = sf_readf_float(mFile, (float *)srcbuffer, block);
         
         if (block) {
            for(c=0; c<(*outNumTracks); c++) {
               if (mFormat==int16Sample) {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*outNumTracks)*j+c];
               }
               else {
                  for(int j=0; j<block; j++)
                     ((float *)buffer)[j] =
                        ((float *)srcbuffer)[(*outNumTracks)*j+c];
               }
               
               channels[c]->Append(buffer, mFormat, block);
            }
            framescompleted += block;
         }

         if( mProgressCallback )
            cancelled = mProgressCallback(mUserData,
                                          framescompleted*1.0 /
                                          fileTotalFrames);
         if (cancelled)
            break;

      } while (block > 0);
   }

   if (cancelled) {
      for (c = 0; c < *outNumTracks; c++)
         delete channels[c];
      delete[] channels;

      return false;
   }
   else {
      *outTracks = new Track *[*outNumTracks];
      for(c = 0; c < *outNumTracks; c++) {
         channels[c]->Flush();
         (*outTracks)[c] = channels[c];
      }
      delete[] channels;

      return true;
   }
}

PCMImportFileHandle::~PCMImportFileHandle()
{
   sf_close(mFile);
}


#if 0

#include <wx/file.h>
#include <wx/string.h>
#include <wx/thread.h>
#include <wx/timer.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/intl.h>

#include "Import.h"
#include "ImportPCM.h"

#include "../FileFormats.h"
#include "../WaveTrack.h"
#include "../DirManager.h"
#include "../Prefs.h"

#include "sndfile.h"

bool IsPCM(wxString fName)
{
   wxFile testFile;
   testFile.Open(fName);
   if (!testFile.IsOpened())
      return false;
   testFile.Close();

   SF_INFO    info;
   SNDFILE   *fp;

   fp = sf_open_read(FILENAME(fName), &info);

   if (fp) {
      sf_close(fp);
      return true;
   }

   return false;
}


bool ImportPCM(wxWindow * parent,
               wxString fName, 
               WaveTrack ** channels[],
               int *numChannels,
               DirManager * dirManager)
{
   SF_INFO       info;
   SNDFILE      *fp;
   sampleFormat  format;

   fp = sf_open_read(FILENAME(fName), &info);

   if (!fp) {
      char str[1000];
      sf_error_str((SNDFILE *)NULL, str, 1000);
      wxMessageBox(str);

      return false;
   }

   wxString progressStr;
   wxString formatName = sf_header_name(info.format & SF_FORMAT_TYPEMASK);
   progressStr.Printf(_("Importing %s file..."),
                      (const char *)formatName);

   *numChannels = info.channels;
   *channels = new WaveTrack*[*numChannels];

   if (info.pcmbitwidth > 16)
      format = floatSample;
   else
      format = int16Sample;

   int c;
   for(c=0; c<*numChannels; c++) {
      (*channels)[c] = new WaveTrack(dirManager, format);
      (*channels)[c]->SetRate(info.samplerate);
      (*channels)[c]->SetName(TrackNameFromFileName(fName));
      (*channels)[c]->SetChannel(Track::MonoChannel);
   }

   if (*numChannels == 2) {
      (*channels)[0]->SetChannel(Track::LeftChannel);
      (*channels)[1]->SetChannel(Track::RightChannel);
      (*channels)[0]->SetLinked(true);
   }

   sampleCount fileTotalFrames = (sampleCount)info.frames;
   sampleCount maxBlockSize = (*channels)[0]->GetMaxBlockSize();

   wxString copyEdit =
       gPrefs->Read("/FileFormats/CopyOrEditUncompressedData", "edit");

   // Fall back to "edit" if it doesn't match anything else
   bool doEdit = true;          
   if (copyEdit.IsSameAs("copy", false))
      doEdit = false;

   if (doEdit) {

      // If this mode has been selected, we form the tracks as
      // aliases to the files we're editing, i.e. ("foo.wav", 12000-18000)
      // instead of actually making fresh copies of the samples.

      wxProgressDialog *progress = NULL;
      wxYield();
      wxStartTimer();
      wxBusyCursor busy;

      bool cancelling = false;

      for (sampleCount i = 0; i < fileTotalFrames; i += maxBlockSize) {
         sampleCount blockLen = maxBlockSize;
         if (i + blockLen > fileTotalFrames)
            blockLen = fileTotalFrames - i;

         for(c=0; c<*numChannels; c++)
            (*channels)[c]->AppendAlias(fName, i, blockLen, c);

         if (!progress && wxGetElapsedTime(false) > 500) {
            progress =
                new wxProgressDialog(_("Import"), progressStr,
                                     1000,
                                     parent,
                                     wxPD_CAN_ABORT |
                                     wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
         }
         if (progress) {
            cancelling = !progress->Update((int)((i*1000.0)/fileTotalFrames));

            if (cancelling)
               i = fileTotalFrames;
         }
      }

      //printf(_("Time elapsed: %d\n"), wxGetElapsedTime());

      if (progress)
         delete progress;

      if (cancelling) {
         for(c=0; c<*numChannels; c++)
            delete (*channels)[c];
         delete[] (*channels);
         *channels = NULL;

         return false;
      }

      return true;
   }

   // Otherwise, we're in the "copy" mode, where we read in the actual
   // samples from the file and store our own local copy of the
   // samples in the tracks.

   samplePtr srcbuffer = NewSamples(maxBlockSize * (*numChannels),
                                    format);
   samplePtr buffer = NewSamples(maxBlockSize, format);

   sampleCount framescompleted = 0;

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;

   bool cancelling = false;

   long block;
   do {
      block = maxBlockSize;

      if (format == int16Sample)
         block = sf_readf_short(fp, (short *)srcbuffer, block);
      else
         block = sf_readf_float(fp, (float *)srcbuffer, block);

      if (block) {
         for(c=0; c<(*numChannels); c++) {

            if (format==int16Sample) {
               if (info.pcmbitwidth == 8) {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*numChannels)*j+c] << 8;
               }
               else {
                  for(int j=0; j<block; j++)
                     ((short *)buffer)[j] =
                        ((short *)srcbuffer)[(*numChannels)*j+c];
               }
            }
            else
               for(int j=0; j<block; j++)
                  ((float *)buffer)[j] =
                     ((float *)srcbuffer)[(*numChannels)*j+c];

            (*channels)[c]->Append(buffer, format, block);
         }

         framescompleted += block;
      }

      if (!progress && wxGetElapsedTime(false) > 500) {
         progress =
            new wxProgressDialog(_("Import"), progressStr,
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }
      if (progress) {
         int progressvalue = (framescompleted > fileTotalFrames) ?
             fileTotalFrames : framescompleted;

         cancelling =
            !progress->Update((int)((progressvalue*1000.0)/fileTotalFrames));

         if (cancelling)
            block = 0;
      }
   } while (block > 0);

   sf_close(fp);

   //printf("Time elapsed: %d\n", wxGetElapsedTime());

   if (progress)
      delete progress;

   DeleteSamples(srcbuffer);
   DeleteSamples(buffer);

   if (cancelling) {
      for(c=0; c<*numChannels; c++)
         delete (*channels)[c];
      delete[] (*channels);
      *channels = NULL;

      return false;
   }

   return true;
}

#endif
