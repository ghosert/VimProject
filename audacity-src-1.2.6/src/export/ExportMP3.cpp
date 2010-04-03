/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMP3.cpp

  Joshua Haberman

  This just acts as an interface to LAME. A Lame dynamic library must
  be present

  The difficulty in our approach is that we are attempting to use LAME
  in a way it was not designed to be used. LAME's API is reasonably
  consistant, so if we were linking directly against it we could expect
  this code to work with a variety of different LAME versions. However,
  the data structures change from version to version, and so linking
  with one version of the header and dynamically linking against a
  different version of the dynamic library will not work correctly.

  The solution is to find the lowest common denominator between versions.
  The bare minimum of functionality we must use is this:
      1. Initialize the library.
      2. Set, at minimum, the following global options:
          i.  input sample rate
          ii. input channels
      3. Encode the stream
      4. Call the finishing routine

  Just so that it's clear that we're NOT free to use whatever features
  of LAME we like, I'm not including lame.h, but instead enumerating
  here the extent of functions and structures that we can rely on being
  able to import and use from a dynamic library.

  For the record, we aim to support LAME 3.70 on. Since LAME 3.70 was
  released in April of 2000, that should be plenty.


  Copyright 2002, 2003 Joshua Haberman.
  Some portions may be Copyright 2003 Paolo Patruno.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

**********************************************************************/

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <wx/dynlib.h>
#include <wx/msgdlg.h>
#include <wx/utils.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/window.h>
#include <wx/ffile.h>
#include <wx/log.h>
#include <wx/filedlg.h>
#include <wx/intl.h>

#include "../Audacity.h"
#include "ExportMP3.h"
#include "../Internat.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Tags.h"
#include "../WaveTrack.h"

#ifdef __WXMAC__
#define __MOVIES__  /* Apple's Movies.h not compatible with Audacity */
/* #define __MACHELP__ */

#include <wx/mac/private.h>
# ifdef __UNIX__
#  include <CoreServices/CoreServices.h>
# else
#  include <Files.h>
# endif
#endif

MP3Exporter::MP3Exporter()
{
   if (gPrefs)
      mLibPath = gPrefs->Read("/MP3/MP3LibPath", "");
}

bool MP3Exporter::FindLibrary(wxWindow *parent)
{
   mLibPath = gPrefs->Read("/MP3/MP3LibPath", "");

   if (mLibPath=="" || !::wxFileExists(FILENAME(mLibPath))) {
   
      int action = wxMessageBox(GetLibraryMessage(),
                                _("Export MP3"),
                                wxYES_NO | wxICON_EXCLAMATION,
                                parent);

      if (action == wxYES) {
         wxString question;
         /* i18n-hint: It's asking for the location of a file, for
            example, "Where is lame_enc.dll?" - you could translate
            "Where would I find the file %s" instead if you want. */
         question.Printf(_("Where is %s?"), (const char *)GetLibraryName());
         mLibPath = wxFileSelector(question, 
                                   GetLibraryPath(),        // Path
                                   GetLibraryName(),        // Name
                                   "",      // Extension
                                   GetLibraryTypeString(),
                                   wxOPEN, parent);
         
         if (mLibPath == "") {
            mLibPath = "";
            gPrefs->Write("/MP3/MP3LibPath", mLibPath);
         
            return false;
         }
         
         wxString path, baseName, extension;
         ::wxSplitPath(mLibPath, &path, &baseName, &extension);
         
         if (extension != "")
            baseName += "." + extension;
         
         if (baseName.CmpNoCase(GetLibraryName())) {
         
            wxString question;
            question.Printf(_("Audacity was expecting a library named \"%s\".  "
                              "Are you sure you want to attempt to export MP3 "
                              "files using \"%s\"?"),
                            (const char *)GetLibraryName(),
                            (const char *)baseName);

            int action = wxMessageBox(question,
                                      _("Export MP3"),
                                      wxYES_NO | wxICON_EXCLAMATION,
                                      parent);
            
            if (action != wxYES) {
               mLibPath = "";
               gPrefs->Write("/MP3/MP3LibPath", mLibPath);
            
               return false;
            }
         }
      }
      else {
         mLibPath = "";
         gPrefs->Write("/MP3/MP3LibPath", mLibPath);
            
         return false;
      }
      
      gPrefs->Write("/MP3/MP3LibPath", mLibPath);
   }
   
   return true;
}

#if defined(__WXGTK__) || defined(__WXX11__) || defined(__WXMOTIF__)

   /* --------------------------------------------------------------------------*/

   struct lame_global_flags;
   typedef lame_global_flags *lame_init_t(void);
   typedef int lame_init_params_t(lame_global_flags*);
   typedef const char* get_lame_version_t(void);

   typedef int lame_encode_buffer_t (
         lame_global_flags* gf,
         const short int    buffer_l [],
         const short int    buffer_r [],
         const int          nsamples,
         unsigned char *    mp3buf,
         const int          mp3buf_size );

   typedef int lame_encode_buffer_interleaved_t(
         lame_global_flags* gf,
         short int          pcm[],
         int                num_samples,   /* per channel */
         unsigned char*     mp3buf,
         int                mp3buf_size );

   typedef int lame_encode_flush_t(
         lame_global_flags *gf,
         unsigned char*     mp3buf,
         int                size );

   typedef int lame_close_t(lame_global_flags*);
   
   typedef int lame_set_in_samplerate_t(lame_global_flags*, int);
   typedef int lame_set_num_channels_t(lame_global_flags*, int );
   typedef int lame_set_quality_t(lame_global_flags*, int);
   typedef int lame_get_quality_t(lame_global_flags*);
   typedef int lame_set_brate_t(lame_global_flags*, int);
   typedef int lame_get_brate_t(lame_global_flags*);
   

   /* --------------------------------------------------------------------------*/

   class LinuxLAMEExporter : public MP3Exporter {
      private:
         /* function pointers to the symbols we get from the library */
         lame_init_t* lame_init;
         lame_init_params_t* lame_init_params;
         lame_encode_buffer_t* lame_encode_buffer;
         lame_encode_buffer_interleaved_t* lame_encode_buffer_interleaved;
         lame_encode_flush_t* lame_encode_flush;
         lame_close_t* lame_close;
         get_lame_version_t* get_lame_version;
         
         lame_set_in_samplerate_t* lame_set_in_samplerate;
         lame_set_num_channels_t* lame_set_num_channels;
         lame_set_quality_t* lame_set_quality;
         lame_get_quality_t* lame_get_quality;
         lame_set_brate_t* lame_set_brate;
         lame_get_brate_t* lame_get_brate;

         lame_global_flags *mGF;
         
         bool mLibraryLoaded, mEncoding;
         char mVersion[20];

         static const int mSamplesPerChunk = 220500;
         static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);
      public:
         
         LinuxLAMEExporter() {
            mLibraryLoaded = false;
            mEncoding = false;
            mGF = NULL;
         }
         
      wxString GetLibraryPath()
      {
         return "/usr/lib";
      }

         wxString GetLibraryName()
         {
            return "libmp3lame.so";
         }
         
         wxString GetLibraryTypeString()
         {
            return wxString(_("Only libmp3lame.so|libmp3lame.so|Primary Shared Object files (*.so)|*.so|Extended Libraries (*.so*)|*.so*|All Files (*)|*"));
         }
         
         wxString GetLibraryMessage()
         {
            /* i18n-hint: This message is used on Unix/Linux */
            return _("Audacity does not export MP3 files directly, but instead uses the \n"
                   "freely available LAME library to handle MP3 file encoding.  You must \n"
                   "obtain libmp3lame.so separately, either by downloading it or building \n"
                   "it from the sources, and then locate the file for Audacity.  You only \n"
                   "need to do this once.\n\n"
                   "Would you like to locate libmp3lame.so now?");
         }

         bool  LoadLibrary() {
            wxLogNull logNo;

            //BG: I was unable to test the wxDynamicLibrary code on this platform

            if (wxFileExists(FILENAME(mLibPath)))
            {
               if(lame_enc_lib.IsLoaded())
               {
                  lame_enc_lib.Unload();
               }

               if(!lame_enc_lib.Load(FILENAME(mLibPath)))
               {
                  return false;
               }
            }
            else
               return false;

            /* get function pointers from the shared library */

            lame_init = (lame_init_t *)lame_enc_lib.GetSymbol("lame_init");
            get_lame_version = (get_lame_version_t *)lame_enc_lib.GetSymbol("get_lame_version");
            lame_init_params = 
               (lame_init_params_t *) lame_enc_lib.GetSymbol("lame_init_params");
            lame_encode_buffer =
                (lame_encode_buffer_t *) lame_enc_lib.GetSymbol("lame_encode_buffer");
            lame_encode_buffer_interleaved =
                (lame_encode_buffer_interleaved_t *) lame_enc_lib.GetSymbol("lame_encode_buffer_interleaved");
            lame_encode_flush =
                (lame_encode_flush_t *) lame_enc_lib.GetSymbol("lame_encode_flush");
            lame_close =
                (lame_close_t *) lame_enc_lib.GetSymbol("lame_close");

            lame_close =
                (lame_close_t *) lame_enc_lib.GetSymbol("lame_close");

            lame_set_in_samplerate =
                (lame_set_in_samplerate_t *) lame_enc_lib.GetSymbol("lame_set_in_samplerate");
            lame_set_num_channels =
                (lame_set_num_channels_t *) lame_enc_lib.GetSymbol("lame_set_num_channels");
            lame_set_quality =
                (lame_set_quality_t *) lame_enc_lib.GetSymbol("lame_set_quality");
            lame_get_quality =
                (lame_get_quality_t *) lame_enc_lib.GetSymbol("lame_get_quality");
            lame_set_brate =
                (lame_set_brate_t *) lame_enc_lib.GetSymbol("lame_set_brate");
            lame_get_brate =
                (lame_get_brate_t *) lame_enc_lib.GetSymbol("lame_get_brate");

            /* we assume that if all the symbols are found, it's a valid library */

            if (!lame_init ||
                !get_lame_version ||
                !lame_init_params ||
                !lame_encode_buffer ||
                !lame_encode_buffer_interleaved ||
                !lame_encode_flush ||
                !lame_close ||
                !lame_set_in_samplerate ||
                !lame_set_num_channels ||
                !lame_set_quality ||
                !lame_set_brate) {
               return false;
            }

            mGF = lame_init();
            mLibraryLoaded = true;
            return true;
         }

      bool ValidLibraryLoaded() { return mLibraryLoaded; }

      wxString GetLibraryVersion() {
         if(!mLibraryLoaded) return "";

         return wxString::Format("LAME %s", get_lame_version());
      }

      int InitializeStream(int channels, int sampleRate) {
         if(!mLibraryLoaded) return -1;

         lame_set_num_channels(mGF, channels);
         lame_set_in_samplerate(mGF, sampleRate);

         lame_init_params(mGF);

         mEncoding = true;
         return mSamplesPerChunk;
      }

      int GetOutBufferSize() {
         return mOutBufferSize;
      }

      int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
         if(!mEncoding) return -1;

         return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
            outbuffer, mOutBufferSize);
      }

      int EncodeRemainder(short int inbuffer[], int nSamples,
                        unsigned char outbuffer[]) {
         return lame_encode_buffer_interleaved(mGF, inbuffer, nSamples, outbuffer,
            mOutBufferSize);
      }

      int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[]) {
         if(!mEncoding) return -1;

         return lame_encode_buffer(mGF, inbuffer,inbuffer, mSamplesPerChunk,
            outbuffer, mOutBufferSize);
      }

      int EncodeRemainderMono(short int inbuffer[], int nSamples,
                        unsigned char outbuffer[]) {
         return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples, outbuffer,
            mOutBufferSize);
      }

      int FinishStream(unsigned char outbuffer[]) {
         mEncoding = false;
         int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);
         lame_close(mGF);
         return result;
      }

      void CancelEncoding() { mEncoding = false; }

      int GetConfigurationCaps() { return MP3CONFIG_BITRATE|MP3CONFIG_QUALITY; }

      int GetQualityVariance() { return 10; }

      void SetBitrate(int rate) { lame_set_brate(mGF, rate); }
      int GetBitrate() { return lame_get_quality(mGF); }

      void SetQuality(int quality) { lame_set_quality(mGF, quality); }
      int GetQuality() { return lame_get_quality(mGF); }
   };

MP3Exporter *gMP3Exporter = NULL;

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new LinuxLAMEExporter();
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if( gMP3Exporter )
      delete gMP3Exporter;
   gMP3Exporter = NULL;
}


#elif defined(__MACOSX__)

   /* --------------------------------------------------------------------------*/

   struct lame_global_flags;
   typedef lame_global_flags *lame_init_t(void);
   typedef int lame_init_params_t(lame_global_flags*);
   typedef const char* get_lame_version_t(void);

   typedef int lame_encode_buffer_t (
         lame_global_flags* gf,
         const short int    buffer_l [],
         const short int    buffer_r [],
         const int          nsamples,
         unsigned char *    mp3buf,
         const int          mp3buf_size );

   typedef int lame_encode_buffer_interleaved_t(
         lame_global_flags* gf,
         short int          pcm[],
         int                num_samples,   /* per channel */
         unsigned char*     mp3buf,
         int                mp3buf_size );

   typedef int lame_encode_flush_t(
         lame_global_flags *gf,
         unsigned char*     mp3buf,
         int                size );

   typedef int lame_close_t(lame_global_flags*);
   
   typedef int lame_set_in_samplerate_t(lame_global_flags*, int);
   typedef int lame_set_num_channels_t(lame_global_flags*, int );
   typedef int lame_set_quality_t(lame_global_flags*, int);
   typedef int lame_get_quality_t(lame_global_flags*);
   typedef int lame_set_brate_t(lame_global_flags*, int);
   typedef int lame_get_brate_t(lame_global_flags*);
   

   /* --------------------------------------------------------------------------*/

   class MacOSXLAMEExporter : public MP3Exporter {
      private:
         /* function pointers to the symbols we get from the library */
         lame_init_t* lame_init;
         lame_init_params_t* lame_init_params;
         lame_encode_buffer_t* lame_encode_buffer;
         lame_encode_buffer_interleaved_t* lame_encode_buffer_interleaved;
         lame_encode_flush_t* lame_encode_flush;
         lame_close_t* lame_close;
         get_lame_version_t* get_lame_version;
         
         lame_set_in_samplerate_t* lame_set_in_samplerate;
         lame_set_num_channels_t* lame_set_num_channels;
         lame_set_quality_t* lame_set_quality;
         lame_get_quality_t* lame_get_quality;
         lame_set_brate_t* lame_set_brate;
         lame_get_brate_t* lame_get_brate;

         lame_global_flags *mGF;
         
         bool mLibraryLoaded, mEncoding;
         char mVersion[20];

         static const int mSamplesPerChunk = 220500;
         static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);
      public:
         
         MacOSXLAMEExporter() {
            mLibraryLoaded = false;
            mEncoding = false;
            mGF = NULL;
         }
         
      wxString GetLibraryPath()
      {
         return "";
      }


         void MakePString(unsigned char *p, const char *c)
         {
            int len = strlen(c);
            for(int i=len; i>=1; i--)
               p[i] = (unsigned char)c[i-1];
            p[0] = (unsigned char)len;
         }

         // MachOFunctionPointerForCFMFunctionPointer(void *cfmfp)
         //
         // Borrowed from the Apple Sample Code file "CFM_MachO_CFM.c"
         // This function allocates a block of CFM glue code which contains
         // the instructions to call CFM routines

         void *NewMachOFromCFM(void *cfmfp)
         {
            if (cfmfp == 0)
               return 0;

            UInt32 CFMTemplate[6] = {0x3D800000, 0x618C0000, 0x800C0000,
                                     0x804C0004, 0x7C0903A6, 0x4E800420};
            UInt32 *mfp = (UInt32*)NewPtr(sizeof(CFMTemplate));

            mfp[0] = CFMTemplate[0] | ((UInt32)cfmfp >> 16);
            mfp[1] = CFMTemplate[1] | ((UInt32)cfmfp & 0xFFFF);
            mfp[2] = CFMTemplate[2];
            mfp[3] = CFMTemplate[3];
            mfp[4] = CFMTemplate[4];
            mfp[5] = CFMTemplate[5];
            MakeDataExecutable(mfp, sizeof(CFMTemplate));
   
            return(mfp);
         }



         wxString GetLibraryName()
         {
            return "LameLib";
         }
         
         wxString GetLibraryTypeString()
         {
            return wxString(_("Only LameLib|LameLib|All Files (*)|*"));
         }
         
         wxString GetLibraryMessage()
         {
            // Must be <= 255 characters on Mac
            /* i18n-hint: This message is used on Mac OS X.  This particular
             message must be <= 255 characters.  Be brief. */
            return _("Audacity does not export MP3 files directly, but instead uses LAME, "
                   "an MP3 exporting library available separately.  See the documentation "
                   "for more information.\n\n"
                   "Would you like to locate LameLib now?");
         }

         bool  LoadLibrary() {
            FSSpec spec;
            OSErr err;
            CFragConnectionID connID;
            Ptr mainAddr;
            Str255 errMsg;
            Str255 name;
            CFragSymbolClass symClass;

            if (!wxFileExists(FILENAME(mLibPath)))
               return false;

            wxMacFilename2FSSpec(FILENAME(mLibPath), &spec);

            name[0] = 0;
            err = GetDiskFragment(&spec, 0, kCFragGoesToEOF,
                                  name,
                                  kPrivateCFragCopy,
                                  &connID,
                                  &mainAddr,
                                  errMsg);

            if (err) {
               printf("GetDiskFragment: err=%d\n", err);
               return false;
            }

            MakePString(name, "lame_init");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_init = NewMachOFromCFM(mainAddr);

            MakePString(name, "get_lame_version");
            FindSymbol(connID, name, &mainAddr, &symClass);
            get_lame_version = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_init_params");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_init_params = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_encode_buffer");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_encode_buffer = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_encode_buffer_interleaved");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_encode_buffer_interleaved = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_encode_flush");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_encode_flush = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_close");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_close = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_set_in_samplerate");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_set_in_samplerate = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_set_num_channels");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_set_num_channels = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_set_quality");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_set_quality = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_get_quality");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_get_quality = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_set_brate");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_set_brate = NewMachOFromCFM(mainAddr);

            MakePString(name, "lame_get_brate");
            FindSymbol(connID, name, &mainAddr, &symClass);
            lame_get_brate = NewMachOFromCFM(mainAddr);


            /* we assume that if all the symbols are found, it's a valid library */

            if (!lame_init ||
                !get_lame_version ||
                !lame_init_params ||
                !lame_encode_buffer ||
                !lame_encode_buffer_interleaved ||
                !lame_encode_flush ||
                !lame_close ||
                !lame_set_in_samplerate ||
                !lame_set_num_channels ||
                !lame_set_quality ||
                !lame_set_brate) {
               return false;
            }

            mGF = lame_init();
            mLibraryLoaded = true;
            return true;
         }

      bool ValidLibraryLoaded() { return mLibraryLoaded; }

      wxString GetLibraryVersion() {
         if(!mLibraryLoaded) return "";

         return wxString::Format("LAME %s", get_lame_version());
      }

      int InitializeStream(int channels, int sampleRate) {
         if(!mLibraryLoaded) return -1;

         lame_set_num_channels(mGF, channels);
         lame_set_in_samplerate(mGF, sampleRate);

         lame_init_params(mGF);

         mEncoding = true;
         return mSamplesPerChunk;
      }

      int GetOutBufferSize() {
         return mOutBufferSize;
      }

      int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
         if(!mEncoding) return -1;

         return lame_encode_buffer_interleaved(mGF, inbuffer, mSamplesPerChunk,
            outbuffer, mOutBufferSize);
      }

      int EncodeRemainder(short int inbuffer[], int nSamples,
                        unsigned char outbuffer[]) {
         return lame_encode_buffer_interleaved(mGF, inbuffer, nSamples, outbuffer,
            mOutBufferSize);
      }

      int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[]) {
         if(!mEncoding) return -1;

         return lame_encode_buffer(mGF, inbuffer, inbuffer, mSamplesPerChunk,
            outbuffer, mOutBufferSize);
      }

      int EncodeRemainderMono(short int inbuffer[], int nSamples,
                        unsigned char outbuffer[]) {
         return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples, outbuffer,
            mOutBufferSize);
      }

      int FinishStream(unsigned char outbuffer[]) {
         mEncoding = false;
         int result = lame_encode_flush(mGF, outbuffer, mOutBufferSize);
         lame_close(mGF);
         return result;
      }

      void CancelEncoding() { mEncoding = false; }

      int GetConfigurationCaps() { return MP3CONFIG_BITRATE|MP3CONFIG_QUALITY; }

      int GetQualityVariance() { return 10; }

      void SetBitrate(int rate) { lame_set_brate(mGF, rate); }
      int GetBitrate() { return lame_get_quality(mGF); }

      void SetQuality(int quality) { lame_set_quality(mGF, quality); }
      int GetQuality() { return lame_get_quality(mGF); }
   };

MP3Exporter *gMP3Exporter = NULL;

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new MacOSXLAMEExporter();
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if( gMP3Exporter )
      delete gMP3Exporter;
   gMP3Exporter = NULL;
}


#elif defined(__WXMAC__)

   /* --------------------------------------------------------------------------*/

   /* The following code is intended to work with LAMELib, roughly LAME
      verson 3.87, as distributed by N2MP3. */

   typedef struct {
      /* input file description */
      unsigned long num_samples;  /* number of samples. default=2^32-1    */
      int num_channels;           /* input number of channels. default=2  */
      int in_samplerate;          /* input_samp_rate. default=44.1kHz     */
      int out_samplerate;         /* output_samp_rate. (usually determined automatically)   */ 
      float scale;                /* scale input by this amount */

      /* general control params */
      int gtkflag;                /* run frame analyzer?       */
      int bWriteVbrTag;           /* add Xing VBR tag?         */
      int disable_waveheader;     /* disable writing of .wav header, when *decoding* */
      int decode_only;            /* use lame/mpglib to convert mp3 to wav */
      int ogg;                    /* encode to Vorbis .ogg file */

      int quality;                /* quality setting 0=best,  9=worst  */
      int silent;                 /* disable some status output */
      float update_interval;      /* to use Frank's time status display */
      int brhist_disp;            /* enable VBR bitrate histogram display */
      int mode;                       /* 0,1,2,3 stereo,jstereo,dual channel,mono */
      int mode_fixed;                 /* use specified the mode, do not use lame's opinion of the best mode */
      int force_ms;                   /* force M/S mode.  requires mode=1 */
      int brate;                      /* bitrate */
      float compression_ratio;          /* user specified compression ratio, instead of brate */
      int free_format;                /* use free format? */

      int space[10000];  /* to liberally accomadate for the real size of the struct */
   } lame_global_flags;

   /* All functions types are suffexed with _t because gcc won't let you have a
    * type and a variable of the same name */

   typedef void lame_init_t(lame_global_flags *);

   /* NOTE: Same deal with this one: >= 3.88 changes it to: const char
    * *get_lame_version(), but this time they don't even leave us a
    * compatibility version! aggh! */
   typedef void lame_version_t(lame_global_flags *, char *);
   typedef const char *get_lame_version_t();

   typedef void lame_init_params_t(lame_global_flags*);

   typedef int lame_encode_buffer_t (
         lame_global_flags* gf,
         const short int    buffer_l [],
         const short int    buffer_r [],
         const int          nsamples,
         unsigned char *    mp3buf,
         const int          mp3buf_size );

   typedef int lame_encode_buffer_interleaved_t(
         lame_global_flags* gf,
         short int          pcm[],
         int                num_samples,   /* per channel */
         unsigned char*     mp3buf,
         int                mp3buf_size );

   typedef int lame_encode_finish_t(
         lame_global_flags *gf,
         unsigned char*     mp3buf,
         int                size );

   /* --------------------------------------------------------------------------*/

   class MacLAMEExporter : public MP3Exporter {
      private:
         lame_init_t* lame_init;
         lame_version_t* lame_version;
         get_lame_version_t* get_lame_version;
         lame_init_params_t* lame_init_params;
         lame_encode_buffer_t* lame_encode_buffer;
         lame_encode_finish_t* lame_encode_finish;

         lame_global_flags *mGF;
         
         bool mLibraryLoaded, mEncoding;
         char mVersion[20];

         static const int mSamplesPerChunk = 220500;
         static const int mOutBufferSize = int(1.25 * mSamplesPerChunk + 7200);

         short int *mLeftBuffer;
         short int *mRightBuffer;
         
      public:
         
         MacLAMEExporter() {
            mLibraryLoaded = false;
            mEncoding = false;
            mGF = NULL;
         }

         wxString GetLibraryPath()
         {
            return "";
         }

         wxString GetLibraryName()
         {
            return "LAMELib";
         }
         
         wxString GetLibraryTypeString()
         {
            return _("Only LAMELib|LAMELib|Shared Libraries (*)|*");
         }
         
         wxString GetLibraryMessage()
         {
            // Must be <= 255 characters on Mac
            /* i18n-hint: This message is used on Mac OS 9.  This particular
             message must be <= 255 characters.  Be brief. */
            return _("Audacity does not export MP3 files directly, but instead uses LAME, "
                   "an MP3 exporting library available separately.  See the documentation "
                   "for more information.\n\n"
                   "Would you like to locate LameLib now?");
         }

         bool  LoadLibrary() {
            wxLogNull logNo;

            //BG: I was unable to test the wxDynamicLibrary code on this platform

            if (wxFileExists(FILENAME(mLibPath)))
            {
               if(lame_enc_lib.IsLoaded())
               {
                  lame_enc_lib.Unload();
               }

               if(!lame_enc_lib.Load(FILENAME(mLibPath)))
               {
                  return false;
               }
            }
            else
               return false;

            lame_init = (lame_init_t *) lame_enc_lib.GetSymbol("lame_init");

            lame_version = (lame_version_t *) lame_enc_lib.GetSymbol("lame_version");
            
            get_lame_version =
               (get_lame_version_t *) lame_enc_lib.GetSymbol("get_lame_version");

            lame_init_params = 
               (lame_init_params_t *) lame_enc_lib.GetSymbol("lame_init_params");

            lame_encode_buffer =
                (lame_encode_buffer_t *) lame_enc_lib.GetSymbol("lame_encode_buffer");
            lame_encode_finish =
                (lame_encode_finish_t *) lame_enc_lib.GetSymbol("lame_encode_finish");

            if (!lame_init ||
                !lame_init_params ||
                !lame_encode_buffer ||
                !(lame_version || get_lame_version) ||
                !lame_encode_finish) {
               return false;
            }

            mGF = new lame_global_flags;
            lame_init(mGF);
            mLibraryLoaded = true;
            return true;
         }

      bool ValidLibraryLoaded() { return mLibraryLoaded; }

      wxString GetLibraryVersion() {
         if(!mLibraryLoaded) return "";

         if(get_lame_version)
            return get_lame_version();
         else {
            lame_version(mGF, mVersion);
            return mVersion;
         }
      }

      int InitializeStream(int channels, int sampleRate) {
         if(!mLibraryLoaded) return -1;

         mGF->num_channels = channels;
         mGF->in_samplerate = sampleRate;
         mGF->out_samplerate = sampleRate;
         mGF->num_samples = 0;
         
         if (channels == 1)
            mGF->mode = 3;  // mono
         else
            mGF->mode = 1;  // joint stereo

         lame_init_params(mGF);
         
         mLeftBuffer = new short[mSamplesPerChunk];
         mRightBuffer = new short[mSamplesPerChunk];

         mEncoding = true;
         return mSamplesPerChunk;
      }

      int GetOutBufferSize() {
         return mOutBufferSize;
      }
      
      int GetConfigurationCaps() {
         return MP3CONFIG_BITRATE;
      }

      int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
         if(!mEncoding) return -1;
         
         if (mGF->num_channels == 2) {
            for(int i=0; i<mSamplesPerChunk; i++) {
               mLeftBuffer[i] = inbuffer[2*i];
               mRightBuffer[i] = inbuffer[2*i+1];
            }

            return lame_encode_buffer(mGF, mLeftBuffer, mRightBuffer, mSamplesPerChunk,
                                      outbuffer, mOutBufferSize);
         }
         else {
            return lame_encode_buffer(mGF, inbuffer, inbuffer, mSamplesPerChunk,
                       outbuffer, mOutBufferSize);
         }
      }

      int EncodeRemainder(short int inbuffer[], int nSamples,
                        unsigned char outbuffer[]) {

         if (mGF->num_channels == 2) {
            for(int i=0; i<nSamples; i++) {
               mLeftBuffer[i] = inbuffer[2*i];
               mRightBuffer[i] = inbuffer[2*i+1];
            }

            return lame_encode_buffer(mGF, mLeftBuffer, mRightBuffer, nSamples, outbuffer,
               mOutBufferSize);
         }
         else {
            return lame_encode_buffer(mGF, inbuffer, inbuffer, nSamples,
                       outbuffer, mOutBufferSize);
         }
      }

      int FinishStream(unsigned char outbuffer[]) {
         mEncoding = false;
         int result = lame_encode_finish(mGF, outbuffer, mOutBufferSize);
         
         delete[] mLeftBuffer;
         delete[] mRightBuffer;
         
         return result;
      }

      void CancelEncoding() { mEncoding = false; }

      int GetQualityVariance() { return -1; }

      void SetBitrate(int rate) { }
      int GetBitrate() { return -1; }

      void SetQuality(int quality) { }
      int GetQuality() { return -1; }
   };

MP3Exporter *gMP3Exporter = NULL;

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new MacLAMEExporter();
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if( gMP3Exporter )
      delete gMP3Exporter;
   gMP3Exporter = NULL;
}


#elif defined(__WXMSW__)

#include "BladeMP3EncDLL.h"

class BladeEncExporter : public MP3Exporter {
private:
   BE_CONFIG mConf;
   BE_VERSION mVersion;
   HBE_STREAM mStreamHandle;
   bool mLibraryLoaded, mEncoding,mStereo;
   unsigned long mOutBufferSize, mInSampleNum;
   int mDefaultRate;


   BEINITSTREAM beInitStream;
   BEENCODECHUNK beEncodeChunk;
   BEDEINITSTREAM beDeinitStream;
   BECLOSESTREAM beCloseStream;
   BEVERSION beVersion;


public:
   BladeEncExporter() {
      mLibraryLoaded = false;
      mEncoding = false;

      /* Set all the config defaults to sane values */

      memset(&mConf, 0, sizeof(BE_CONFIG));
//      mConf.dwConfig = BE_CONFIG_LAME;
//      mConf.format.LHV1.dwStructVersion = 1;
//      mConf.format.LHV1.dwStructSize = sizeof(BE_CONFIG);
//      mConf.format.LHV1.dwReSampleRate = 0;
//      mConf.format.LHV1.dwBitrate = 128;
      //mConf.format.LHV1.dwMaxBitrate = 128;
//      mConf.format.LHV1.nPreset = LQP_HIGH_QUALITY;
//	  mConf.format.LHV1.dwMpegVersion = MPEG1;
//      mConf.format.LHV1.bCopyright = false;
//      mConf.format.LHV1.bCRC = true;
//      mConf.format.LHV1.bOriginal = false;
//      mConf.format.LHV1.bPrivate = false;
//      mConf.format.LHV1.bWriteVBRHeader = false;
//      mConf.format.LHV1.bEnableVBR = true;
//      mConf.format.LHV1.nVBRQuality = 2;
//      mConf.format.LHV1.dwVbrAbr_bps = -1;
//      mConf.format.LHV1.bNoRes = true;
      mConf.dwConfig = BE_CONFIG_MP3;
      mConf.format.mp3.wBitrate = 128;
      mConf.format.mp3.bCopyright = false;
      mConf.format.mp3.bCRC = true;
      mConf.format.mp3.bOriginal = false;
      mConf.format.mp3.bPrivate = false;
      
      mDefaultRate = 128;
   }


   wxString GetLibraryPath()
   {
      return "";
   }

   wxString GetLibraryName()
   {
      return "lame_enc.dll";
   }
   
   wxString GetLibraryTypeString()
   {
      return _("Only lame_enc.dll|lame_enc.dll|Dynamically Linked Libraries (*.dll)|*.dll|All Files (*.*)|*");
   }
   
   wxString GetLibraryMessage()
   {
      /* i18n-hint: This message is used on Windows. */
      return _("Audacity does not export MP3 files directly, but instead uses the\n"
             "freely available LAME library to handle MP3 file encoding.  You must\n"
             "obtain lame_enc.dll separately, by downloading the LAME MP3 encoder,"
             "and then locate this file for Audacity.  You only need to do this once.\n\n"
             "Would you like to locate lame_enc.dll now?");
   }


   bool  LoadLibrary() {
      wxLogNull logNo;

      if (wxFileExists(FILENAME(mLibPath)))
      {
         if(lame_enc_lib.IsLoaded())
         {
            lame_enc_lib.Unload();
         }

         if(!lame_enc_lib.Load(FILENAME(mLibPath)))
         {
            return false;
         }
      }
      else
         return false;

      beInitStream = (BEINITSTREAM)lame_enc_lib.GetSymbol("beInitStream");
      beEncodeChunk = (BEENCODECHUNK)lame_enc_lib.GetSymbol("beEncodeChunk");
      beDeinitStream = (BEDEINITSTREAM)lame_enc_lib.GetSymbol("beDeinitStream");
      beCloseStream = (BECLOSESTREAM)lame_enc_lib.GetSymbol("beCloseStream");
      beVersion = (BEVERSION)lame_enc_lib.GetSymbol("beVersion");

      if(!beInitStream ||
         !beEncodeChunk ||
         !beDeinitStream ||
         !beCloseStream ||
         !beVersion)
         return false;

      beVersion(&mVersion);
      mLibraryLoaded = true;
      return true;
   }

   bool ValidLibraryLoaded() { return mLibraryLoaded; }

   wxString GetLibraryVersion() {
      BE_VERSION ver;

      if(!mLibraryLoaded)
         return "";

      beVersion(&ver);

	  return wxString::Format("LAME v%d.%d", ver.byMajorVersion, ver.byMinorVersion);
   }

   int InitializeStream(int channels, int sampleRate) {

      if(!mLibraryLoaded)
         return -1;

	  //int modes[] = { 0, BE_MP3_MODE_MONO, BE_MP3_MODE_STEREO };
	  //mConf.format.LHV1.dwSampleRate = sampleRate;
	  //mConf.format.LHV1.nMode = modes[channels];
      int modes[] = { 0, BE_MP3_MODE_MONO, BE_MP3_MODE_STEREO };
      mConf.format.mp3.byMode = modes[channels];
      mConf.format.mp3.dwSampleRate = sampleRate;
      mConf.format.mp3.wBitrate = mDefaultRate;

      beInitStream(&mConf, &mInSampleNum, &mOutBufferSize, &mStreamHandle);

      mEncoding = true;

      if(channels == 2) {
         mStereo = true;
         return(mInSampleNum / 2); /* convert samples_total into samples_per_channel */
      }
      else {
         mStereo = false;
         return (mInSampleNum);
      }

   }

   int GetOutBufferSize() {
      if (!mEncoding)
         return -1;

      return mOutBufferSize;
   }

   int EncodeBuffer(short int inbuffer[], unsigned char outbuffer[]) {
      if(!mEncoding)
         return -1;
      
      unsigned long bytes;
      beEncodeChunk(mStreamHandle, mInSampleNum, inbuffer, outbuffer, &bytes);

      return bytes;
   }

   int EncodeRemainder(short int inbuffer[], int nSamples, unsigned char outbuffer[]) {
      if(!mEncoding)
         return -1;

      unsigned long bytes;
      beEncodeChunk(mStreamHandle, nSamples, inbuffer, outbuffer, &bytes);

      return bytes;
   }

   int EncodeBufferMono(short int inbuffer[], unsigned char outbuffer[]) {
      return EncodeBuffer(inbuffer, outbuffer);
   }

   int EncodeRemainderMono(short int inbuffer[], int nSamples,
                     unsigned char outbuffer[]) {
      return EncodeRemainder(inbuffer, nSamples, outbuffer);
   }

   int FinishStream(unsigned char outbuffer[]) {
      if(!mEncoding)
         return -1;

      unsigned long bytes;
      beDeinitStream(mStreamHandle, outbuffer, &bytes);
      beCloseStream(mStreamHandle);

      mEncoding = false;
      return bytes;
   }

   void CancelEncoding() {
      beCloseStream(mStreamHandle);
   }

   int GetQualityVariance() { return -1; }

   int GetConfigurationCaps() {
      return MP3CONFIG_BITRATE;
   }
   
   void SetBitrate(int rate) { 
      mDefaultRate = rate;
   }

   int GetBitrate() {
      return mDefaultRate;
   }

   void SetQuality(int quality) { }
   int GetQuality() { return -1; }

};

MP3Exporter *gMP3Exporter = NULL;

MP3Exporter *GetMP3Exporter()
{
   if (!gMP3Exporter)
      gMP3Exporter = new BladeEncExporter();
   
   return gMP3Exporter;
}

void ReleaseMP3Exporter()
{
   if( gMP3Exporter )
      delete gMP3Exporter;
   gMP3Exporter = NULL;
}

#endif  

class MP3ExporterCleanup
{
public:
   MP3ExporterCleanup(){};
   ~MP3ExporterCleanup(){ ReleaseMP3Exporter();}
};

// Create instance of this cleanup class purely to clean up 
// the exporter.
// No one will reference this variable, but when the program
// exits it will clean up any allocated MP3 exporter.
MP3ExporterCleanup gMP3ExporterCleanup;


bool ExportMP3(AudacityProject *project,
               bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1)
{
   double rate = project->GetRate();
   wxWindow *parent = project;
   TrackList *tracks = project->GetTracks();

   wxLogNull logNo;             /* temporarily disable wxWindows error messages */

   bool success = GetMP3Exporter()->FindLibrary(parent);
   
   if (!success)
      return false;

   success = GetMP3Exporter()->LoadLibrary();
   if (!success) {
      wxMessageBox(_("Could not open MP3 encoding library!"));
      gPrefs->Write("/MP3/MP3LibPath", wxString(""));

      return false;
   }

   if(!GetMP3Exporter()->ValidLibraryLoaded()) {
      wxMessageBox(_("Not a valid or supported MP3 encoding library!"));      
      gPrefs->Write("/MP3/MP3LibPath", wxString(""));
      
      return false;
   }
   
   /* Open file for writing */

   wxFFile outFile(FILENAME(fName), "wb");
   if (!outFile.IsOpened()) {
      wxMessageBox(_("Unable to open target file for writing"));
      return false;
   }
   
   /* Put ID3 tags at beginning of file */
   
   Tags *tags = project->GetTags();
   if (tags->IsEmpty()) {
      if (!tags->ShowEditDialog(project,
                                _("Edit the ID3 tags for the MP3 file")))
         return false;  // used selected "cancel"
   }

   char *id3buffer = NULL;
   int id3len;
   bool endOfFile;
   id3len = tags->ExportID3(&id3buffer, &endOfFile);
   if (!endOfFile)
     outFile.Write(id3buffer, id3len);

   /* Export MP3 using DLL */

   long bitrate = gPrefs->Read("/FileFormats/MP3Bitrate", 128);
   GetMP3Exporter()->SetBitrate(bitrate);

   sampleCount inSamples = GetMP3Exporter()->InitializeStream(stereo ? 2 : 1, int(rate + 0.5));

   wxProgressDialog *progress = NULL;
   wxYield();
   wxStartTimer();
   wxBusyCursor busy;
   bool cancelling = false;
   long bytes;

   int bufferSize = GetMP3Exporter()->GetOutBufferSize();
   unsigned char *buffer = new unsigned char[bufferSize];
   wxASSERT(buffer);

   int numWaveTracks;
   WaveTrack **waveTracks;
   tracks->GetWaveTracks(selectionOnly, &numWaveTracks, &waveTracks);
   Mixer *mixer = new Mixer(numWaveTracks, waveTracks,
                            tracks->GetTimeTrack(),
                            t0, t1,
                            stereo? 2: 1, inSamples, true,
                            rate, int16Sample);

   while(!cancelling) {
      sampleCount blockLen = mixer->Process(inSamples);

      if (blockLen == 0)
         break;
      
      short *mixed = (short *)mixer->GetBuffer();

      if(blockLen < inSamples) {
         if (stereo)
            bytes = GetMP3Exporter()->EncodeRemainder(mixed,  blockLen , buffer);
         else
            bytes = GetMP3Exporter()->EncodeRemainderMono(mixed,  blockLen , buffer);
      }
      else {
         if (stereo)
            bytes = GetMP3Exporter()->EncodeBuffer(mixed, buffer);
         else
            bytes = GetMP3Exporter()->EncodeBufferMono(mixed, buffer);
      }

      outFile.Write(buffer, bytes);

      if (!progress && wxGetElapsedTime(false) > 500) {

         wxString message;

         if (selectionOnly)
            message =
                wxString::Format(_("Exporting the selected audio as an mp3"));
         else
            message =
                wxString::Format(_("Exporting the entire project as an mp3"));

         progress =
             new wxProgressDialog(_("Export"),
                                  message,
                                  1000,
                                  parent,
                                  wxPD_CAN_ABORT |
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);
      }

      if (progress) {
         int progressvalue = int (1000 * ((mixer->MixGetCurrentTime()-t0) /
                                          (t1-t0)));
         cancelling = !progress->Update(progressvalue);
      }

   }

   delete mixer;

   bytes = GetMP3Exporter()->FinishStream(buffer);

   if (bytes)
      outFile.Write(buffer, bytes);
   
   /* Write ID3 tag if it was supposed to be at the end of the file */
   
   if (endOfFile)
      outFile.Write(id3buffer, id3len);
   free(id3buffer);

   /* Close file */
   
   outFile.Close();
      
   /* MacOS: set the file type/creator so that the OS knows it's an MP3
      file which was created by Audacity */
      
#ifdef __WXMAC__
   FSSpec spec;
   wxMacFilename2FSSpec(FILENAME(fName), &spec);
   FInfo finfo;
   if (FSpGetFInfo(&spec, &finfo) == noErr) {
      finfo.fdType = 'MP3 ';
      finfo.fdCreator = AUDACITY_CREATOR;

      FSpSetFInfo(&spec, &finfo);
   }
#endif

   if (progress)
      delete progress;

   delete[]buffer;
   
   return true;
}
