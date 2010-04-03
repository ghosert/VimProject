/**********************************************************************

  Audacity: A Digital Audio Editor

  Prefs.cpp

  Dominic Mazzoni

  Audacity uses wxWindows' wxConfig class to handle preferences.
  See Prefs.h for more information on how it works...

  Preference field specification:
  	/
		Version					- Audacity Version that created these prefs
		DefaultOpenPath			- Default directory for new file selector
	/FileFormats
		CopyOrEditUncompressedData - Copy data from uncompressed files or
			[ "copy", "edit"]   - edit in place?
		ExportFormat_SF1		   - Format to export PCM data in
                             (this number is a libsndfile1.0 format)
	/SamplingRate
		DefaultProjectSampleRate- New projects will have this rate
			[ 8000, 11025, 16000, 22050, 44100, 48000 ]
	/AudioIO
		PlaybackDevice			- device to use for playback
		RecordingDevice			- device to use for recording
			(these are device names understood by PortAudio)
	/Display
		WaveformColor			- 0xRRGGBB  --since it will be stored in
		ShadowColor				- 			  decimal, it will be somewhat
		SpectrumLowColor		- 			  non-intuitive to edit, but
		SpectrumHighColor		- 			  much easier to parse.
	/Locale
		Language				- two-letter language code for translations

	(*): wxGTK
	(+): wxWin
	($): wxMac

**********************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/app.h>
#include <wx/config.h>
#include <wx/intl.h>

#include "sndfile.h"

#ifdef __MACOSX__
#include <CoreServices/CoreServices.h>
#endif

#ifdef __MACOS9__
#include <Files.h>
#include <Folders.h>
#endif

#ifdef __WXMAC__
/* prototype of MoreFiles fn, included in wxMac already */
pascal OSErr FSpGetFullPath(const FSSpec * spec,
                            short *fullPathLength, Handle * fullPath);
#endif

#include "Audacity.h"
#include "Prefs.h"

wxConfig *gPrefs = NULL;
int gMenusDirty = 0;

void InitPreferences()
{
#ifdef AUDACITY_NAME
   wxString appName = AUDACITY_NAME;
   wxString vendorName = AUDACITY_NAME;
#else
   wxString vendorName = "Audacity";
   wxString appName = "Audacity";
#endif

   wxTheApp->SetVendorName(vendorName);
   wxTheApp->SetAppName(appName);

   gPrefs = new wxConfig(appName);
   wxConfigBase::Set(gPrefs);

#ifdef __WXMAC__
#ifndef __UNIX__
   // This fixes changes in Mac filenames under wxWindows between versions
   // 0.95 and 0.96 of Audacity.
   wxString path;
   bool fix = false;   
   path = gPrefs->Read("/DefaultOpenPath", "");
   if (path.Length() > 0 && path.Left(1)=="/")
      fix = true;
   path = gPrefs->Read("/DefaultExportPath", "");
   if (path.Length() > 0 && path.Left(1)=="/")
      fix = true;
   path = gPrefs->Read("/Directories/TempDir", "");
   if (path.Length() > 0 && path.Left(1)=="/")
      fix = true;
   if (fix) {
      gPrefs->Write("/DefaultOpenPath", FROMFILENAME(::wxGetCwd()));
      gPrefs->Write("/DefaultExportPath", FROMFILENAME(::wxGetCwd()));
      gPrefs->Write("/Directories/TempDir", "");
      wxMessageBox(_("Some of your preferences were from an earlier version "
                     "of Audacity and have been reset."));
   }
#endif
#endif

   gPrefs->Write("/Version", (wxString)AUDACITY_VERSION_STRING);

   // BG: Make sure the users prefs are up to date
   // BG: Otherwise reset some of them to their defaults
   wxString prefsversion;
   prefsversion = gPrefs->Read("/PrefsVersion", "");

   if(prefsversion.CmpNoCase((wxString)AUDACITY_PREFS_VERSION_STRING))
   {
      // BG: Reset the prefs by removing them
      if(gPrefs->Exists("/Keyboard"))
         gPrefs->DeleteGroup("/Keyboard");
      if(gPrefs->Exists("/Locale"))
         gPrefs->DeleteGroup("/Locale");
      gPrefs->Write("/PrefsVersion", (wxString)AUDACITY_PREFS_VERSION_STRING);
   }
}

void FinishPreferences()
{
   if (gPrefs) {
      wxConfigBase::Set(NULL);
      delete gPrefs;
      gPrefs = NULL;
   }
}

int ReadExportFormatPref()
{
   return gPrefs->Read("/FileFormats/ExportFormat_SF1",
                       (long int)(SF_FORMAT_WAV | SF_FORMAT_PCM_16));
}

void WriteExportFormatPref(int format)
{
   gPrefs->Write("/FileFormats/ExportFormat_SF1", (long int)format);
}
