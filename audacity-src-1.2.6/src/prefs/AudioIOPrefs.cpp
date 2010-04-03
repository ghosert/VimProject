/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni

  Presents interface for user to select the recording device and
  playback device, from the list of choices that PortAudio
  makes available.

  Also lets user decide whether or not to record in stereo, and
  whether or not to play other tracks while recording one (duplex).

**********************************************************************/

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/utils.h>
#include <wx/window.h>

#include "../Prefs.h"
#include "../AudioIO.h"
#include "../Project.h"
#include "../MixerToolBar.h"
#include "AudioIOPrefs.h"

#include "portaudio.h"

enum {
   RecChoiceID = 1000,
   PlayChoiceID
};

BEGIN_EVENT_TABLE(AudioIOPrefs, wxPanel)
END_EVENT_TABLE()	

AudioIOPrefs::AudioIOPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   /* read prefs all at once, then set up the dialog */
   gPrefs->SetPath("/AudioIO");
   mPlayDevice = gPrefs->Read("PlaybackDevice", "");
   mRecDevice = gPrefs->Read("RecordingDevice", "");

   long recordChannels = 1;
   gPrefs->Read("RecordChannels", &recordChannels, 1L);
   bool duplex;
   gPrefs->Read("Duplex", &duplex, false);

   #ifdef __MACOSX__
   bool playthrough;
   gPrefs->Read("Playthrough", &playthrough, false);
   #endif

   bool swplaythrough;
   gPrefs->Read("SWPlaythrough", &swplaythrough, false);

#ifdef __WXMAC__
   bool noModifyDevice;
   gPrefs->Read("NoModifyDevice", &noModifyDevice, false);
#endif

   gPrefs->SetPath("/");

   topSizer = new wxBoxSizer( wxVERTICAL );

   //
   // Playback
   //

   wxStaticBoxSizer *playbackSizer =
      new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("Playback")),
            wxVERTICAL);

   wxBoxSizer *pFileSizer = new wxBoxSizer(wxHORIZONTAL);

   int j, k;
   int playIndex = 0;
   int numDevices = 0;

   // Count the number of devices which do output
#if USE_PORTAUDIO_V19
   for(j=0; j<Pa_GetDeviceCount(); j++) {
#else
   for(j=0; j<Pa_CountDevices(); j++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxOutputChannels > 0)
         numDevices++;
   }

   mPlayNames = new wxString[numDevices];
   wxString *playLabels = new wxString[numDevices];
   k = 0;
#if USE_PORTAUDIO_V19
   for(j=0; j<Pa_GetDeviceCount(); j++) {
#else
   for(j=0; j<Pa_CountDevices(); j++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxOutputChannels > 0) {
         mPlayNames[k] = info->name;
#if USE_PORTAUDIO_V19
         playLabels[k].Printf("%s: %s",
                             Pa_GetHostApiInfo(info->hostApi)->name,
                             info->name);
#else
         playLabels[k] = info->name;
#endif

         if (mPlayNames[k] == mPlayDevice)
            playIndex = k;
         k++;
      }
   }

   mPlayChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                              numDevices, playLabels);
   mPlayChoice->SetSelection(playIndex);
   delete [] playLabels;

   pFileSizer->Add(
      new wxStaticText(this, -1, _("Device:")), 0, 
      wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   pFileSizer->Add(mPlayChoice, 1, 
      wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   playbackSizer->Add(pFileSizer, 0,
      wxGROW|wxALL, GENERIC_CONTROL_BORDER);

   topSizer->Add(playbackSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);

   //
   // Recording
   //

   wxStaticBoxSizer *recordingSizer =
      new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("Recording")),
            wxVERTICAL);

   wxBoxSizer *rFileSizer = new wxBoxSizer(wxHORIZONTAL);

   int recIndex = 0;
   numDevices = 0;

   // Count the number of devices which do input
#if USE_PORTAUDIO_V19
   for(j=0; j<Pa_GetDeviceCount(); j++) {
#else
   for(j=0; j<Pa_CountDevices(); j++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxInputChannels > 0)
         numDevices++;
   }

   mRecNames = new wxString[numDevices];
   wxString *recLabels = new wxString[numDevices];
   k = 0;
#if USE_PORTAUDIO_V19
   for(j=0; j<Pa_GetDeviceCount(); j++) {
#else
   for(j=0; j<Pa_CountDevices(); j++) {
#endif
      const PaDeviceInfo* info = Pa_GetDeviceInfo(j);
      if (info->maxInputChannels > 0) {
         mRecNames[k] = info->name;
#if USE_PORTAUDIO_V19
         recLabels[k].Printf("%s: %s",
                             Pa_GetHostApiInfo(info->hostApi)->name,
                             info->name);
#else
         recLabels[k] = info->name;
#endif
         if (mRecNames[k] == mRecDevice)
            recIndex = k;
         k++;
      }
   }

   mRecChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                              numDevices, recLabels);
   mRecChoice->SetSelection(recIndex);
   delete[] recLabels;

   rFileSizer->Add(
      new wxStaticText(this, -1, _("Device:")), 0, 
      wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   rFileSizer->Add(mRecChoice, 1, 
      wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   recordingSizer->Add(rFileSizer, 0,
      wxGROW|wxALL, GENERIC_CONTROL_BORDER);

   rFileSizer = new wxBoxSizer(wxHORIZONTAL);

   const int numChannels = 16;
   wxString channelNames[16];
   for(int c=0; c<numChannels; c++)
      channelNames[c] = wxString::Format("%d", c+1);
   channelNames[0] = wxString::Format(_("1 (Mono)"));
   channelNames[1] = wxString::Format(_("2 (Stereo)"));

   mChannelsChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                                  numChannels, channelNames);
   mChannelsChoice->SetSelection(recordChannels-1);

   rFileSizer->Add(
      new wxStaticText(this, -1, _("Channels:")), 0, 
      wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER);

   rFileSizer->Add(mChannelsChoice, 1, 
      wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   recordingSizer->Add(rFileSizer, 0,
      wxGROW|wxALL, GENERIC_CONTROL_BORDER);

   topSizer->Add(recordingSizer, 0, wxALL|wxGROW, TOP_LEVEL_BORDER);

   mDuplex = new wxCheckBox(this, -1,
                            _("Play other tracks while recording new one"));
   mDuplex->SetValue(duplex);
   topSizer->Add(mDuplex, 0, wxGROW|wxALL, 2);

   #ifdef __MACOSX__
   mPlaythrough = new wxCheckBox(this, -1,
                                 _("Hardware Playthrough (Play new track while recording it)"));
   mPlaythrough->SetValue(playthrough);
   topSizer->Add(mPlaythrough, 0, wxGROW|wxALL, 2);
   #endif

   mSWPlaythrough = new wxCheckBox(this, -1,
                                 _("Software Playthrough (Play new track while recording it)"));
   mSWPlaythrough->SetValue(swplaythrough);
   topSizer->Add(mSWPlaythrough, 0, wxGROW|wxALL, 2);

#ifdef __WXMAC__
   mNoModifyDevice = new wxCheckBox(this, -1,
                                    _("Do not modify audio device settings (such as sample rate)"));
   mNoModifyDevice->SetValue(noModifyDevice);
   topSizer->Add(mNoModifyDevice, 0, wxGROW|wxALL, 2);
#endif

   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
   SetSizer(outSizer);
}

AudioIOPrefs::~AudioIOPrefs()
{
   delete[] mPlayNames;
   delete[] mRecNames;
}

bool AudioIOPrefs::Apply()
{
   if (mPlayChoice->GetCount() > 0)
      mPlayDevice = mPlayNames[mPlayChoice->GetSelection()];

   if (mRecChoice->GetCount() > 0)
      mRecDevice = mRecNames[mRecChoice->GetSelection()];

   long recordChannels = mChannelsChoice->GetSelection()+1;
   bool duplex = mDuplex->GetValue();

   /* Step 2: Write to gPrefs */
   gPrefs->SetPath("/AudioIO");

   gPrefs->Write("PlaybackDevice", mPlayDevice);
   gPrefs->Write("RecordingDevice", mRecDevice);

   gPrefs->Write("RecordChannels", recordChannels);
   gPrefs->Write("Duplex", duplex);

   #ifdef __MACOSX__
   gPrefs->Write("Playthrough", mPlaythrough->GetValue());
   #endif

   gPrefs->Write("SWPlaythrough", mSWPlaythrough->GetValue());

#ifdef __WXMAC__
   gPrefs->Write("NoModifyDevice", mNoModifyDevice->GetValue());
#endif

   gPrefs->SetPath("/");

   /* Step 3: Make audio sub-system re-read preferences */

#if USE_PORTMIXER
   if (gAudioIO)
      gAudioIO->HandleDeviceChange();

   for( unsigned int i = 0; i < gAudacityProjects.GetCount(); i++ )
     {
       if(gAudacityProjects[i]->GetMixerToolBar())
	 gAudacityProjects[i]->GetMixerToolBar()->UpdateControls();
     }
#endif // USE_PORTMIXER

   return true;
}

