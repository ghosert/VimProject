/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIOPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_AUDIO_IO_PREFS__
#define __AUDACITY_AUDIO_IO_PREFS__

#include <wx/string.h>

#include "PrefsPanel.h"

class wxWindow;
class wxCheckBox;
class wxChoice;

class AudioIOPrefs:public PrefsPanel {

 public:
   AudioIOPrefs(wxWindow * parent);
   ~AudioIOPrefs();
   bool Apply();

 private:
   wxString  mRecDevice;
   wxString  mPlayDevice;
   
   wxCheckBox *mDuplex;

   wxCheckBox *mPlaythrough; // Hardware playthrough, currently Mac OS X only
   wxCheckBox *mSWPlaythrough;
   wxCheckBox *mNoModifyDevice; // Mac OS X only

   wxChoice *mRecChoice;
   wxChoice *mPlayChoice;

   wxChoice *mChannelsChoice;

   wxString *mPlayNames;
   wxString *mRecNames;

 public:
    DECLARE_EVENT_TABLE()

};

#endif
