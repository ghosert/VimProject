/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.h

  Brian Gunlogson
  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_GUI_PREFS__
#define __AUDACITY_GUI_PREFS__

#include <wx/defs.h>
#include <wx/string.h>

#include "PrefsPanel.h"

class wxWindow;
class wxCheckBox;
class wxChoice;
class wxTextCtrl;
class wxStaticText;
class wxRadioButton;

class GUIPrefs : public PrefsPanel {

 public:
   GUIPrefs(wxWindow * parent);
   ~GUIPrefs();
   bool Apply();

 private:
    wxCheckBox *mAutoscroll;
    wxCheckBox *mAlwaysEnablePause;
    wxCheckBox *mAlwaysEnablePlay;
    wxCheckBox *mSpectrogram;
    wxCheckBox *mEditToolBar;
    wxCheckBox *mMixerToolBar;
    wxCheckBox *mMeterToolBar;
    wxCheckBox *mQuitOnClose;
    wxCheckBox *mAdjustSelectionEdges;
    wxCheckBox *mErgonomicTransportButtons;
    wxRadioButton *mdBArray[5];

    wxChoice *mLocale;
    wxStaticText *mLocaleLabel;

    wxArrayString mLangCodes;
    wxArrayString mLangNames;
};

#endif
