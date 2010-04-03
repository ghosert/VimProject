/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_PREFS__
#define __AUDACITY_SPECTRUM_PREFS__

#include "PrefsPanel.h"

class wxRadioButton;
class wxCheckBox;
class wxTextCtrl;

class SpectrumPrefs:public PrefsPanel {

 public:
   SpectrumPrefs(wxWindow * parent);
   ~SpectrumPrefs();
   bool Apply();

 private:
   /* an arbitrary limit designed to be past what we'll ever need */
   wxRadioButton *mFFTSize[20];
   wxCheckBox *mGrayscale;

   wxTextCtrl *mMaxFreqCtrl;
};

#endif
