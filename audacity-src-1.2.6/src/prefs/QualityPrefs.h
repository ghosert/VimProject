/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_RATE_PREFS__
#define __AUDACITY_SAMPLE_RATE_PREFS__

#include "PrefsPanel.h"

class wxChoice;
class wxTextCtrl;

class QualityPrefs:public PrefsPanel {

 public:
   QualityPrefs(wxWindow * parent);
   ~QualityPrefs();

   void OnSampleRateChoice(wxCommandEvent& evt);

   bool Apply();

 private:
   wxChoice *mSampleRates;
   wxChoice *mSampleFormats;
   wxTextCtrl *mOtherSampleRate;

   wxChoice *mConverters;
   wxChoice *mHQConverters;

   wxChoice *mDithers;
   wxChoice *mHQDithers;

 public:
   DECLARE_EVENT_TABLE();
};

#endif
