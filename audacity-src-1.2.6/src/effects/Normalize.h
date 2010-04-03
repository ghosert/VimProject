/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NORMALIZE__
#define __AUDACITY_EFFECT_NORMALIZE__

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/intl.h>

#include "Effect.h"

class wxString;
class WaveTrack;

class EffectNormalize: public Effect {
   
public:
   
   EffectNormalize();
   
   virtual wxString GetEffectName() {
      return wxString(_("Normalize..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Normalizing..."));
   }
   
   virtual bool PromptUser();

   virtual bool Process();
   
 private:
   bool ProcessOne(WaveTrack * t,
                   longSampleCount start, longSampleCount end);

   virtual void StartAnalysis();
   virtual void AnalyzeData(float *buffer, sampleCount len);

   virtual void StartProcessing();
   virtual void ProcessData(float *buffer, sampleCount len);

   bool   mGain;
   bool   mDC;

   int    mCurTrackNum;
   double mCurRate;
   double mCurT0;
   double mCurT1;
   int    mCurChannel;
   float mMult;
   float mOffset;
   float mMin;
   float mMax;
   double mSum;
   int mCount;

friend class NormalizeDialog;
};

//----------------------------------------------------------------------------
// NormalizeDialog
//----------------------------------------------------------------------------

class NormalizeDialog: public wxDialog
{
public:
   // constructors and destructors
   NormalizeDialog( EffectNormalize *effect,
                    wxWindow *parent, wxWindowID id, const wxString &title,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize,
                    long style = wxDEFAULT_DIALOG_STYLE );
   
   bool mGain;
   bool mDC;
   
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   
   void OnPreview(wxCommandEvent &event);
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );

   EffectNormalize *mEffect;
   wxCheckBox *mGainCheckBox;
   wxCheckBox *mDCCheckBox;

private:
   DECLARE_EVENT_TABLE()
};

#endif

