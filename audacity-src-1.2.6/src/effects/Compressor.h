/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

class wxString;

#include <wx/defs.h>
#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/slider.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/intl.h>
#include "TwoPassSimpleMono.h"

class WaveTrack;

class EffectCompressor: public EffectTwoPassSimpleMono {
   
public:
   
   EffectCompressor();
   
   virtual wxString GetEffectName() {
      return wxString(_("Compressor..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Applying Dynamic Range Compression..."));
   }
   
   virtual bool PromptUser();

 protected:
   virtual bool ProcessPass1(float *buffer, sampleCount len);
   virtual bool ProcessPass2(float *buffer, sampleCount len);

 private:

   virtual bool NewTrackPass1();
   virtual bool InitPass1();
   virtual bool InitPass2();

   float AvgCircle(float x);
   void Follow(float x, double *outEnv, int maxBack);
   float DoCompression(float x, double env);
   
   double    mAttackTime;
   double    mThresholdDB;
   double    mRatio;
   bool      mNormalize;	//MJS
   
   double    mDecayTime;
   double    mAttackFactor;
   double    mDecayFactor;
   double    mFloor;
   double    mThreshold;
   double    mGain;
   double    mRMSSum;
   int       mCircleSize;
   int       mCirclePos;
   double   *mCircle;
   double   *mLevelCircle;
   double    mLastLevel;

   double    mMax;			//MJS

   friend class CompressorDialog;
};

class CompressorPanel: public wxPanel
{
public:
   CompressorPanel( wxWindow *parent, wxWindowID id, 
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize);

   void OnPaint(wxPaintEvent & event);

   double threshold;
   double ratio;

private:

   wxBitmap *mBitmap;
   wxRect mEnvRect;
   int mWidth;
   int mHeight;

   DECLARE_EVENT_TABLE()
};

// WDR: class declarations

//----------------------------------------------------------------------------
// CompressorDialog
//----------------------------------------------------------------------------

class CompressorDialog: public wxDialog
{
public:
   // constructors and destructors
   CompressorDialog( EffectCompressor *effect,
                     wxWindow *parent, wxWindowID id, const wxString &title,
                     const wxPoint& pos = wxDefaultPosition,
                     const wxSize& size = wxDefaultSize,
                     long style = wxDEFAULT_DIALOG_STYLE );

   double threshold;
   double ratio;
   double attack;
   bool useGain;
   
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   
private:
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void OnSize( wxSizeEvent &event );
   void OnSlider( wxCommandEvent &event );
   void OnPreview( wxCommandEvent &event );

   EffectCompressor *mEffect;
   CompressorPanel *mPanel;
   wxSlider *mThresholdSlider;
   wxSlider *mRatioSlider;
   wxSlider *mAttackSlider;
   wxCheckBox *mGainCheckBox;
   wxStaticText *mThresholdText;
   wxStaticText *mRatioText;
   wxStaticText *mAttackText;
   
private:
   DECLARE_EVENT_TABLE()
};

#endif

