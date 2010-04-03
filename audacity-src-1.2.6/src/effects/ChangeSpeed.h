/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.h

  Vaughan Johnson, Dominic Mazzoni
  
  Change Speed effect, that affects both pitch & tempo.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CHANGESPEED__
#define __AUDACITY_EFFECT_CHANGESPEED__

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>

#include <wx/intl.h>

#include "Effect.h"
#include "../Resample.h"

class wxString;

class EffectChangeSpeed : public Effect {

 public:
   EffectChangeSpeed();

   virtual wxString GetEffectName() {
      return wxString(_("Change Speed..."));
   }
   virtual wxString GetEffectAction() {
      return wxString(_("Changing Speed"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

 protected:
   virtual bool PromptUser();
   virtual bool Process();

 private:
   bool ProcessOne(WaveTrack * t,
                   longSampleCount start, longSampleCount end);

 private:
	// track related
   int    mCurTrackNum;
	double m_maxNewLength;

	// control values
   double	m_PercentChange;	// percent change to apply to tempo
										// -100% is meaningless, but sky's the upper limit.
										// Slider is (-100, 200], but textCtrls can set higher.
   int		m_FromVinyl;		// from standard vinyl speed (RPM)
   int		m_ToVinyl;			// to standard vinyl speed (RPM)

   
friend class ChangeSpeedDialog;
};

//----------------------------------------------------------------------------
// ChangeSpeedDialog
//----------------------------------------------------------------------------

class ChangeSpeedDialog:public wxDialog {
 public:
   ChangeSpeedDialog(EffectChangeSpeed * effect,
							wxWindow * parent, wxWindowID id, 
							const wxString & title, 
							const wxPoint & pos = wxDefaultPosition, 
							const wxSize & size = wxDefaultSize, 
							long style = wxDEFAULT_DIALOG_STYLE);

   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
	// handlers
	void OnText_PercentChange(wxCommandEvent & event);
   void OnSlider_PercentChange(wxCommandEvent & event);
   void OnChoice_FromVinyl(wxCommandEvent & event); 
   void OnChoice_ToVinyl(wxCommandEvent & event); 

   void OnPreview(wxCommandEvent &event);
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

	// helper fns
	void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
	void Update_Vinyl(); // Update Vinyl controls for new percent change.
	void Update_PercentChange(); // Update percent change controls for new Vinyl values.

 private:
	bool m_bLoopDetect;
	EffectChangeSpeed * m_pEffect;

   // controls
   wxTextCtrl *	m_pTextCtrl_PercentChange;
   wxSlider *		m_pSlider_PercentChange;
   wxChoice *		m_pChoice_FromVinyl;
   wxChoice *		m_pChoice_ToVinyl;

 public:
	// effect parameters
   double	m_PercentChange;	// percent change to apply to tempo
										// -100% is meaningless, but sky's the upper limit.
										// Slider is (-100, 200], but textCtrls can set higher.
   int		m_FromVinyl;		// from standard vinyl speed (RPM)
   int		m_ToVinyl;			// to standard vinyl speed (RPM)

 private:
   DECLARE_EVENT_TABLE()
};


#endif // __AUDACITY_EFFECT_CHANGESPEED__
