/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.h

  Vaughan Johnson, Dominic Mazzoni
  
  Change Tempo effect provides speeding up or 
  slowing down tempo without changing pitch.

**********************************************************************/

#if USE_SOUNDTOUCH

#ifndef __AUDACITY_EFFECT_CHANGETEMPO__
#define __AUDACITY_EFFECT_CHANGETEMPO__

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>

#include <wx/intl.h>

#include "SoundTouchEffect.h"


class wxString;


class EffectChangeTempo:public EffectSoundTouch {

 public:
   EffectChangeTempo();

   virtual wxString GetEffectName() {
      return wxString(_("Change Tempo..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Changing Tempo"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool Init();

   virtual bool PromptUser();
   virtual bool Process();
  
 private:
   double			m_PercentChange;	// percent change to apply to tempo
												// -100% is meaningless, but sky's the upper limit
   unsigned int	m_FromBPM;			// user-set beats-per-minute. Zero means not yet set.
   unsigned int	m_ToBPM;				// Zero value means not yet set.
   double			m_FromLength;		// starting length of selection
   double			m_ToLength;			// target length of selection

friend class ChangeTempoDialog;
};

//----------------------------------------------------------------------------
// ChangeTempoDialog
//----------------------------------------------------------------------------

class ChangeTempoDialog:public wxDialog {
 public:
   ChangeTempoDialog(EffectChangeTempo * effect, 
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
   void OnText_FromBPM(wxCommandEvent & event); 
   void OnText_ToBPM(wxCommandEvent & event); 
   void OnText_ToLength(wxCommandEvent & event); 

   void OnPreview( wxCommandEvent &event );
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

	// helper fns
	void Update_Text_PercentChange(); // Update control per current m_PercentChange.
   void Update_Slider_PercentChange(); // Update control per current m_PercentChange.
	void Update_Text_ToBPM(); // Use m_FromBPM & m_PercentChange to set new m_ToBPM & control.
	void Update_Text_ToLength(); // Use m_FromLength & m_PercentChange to set new m_ToLength & control.

 private:
   bool m_bLoopDetect;
	EffectChangeTempo * m_pEffect;

   // controls
   wxTextCtrl *	m_pTextCtrl_PercentChange;
   wxSlider *		m_pSlider_PercentChange;
   wxTextCtrl *	m_pTextCtrl_FromBPM;
   wxTextCtrl *	m_pTextCtrl_ToBPM;
   wxTextCtrl *	m_pTextCtrl_FromLength;
   wxTextCtrl *	m_pTextCtrl_ToLength;

 public:
	// effect parameters
   double			m_PercentChange;	// percent change to apply to tempo
												// -100% is meaningless, but sky's the upper limit.
												// Slider is (-100, 200], but textCtrls can set higher.
   unsigned int	m_FromBPM;			// user-set beats-per-minute. Zero means not yet set.
   unsigned int	m_ToBPM;				// Zero value means not yet set.
   double			m_FromLength;		// starting length of selection
   double			m_ToLength;			// target length of selection

 private:
   DECLARE_EVENT_TABLE()
};


#endif // __AUDACITY_EFFECT_CHANGETEMPO__

#endif // USE_SOUNDTOUCH
