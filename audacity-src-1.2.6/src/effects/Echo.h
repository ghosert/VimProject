/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.h

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_ECHO__
#define __AUDACITY_EFFECT_ECHO__

class wxString;

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>

#include <wx/intl.h>

#include "Effect.h"

class WaveTrack;

class EffectEcho:public Effect {

 public:

   EffectEcho();

   virtual wxString GetEffectName() {
      return wxString(_("Echo..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Performing Echo"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool PromptUser();
   
   virtual bool Process();

 private:
   bool ProcessOne(int count, WaveTrack * t,
                   longSampleCount start, sampleCount len);
 
   float delay;
   float decay;

friend class EchoDialog;
};

//----------------------------------------------------------------------------
// EchoDialog
//----------------------------------------------------------------------------

class EchoDialog:public wxDialog {
 public:
   EchoDialog(EffectEcho * effect,
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
   void OnPreview( wxCommandEvent &event );
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
	bool				m_bLoopDetect;
	EffectEcho *	m_pEffect;

   // controls
   wxTextCtrl *	m_pTextCtrl_Delay;
   wxTextCtrl *	m_pTextCtrl_Decay;

 public:
	// effect parameters
   float delay;
   float decay;

 private:
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_EFFECT_ECHO__
