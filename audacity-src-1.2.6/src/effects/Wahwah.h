/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah

  Effect programming:
  Nasca Octavian Paul

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_WAHWAH__
#define __AUDACITY_EFFECT_WAHWAH__

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/spinbutt.h>
#include <wx/spinctrl.h>
#include <wx/intl.h>

class wxString;

#include "SimpleMono.h"

class EffectWahwah:public EffectSimpleMono {

 public:
   EffectWahwah();

   virtual wxString GetEffectName() {
      return wxString(_("Wahwah..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Applying Wahwah"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool PromptUser();
   
 protected:
   virtual bool NewTrackSimpleMono();

   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);

   float phase;
   float lfoskip;
   unsigned long skipcount;
   float xn1, xn2, yn1, yn2;
   float b0, b1, b2, a0, a1, a2;

/* Parameters:
   freq - LFO frequency 
   startphase - LFO startphase in RADIANS - usefull for stereo WahWah
   depth - Wah depth
   freqofs - Wah frequency offset
   res - Resonance

   !!!!!!!!!!!!! IMPORTANT!!!!!!!!! :
   depth and freqofs should be from 0(min) to 1(max) !
   res should be greater than 0 !  */

 private:
   float freq, startphase;
   float depth, freqofs, res;

friend class WahwahDialog;
};

// Declare window functions

#define ID_BUTTON_PREVIEW 10000
#define ID_STAGES 10001
#define ID_DRYWET 10002
#define ID_FREQTEXT 10003
#define ID_FREQSLIDER 10004
#define ID_PHASETEXT 10005
#define ID_PHASESLIDER 10006
#define ID_DEPTHTEXT 10007
#define ID_DEPTHSLIDER 10008
#define ID_RESONANCETEXT 10009
#define ID_RESONANCESLIDER 10010
#define ID_FREQOFFTEXT 10011
#define ID_FREQOFFSLIDER 10012

wxSizer *CreateWahwahDialog(wxWindow * parent, bool call_fit =
                            TRUE, bool set_sizer = TRUE);

// WDR: class declarations

//----------------------------------------------------------------------------
// WahwahDialog
//----------------------------------------------------------------------------

class WahwahDialog:public wxDialog {
 public:
   // constructors and destructors
   WahwahDialog(EffectWahwah * effect, 
						wxWindow * parent, wxWindowID id, const wxString & title,
						const wxPoint & pos = wxDefaultPosition,
						const wxSize & size = wxDefaultSize,
						long style = wxDEFAULT_DIALOG_STYLE);

   // WDR: method declarations for WahwahDialog
   wxSlider *GetResonanceSlider() {
      return (wxSlider *) FindWindow(ID_RESONANCESLIDER);
   } wxSlider *GetDepthSlider() {
      return (wxSlider *) FindWindow(ID_DEPTHSLIDER);
   }
   wxSlider *GetPhaseSlider() {
      return (wxSlider *) FindWindow(ID_PHASESLIDER);
   }
   wxSlider *GetFreqSlider() {
      return (wxSlider *) FindWindow(ID_FREQSLIDER);
   }
   wxSlider *GetFreqOffSlider() {
      return (wxSlider *) FindWindow(ID_FREQOFFSLIDER);
   }
   wxTextCtrl *GetResonanceText() {
      return (wxTextCtrl *) FindWindow(ID_RESONANCETEXT);
   }
   wxTextCtrl *GetDepthText() {
      return (wxTextCtrl *) FindWindow(ID_DEPTHTEXT);
   }
   wxTextCtrl *GetPhaseText() {
      return (wxTextCtrl *) FindWindow(ID_PHASETEXT);
   }
   wxTextCtrl *GetFreqText() {
      return (wxTextCtrl *) FindWindow(ID_FREQTEXT);
   }
   wxTextCtrl *GetFreqOffText() {
      return (wxTextCtrl *) FindWindow(ID_FREQOFFTEXT);
   }
   wxSlider *GetDryWet() {
      return (wxSlider *) FindWindow(ID_DRYWET);
   }
   wxSpinCtrl *GetStages() {
      return (wxSpinCtrl *) FindWindow(ID_STAGES);
   }
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
   // WDR: member variable declarations for WahwahDialog

 private:
   // WDR: handler declarations for WahwahDialog
   void OnResonanceSlider(wxCommandEvent & event);
   void OnDepthSlider(wxCommandEvent & event);
   void OnPhaseSlider(wxCommandEvent & event);
   void OnFreqSlider(wxCommandEvent & event);
   void OnFreqOffSlider(wxCommandEvent & event);
   void OnResonanceText(wxCommandEvent & event);
   void OnDepthText(wxCommandEvent & event);
   void OnPhaseText(wxCommandEvent & event);
   void OnFreqText(wxCommandEvent & event);
   void OnFreqOffText(wxCommandEvent & event);
   void OnPreview(wxCommandEvent &event);
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
	EffectWahwah * m_pEffect;

 public:
   float freq;
   float freqoff;
   float startphase;
   float res;
   float depth;

 private:
   DECLARE_EVENT_TABLE()
};

#endif
