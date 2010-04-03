/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.h

  Steve Jolly
  
  This class implements a tone generator effect.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_TONEGEN__
#define __AUDACITY_EFFECT_TONEGEN__

#include <math.h>
#include "../FFT.h"

#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/choice.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/intl.h>

#include "SimpleMono.h"

#define __UNINITIALIZED__ (-1)

class WaveTrack;

class EffectToneGen:public Effect {

 public:
   EffectToneGen();

   virtual wxString GetEffectName() {
      return wxString(_("Tone..."));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Generating Tone"));
   }

   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | INSERT_EFFECT;
   }

   virtual bool PromptUser();

   virtual bool Process();

 protected:
   virtual bool MakeTone(float *buffer, sampleCount len);

 private:
   int waveform;
   float frequency;
   float amplitude;
   double length;
   int mSample;
   double mCurRate;
};

// Declare window functions

#define ID_TEXT 10000
#define ID_WAVEFORM 10001
#define ID_AMPTEXT 10002
#define ID_FREQTEXT 10003
#define ID_LENGTHTEXT 10005
wxSizer *CreateToneGenDialog(wxWindow * parent, bool call_fit =
                             TRUE, bool set_sizer = TRUE);

// WDR: class declarations

//----------------------------------------------------------------------------
// ToneGenDialog
//----------------------------------------------------------------------------

class ToneGenDialog:public wxDialog {
 public:
   // constructors and destructors
   ToneGenDialog(wxWindow * parent, wxWindowID id, const wxString & title,
                 const wxPoint & pos = wxDefaultPosition,
                 const wxSize & size = wxDefaultSize,
                 long style = wxDEFAULT_DIALOG_STYLE);

   wxSizer *MakeToneGenDialog(wxWindow * parent, bool call_fit = TRUE,
                              bool set_sizer = TRUE);

   wxTextCtrl *GetFreqText() {
      return (wxTextCtrl *) FindWindow(ID_FREQTEXT);
   } wxTextCtrl *GetAmpText() {
      return (wxTextCtrl *) FindWindow(ID_AMPTEXT);
   } wxTextCtrl *GetLengthText() {
      return (wxTextCtrl *) FindWindow(ID_LENGTHTEXT);
   }
   wxChoice *GetWaveformChoice() {
      return (wxChoice *) FindWindow(ID_WAVEFORM);
   }
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   wxButton *mCreateToneButton;

 private:
   // WDR: handler declarations for FilterDialog
   void OnCreateTone(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()

 public:
   int waveform;
   float frequency;
   float amplitude;
   double length;
};

#endif
