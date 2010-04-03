/**********************************************************************

  Audacity: A Digital Audio Editor

  Repeat.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_REPEAT__
#define __AUDACITY_EFFECT_REPEAT__

class wxString;

#include <wx/intl.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>

#include "Effect.h"

class WaveTrack;

class EffectRepeat:public Effect {

 public:

   EffectRepeat();

   virtual wxString GetEffectName() {
      return wxString(_("Repeat..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Performing Repeat"));
   }
   
   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription(); 

   virtual bool PromptUser();
   
   virtual bool Process();

 private:
   int repeatCount;
};

class RepeatDialog:public wxDialog {
 public:
   // constructors and destructors
   RepeatDialog(wxWindow * parent, wxWindowID id, const wxString & title);

   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

   wxTextCtrl   *mRepeatCount;
   wxStaticText *mTotalTime;

 private:
   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnRepeatTextChange(wxCommandEvent & event);

   void DisplayNewTime();

 private:
   DECLARE_EVENT_TABLE()

 public:
   int repeatCount;
   int maxCount;
   double selectionTimeSecs;
};

#endif
