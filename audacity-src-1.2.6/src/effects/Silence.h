/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.h

  Dominic Mazzoni
  
  An effect for the "Generator" menu to add silence.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_SILENCE__
#define __AUDACITY_EFFECT_SILENCE__

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/intl.h>

#include "Effect.h"

class wxSizer;
class wxTextCtrl;

class EffectSilence:public Effect {

 public:
   EffectSilence() {
      length = sDefaultGenerateLen;
   }

   virtual wxString GetEffectName() {
      return wxString(_("Silence..."));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Generating Silence"));
   }

   // Useful only after PromptUser values have been set. 
   virtual wxString GetEffectDescription() { 
      return wxString::Format(_("Applied effect: Generate Silence, %.6lf seconds"), length); 
   } 

   virtual int GetEffectFlags() {
      return BUILTIN_EFFECT | INSERT_EFFECT;
   }

   virtual bool PromptUser();

   virtual bool Process();

 private:
   double length;
};

wxSizer *CreateGenerateDialog(const wxString &action, wxWindow * parent, bool call_fit =
                             TRUE, bool set_sizer = TRUE);

class GenerateDialog:public wxDialog {
 public:
   // constructors and destructors
   GenerateDialog(wxWindow * parent, wxWindowID id, const wxString & action,
                 const wxPoint & pos = wxDefaultPosition,
                 const wxSize & size = wxDefaultSize,
                 long style = wxDEFAULT_DIALOG_STYLE);

   wxSizer *MakeGenerateDialog(wxWindow * parent, bool call_fit = TRUE,
                              bool set_sizer = TRUE);
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
   // WDR: handler declarations for FilterDialog
   void OnCreateSilence(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()

 public:
   double length;
};

#endif
