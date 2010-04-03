/**********************************************************************

  Audacity: A Digital Audio Editor

  Warning.cpp

  Dominic Mazzoni

**********************************************************************/

#include "Warning.h"

#include "../Prefs.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

class WarningDialog : public wxDialog
{
   public:
   // constructors and destructors
   WarningDialog(wxWindow *parent, 
                 wxString message);

   bool dontShow;
   
private:
   void OnOk( wxCommandEvent &event );

   wxCheckBox *mCheckBox;
   
private:
   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(WarningDialog, wxDialog)
   EVT_BUTTON( wxID_OK, WarningDialog::OnOk )
END_EVENT_TABLE()

WarningDialog::WarningDialog(wxWindow *parent, wxString message):
   wxDialog(parent, (wxWindowID)-1, (wxString)_("Warning"))
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);

   wxStaticText *statText = new wxStaticText(this, -1, message);
   vSizer->Add(statText, 0, wxALIGN_LEFT|wxALL, 5);

   mCheckBox = new wxCheckBox(this, -1, _("Don't show this warning again"));
   vSizer->Add(mCheckBox, 0, wxALIGN_LEFT|wxALL, 5);

   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   ok->SetDefault();
   ok->SetFocus();
   vSizer->Add(ok, 0, wxALIGN_CENTRE|wxALL, 5);

   mainSizer->Add(vSizer, 0, wxALL, 15);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

void WarningDialog::OnOk(wxCommandEvent &event)
{
   dontShow = mCheckBox->GetValue();
   
   EndModal(true);
}

void ShowWarningDialog(wxWindow *parent,
                       wxString internalDialogName,
                       wxString message)
{
   bool dontShow = false;

   gPrefs->SetPath("/Warnings");
   gPrefs->Read(internalDialogName, &dontShow, false);
   gPrefs->SetPath("/");

   if (dontShow)
      return;

   WarningDialog dlog(parent, message);
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.dontShow) {
      gPrefs->SetPath("/Warnings");
      gPrefs->Write(internalDialogName, true);
      gPrefs->SetPath("/");
   }
}
