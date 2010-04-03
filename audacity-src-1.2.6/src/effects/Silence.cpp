/**********************************************************************

  Audacity: A Digital Audio Editor

  Silence.cpp

  Dominic Mazzoni
  
  An effect for the "Generator" menu to add silence.

**********************************************************************/

#include <wx/defs.h> 

#include <wx/button.h> 
#include <wx/sizer.h> 
#include <wx/stattext.h> 
#include <wx/textctrl.h> 

#include "Silence.h"
#include "../WaveTrack.h"

#define ID_TEXT 10000
#define ID_LENGTHTEXT 10001

bool EffectSilence::PromptUser()
{
   if (mT1 > mT0)
      length = mT1 - mT0;

   GenerateDialog dlog(mParent, -1, _("Generate Silence"));
   dlog.length = length;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == 0)
      return false;

   length = dlog.length;
   return true;
}

bool EffectSilence::Process()
{
   if (length <= 0.0)
      length = sDefaultGenerateLen;

   //Iterate over each track
   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   while (track) {
      if (track->GetSelected()) {
         track->Clear(mT0, mT1);
         track->InsertSilence(mT0, length);
      }
      
      //Iterate to the next track
      track = iter.Next();
   }

	mT1 = mT0 + length; // Update selection.
   return true;
}

BEGIN_EVENT_TABLE(GenerateDialog, wxDialog)
   EVT_BUTTON(wxID_OK, GenerateDialog::OnCreateSilence)
   EVT_BUTTON(wxID_CANCEL, GenerateDialog::OnCancel)
END_EVENT_TABLE()

GenerateDialog::GenerateDialog(wxWindow * parent, wxWindowID id, const wxString & action, const wxPoint & position, const wxSize & size, long style):
wxDialog(parent, id, action, position, size, style)
{
   CreateGenerateDialog(action, this, TRUE);
}

bool GenerateDialog::Validate()
{
   return TRUE;
}

bool GenerateDialog::TransferDataToWindow()
{
   wxTextCtrl *text = (wxTextCtrl *) FindWindow(ID_LENGTHTEXT);
   if (text) {
      wxString str;
      str.Printf("%.6lf", length);
      text->SetValue(str);
      text->SetSelection(-1, -1);
      text->SetFocus();
   }
   return TRUE;
}

bool GenerateDialog::TransferDataFromWindow()
{
   wxTextCtrl *t = (wxTextCtrl *) FindWindow(ID_LENGTHTEXT);
   if (t) {
      t->GetValue().ToDouble(&length);
   }
   return TRUE;
}

// WDR: handler implementations for GenerateDialog

void GenerateDialog::OnCreateSilence(wxCommandEvent & event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void GenerateDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

wxSizer *CreateGenerateDialog(const wxString &action, wxWindow * parent, bool call_fit,
                             bool set_sizer)
{
   wxBoxSizer *item0 = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *item2 = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item9 = new wxStaticText(parent, ID_TEXT, _("Length (seconds)"),
         wxDefaultPosition, wxDefaultSize, 0);
   item2->Add(item9, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item10 = new wxTextCtrl(parent, ID_LENGTHTEXT, "",
         wxDefaultPosition, wxSize(120, -1), 0);
   item2->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item2, 1, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *item11 = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item13 =
       new wxButton(parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item11->Add(item13, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item12 =
       new wxButton(parent, wxID_OK, action, wxDefaultPosition,
                    wxDefaultSize, 0);
   item12->SetDefault();
   item12->SetFocus();
   item11->Add(item12, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item11, 0, wxALIGN_CENTRE | wxALL, 5);

   if (set_sizer) {
      parent->SetAutoLayout(TRUE);
      parent->SetSizer(item0);
      if (call_fit) {
         item0->Fit(parent);
         item0->SetSizeHints(parent);
      }
   }

   return item0;
}
