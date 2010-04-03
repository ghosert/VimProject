/**********************************************************************

  Audacity: A Digital Audio Editor

  LangChoice.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include "LangChoice.h"
#include "Languages.h"

class LangChoiceDialog:public wxDialog {
public:
   LangChoiceDialog(wxWindow * parent,
                    wxWindowID id,
                    const wxString & title);
   
   wxString GetLang() { return mLang; }

private:
   void OnOk(wxCommandEvent & event);
   
   wxChoice *mChoice;
   wxString mLang;

   int mNumLangs;
   wxArrayString mLangCodes;
   wxArrayString mLangNames;
 
   DECLARE_EVENT_TABLE()
};

wxString ChooseLanguage(wxWindow *parent)
{
   wxString returnVal;

   LangChoiceDialog dlog(parent, -1, _("Audacity First Run"));
   dlog.CentreOnParent();
   dlog.ShowModal();
   returnVal = dlog.GetLang();

   return returnVal;
}

BEGIN_EVENT_TABLE(LangChoiceDialog, wxDialog)
    EVT_BUTTON(wxID_OK, LangChoiceDialog::OnOk)
END_EVENT_TABLE()

LangChoiceDialog::LangChoiceDialog(wxWindow * parent,
                                   wxWindowID id,
                                   const wxString & title):
   wxDialog(parent, id, title)
{
   GetLanguages(mLangCodes, mLangNames);
   mNumLangs = mLangNames.GetCount();

   wxString sysLang = GetSystemLanguageCode();

   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *hSizer;

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   hSizer->Add(new wxStaticText(this, -1,
                                _("Choose Language for Audacity to use:")),
               0, wxALIGN_CENTRE | wxALIGN_CENTER_VERTICAL | wxALL, 8);

   wxString *langArray = new wxString[mNumLangs];
   int i;
   for(i=0; i<mNumLangs; i++)
      langArray[i] = mLangNames[i];
   mChoice = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                          mNumLangs, langArray);
   mChoice->SetSelection(0); // in case nothing else matches
   delete[] langArray;
   for(i=0; i<mNumLangs; i++)
      if (mLangCodes[i] == sysLang)
         mChoice->SetSelection(i);
   hSizer->Add(mChoice,
               0, wxALIGN_CENTRE | wxALIGN_CENTER_VERTICAL | wxALL, 8);

   mainSizer->Add(hSizer,
                  0, wxALL, 8);

   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   ok->SetDefault();
   mainSizer->Add(ok, 0, wxALIGN_CENTRE | wxALL, 8);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

void LangChoiceDialog::OnOk(wxCommandEvent & event)
{
   mLang = mLangCodes[mChoice->GetSelection()];

   EndModal(true);
}
