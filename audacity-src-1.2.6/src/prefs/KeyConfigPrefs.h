/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.h

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_KEY_CONFIG_PREFS__
#define __AUDACITY_KEY_CONFIG_PREFS__

#include <wx/defs.h>
#include <wx/textctrl.h>
#include <wx/string.h>

#include "PrefsPanel.h"
#include "../Project.h"

class wxChoice;
class wxCharEvent;
class wxStaticText;
class wxListCtrl;
class wxListEvent;
class wxWindow;

class SysKeyTextCtrl;
class CommandManager;

class KeyConfigPrefs:public PrefsPanel {

 public:
   KeyConfigPrefs(wxWindow * parent);
   ~KeyConfigPrefs();
   bool Apply();

 private:
   void OnDefaults(wxCommandEvent& event);
   void OnLoad(wxCommandEvent& event);
   void OnSave(wxCommandEvent& event);
   void OnSet(wxCommandEvent& event);
   void OnClear(wxCommandEvent& event);
   void OnItemSelected(wxListEvent &event);
   void RepopulateBindingsList();

   SysKeyTextCtrl *mCurrentComboText;
   wxListCtrl * mList;
   CommandManager *mManager;
   int mCommandSelected;
   wxArrayString mNames;

 public:
   DECLARE_EVENT_TABLE();
};

//BG: A quick and dirty override of wxTextCtrl to capture keys like Ctrl, Alt

class SysKeyTextCtrl:public wxTextCtrl
{
public:
   SysKeyTextCtrl(wxWindow *parent, wxWindowID id,
                  const wxString& value = wxEmptyString,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = 0,
                  const wxValidator& validator = wxDefaultValidator,
                  const wxString& name = wxTextCtrlNameStr);
   ~SysKeyTextCtrl();

private:
   void OnKey(wxKeyEvent& event);
   void OnChar(wxKeyEvent& event);

protected:
   DECLARE_EVENT_TABLE()
};

#endif
