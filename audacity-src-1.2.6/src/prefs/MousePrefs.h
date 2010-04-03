/**********************************************************************

  Audacity: A Digital Audio Editor

  MousePrefs.h

**********************************************************************/

#ifndef __AUDACITY_MOUSE_PREFS__
#define __AUDACITY_MOUSE_PREFS__

#include <wx/string.h>

#include "PrefsPanel.h"

class wxWindow;
class wxCheckBox;
class wxChoice;
class wxListCtrl;

class MousePrefs:public PrefsPanel {

 public:
   MousePrefs(wxWindow * parent);
   ~MousePrefs();
   void AddItem( wxString const & MouseButtons, wxString const & Tool, wxString const & Action );
   bool Apply();

 private:
    wxListCtrl * mList;

 public:
    DECLARE_EVENT_TABLE()

};

#endif
