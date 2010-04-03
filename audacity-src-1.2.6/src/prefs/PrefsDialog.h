/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsDialog.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_PREFS_DIALOG__
#define __AUDACITY_PREFS_DIALOG__

#include <wx/dialog.h>
#include <wx/string.h>

class wxNotebook;
class wxWindow;
class wxButton;
class wxCommandEvent;
class wxFrame;

extern bool gPrefsDialogVisible;

class PrefsDialog:public wxDialog {

 public:
   PrefsDialog(wxWindow * parent);
   ~PrefsDialog();

   void OnCategoryChange(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

   void SelectPageByName(wxString pageName);
   void ShowTempDirPage();

 private:
   wxNotebook *mCategories;
   wxButton *mOK;
   wxButton *mCancel;

   int mSelected;

   #ifdef __WXMAC__
   wxFrame *mMacHiddenFrame;
   #endif

 public:
    DECLARE_EVENT_TABLE()
};

#endif
