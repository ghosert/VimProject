/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_HISTORY_WINDOW__
#define __AUDACITY_HISTORY_WINDOW__

#include <wx/dialog.h>

class wxBoxSizer;
class wxListCtrl;
class wxSpinCtrl;
class wxButton;
class wxStaticText;
class wxCommandEvent;
class wxListEvent;

class AudacityProject;
class UndoManager;

class HistoryWindow :public wxFrame {

 public:
   HistoryWindow(AudacityProject * parent, UndoManager *manager);
   ~HistoryWindow();

   void UpdateDisplay();

 private:

   void OnDiscard(wxCommandEvent & event);
   void OnLabelChanged(wxListEvent & event);
   void OnItemSelected(wxListEvent & event);
   void OnCloseWindow(wxCloseEvent & WXUNUSED(event));
   AudacityProject *mProject;
   
   wxBoxSizer *mTopSizer;
   wxListCtrl *mList;
   wxSpinCtrl *mDiscardNum;
   wxButton   *mDiscard;
   wxStaticText *mLevelsAvailable;
   UndoManager *mManager;

   int mSelected;

 public:
   DECLARE_EVENT_TABLE()
};

#endif
