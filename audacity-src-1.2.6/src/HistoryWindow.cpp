/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/defs.h>
#include <wx/log.h>
#include <wx/button.h>
#include <wx/event.h>
#include <wx/image.h>
#include <wx/imaglist.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/intl.h>
#include <wx/frame.h>

#include "../images/Arrow.xpm"
#include "HistoryWindow.h"
#include "UndoManager.h"
#include "Project.h"

enum {
   HistoryListID = 1000,
   DiscardID
};

BEGIN_EVENT_TABLE(HistoryWindow, wxFrame)
   EVT_CLOSE(HistoryWindow::OnCloseWindow)
   EVT_LIST_END_LABEL_EDIT(HistoryListID, HistoryWindow::OnLabelChanged)
   EVT_LIST_ITEM_SELECTED(HistoryListID, HistoryWindow::OnItemSelected)
   EVT_BUTTON(DiscardID, HistoryWindow::OnDiscard)
END_EVENT_TABLE()

HistoryWindow::HistoryWindow(AudacityProject *parent, UndoManager *manager):
  wxFrame(parent, -1, _("Undo History"), wxDefaultPosition,  wxDefaultSize)

{
   mTopSizer = new wxBoxSizer(wxVERTICAL);

   mList = new wxListCtrl(this, HistoryListID, wxDefaultPosition, wxSize(350, 180),
                          wxLC_REPORT | wxLC_HRULES | wxLC_VRULES | wxSUNKEN_BORDER  /* | wxLC_EDIT_LABELS */);
   mList->SetSizeHints(350, 180);

   wxImageList *imageList = new wxImageList(9, 16);
   imageList->Add(wxIcon(empty_9x16_xpm));
   imageList->Add(wxIcon(arrow_xpm));
   mList->SetImageList(imageList, wxIMAGE_LIST_SMALL);
   mList->InsertColumn(0, _("Action"), wxLIST_FORMAT_LEFT, 280);
   mList->InsertColumn(1, _("Size"), wxLIST_FORMAT_LEFT, 66);

   mTopSizer->Add(mList, 1, wxGROW|wxALL, 2);


   // "Discard" cluster
   wxStaticBoxSizer *purgeSizer = new wxStaticBoxSizer(
            new wxStaticBox(this, -1, _("Discard undo data")),
            wxVERTICAL);

   wxBoxSizer *firstLine = new wxBoxSizer(wxHORIZONTAL);

   purgeSizer->Add(
      mLevelsAvailable = new wxStaticText(this, -1,
         _("Undo Levels Available (lots and lots)"),
         wxDefaultPosition, wxDefaultSize, 0),
         0, wxALIGN_LEFT|wxTOP|wxLEFT|wxRIGHT|wxALIGN_CENTER_VERTICAL, 2);

   purgeSizer->Add(firstLine);

   wxBoxSizer *secondLine = new wxBoxSizer(wxHORIZONTAL);

   secondLine->Add(new wxStaticText(this, -1, _("Levels to discard: ")),
                           0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, 3);

   secondLine->Add(
      mDiscardNum = new wxSpinCtrl(this, -1, "1", wxDefaultPosition, wxDefaultSize,
                     wxSP_ARROW_KEYS, 1, manager->GetCurrentState() - 1),
                     0, wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, 2 );

   secondLine->Add(
      mDiscard = new wxButton(this, DiscardID, _("Discard")),
      0, wxALIGN_RIGHT|wxALL, 2 );

   purgeSizer->Add(secondLine, 0, wxGROW);

   mTopSizer->Add(purgeSizer, 0, wxGROW|wxALL, 3);


   mManager = manager;
   mProject = parent;

   UpdateDisplay();

   SetAutoLayout(true);
   mTopSizer->Fit(this);
   mTopSizer->SetSizeHints(this);
   SetSizer(mTopSizer);
}

void HistoryWindow::UpdateDisplay()
{
   mList->Hide();   // to speed up the update
   
   mList->DeleteAllItems();
   for(unsigned int i = 0; i < mManager->GetNumStates(); i++) {
      wxString desc, size;
      wxListItem item;
      
      mManager->GetLongDescription(i + 1, &desc, &size);
      mList->InsertItem(i, desc, i == mManager->GetCurrentState() - 1 ? 1 : 0);
      mList->SetItem(i, 1, size);

      if(i > mManager->GetCurrentState() - 1) {
         item.m_itemId = i;
         item.SetTextColour(*wxLIGHT_GREY);
         mList->SetItem(item);
      }
   }

   mList->EnsureVisible(mManager->GetCurrentState() - 1);
   
   /* I may or may not like this, we'll see */
   /*
   wxListItem item;
   item.m_itemId = mSelected;
   item.m_state = wxLIST_STATE_SELECTED;
   mList->SetItem(item);
   */
   
   mList->Show();

   mLevelsAvailable->SetLabel(wxString::Format(_("Undo Levels Available: %d"),
                                              mManager->GetCurrentState() - 1));

   mDiscardNum->SetRange(1, mManager->GetCurrentState() - 1);
   if ((mManager->GetCurrentState() - 1) < mDiscardNum->GetValue()) 
      mDiscardNum->SetValue(mManager->GetCurrentState() - 1);

   mDiscard->Enable(mManager->GetCurrentState() > 1);

}

void HistoryWindow::OnLabelChanged(wxListEvent &event)
{
   mManager->SetLongDescription(event.GetIndex() + 1, event.GetItem().m_text);
   UpdateDisplay();
}

void HistoryWindow::OnDiscard(wxCommandEvent &event)
{
   mManager->RemoveStates(mDiscardNum->GetValue());
   UpdateDisplay();
}

void HistoryWindow::OnItemSelected(wxListEvent &event)
{
   mSelected = event.GetIndex();
   mProject->SetStateTo(mSelected + 1);
   UpdateDisplay();
}  


void HistoryWindow::OnCloseWindow(wxCloseEvent & WXUNUSED(event))
{
  this->Show(FALSE);
}


HistoryWindow::~HistoryWindow()
{
   delete mList->GetImageList(wxIMAGE_LIST_SMALL);
}

