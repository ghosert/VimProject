/**********************************************************************

  Audacity: A Digital Audio Editor

  KeyConfigPrefs.cpp

  Brian Gunlogson
  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/statbox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/listctrl.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/menuitem.h>

#include "../Prefs.h"
#include "../commands/CommandManager.h"
#include "../commands/Keyboard.h"
#include "../xml/XMLFileReader.h"

#include "KeyConfigPrefs.h"
#include "../Internat.h"

#define AssignDefaultsButtonID  7001
#define CurrentComboID          7002
#define SetButtonID             7003
#define ClearButtonID           7004
#define CommandsListID          7005
#define SaveButtonID            7006
#define LoadButtonID            7007


// The numbers of the columns of the mList.
enum { BlankColumn=0, CommandColumn=1, KeyComboColumn=2};

BEGIN_EVENT_TABLE(KeyConfigPrefs, wxPanel)
   EVT_BUTTON(AssignDefaultsButtonID, KeyConfigPrefs::OnDefaults)
   EVT_BUTTON(SetButtonID, KeyConfigPrefs::OnSet)
   EVT_BUTTON(ClearButtonID, KeyConfigPrefs::OnClear)
   EVT_BUTTON(SaveButtonID, KeyConfigPrefs::OnSave)
   EVT_BUTTON(LoadButtonID, KeyConfigPrefs::OnLoad)
   EVT_LIST_ITEM_SELECTED(CommandsListID, KeyConfigPrefs::OnItemSelected)
END_EVENT_TABLE()

KeyConfigPrefs::KeyConfigPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   AudacityProject *project = GetActiveProject();
   if (!project)
      return;

   mManager = project->GetCommandManager();

   topSizer = new wxBoxSizer( wxVERTICAL );

   // This code for displaying keybindings is similar to code in MousePrefs.
   // Would be nice to create a new 'Bindings' class which both 
   // KeyConfigPrefs and MousePrefs use.
   mList = new wxListCtrl( this, CommandsListID ,
      wxDefaultPosition, wxSize(500,250),
      wxLC_REPORT | wxLC_HRULES | wxLC_VRULES | wxSUNKEN_BORDER 
      );

   wxASSERT( mList );

   //An empty first column is a workaround - under Win98 the first column 
   //can't be right aligned.
   mList->InsertColumn(BlankColumn,    "", wxLIST_FORMAT_LEFT );
   mList->InsertColumn(CommandColumn,  _("Command"),  wxLIST_FORMAT_RIGHT );
   mList->InsertColumn(KeyComboColumn, _("Key Combination"), wxLIST_FORMAT_LEFT );

   RepopulateBindingsList();

   mList->SetColumnWidth( BlankColumn, 0 ); // First column width is zero, to hide it.
   // Would like to use wxLIST_AUTOSIZE but
   // wxWindows does not look at the size of column heading.
//   mList->SetColumnWidth( CommandColumn, 250 );
   mList->SetColumnWidth( CommandColumn, wxLIST_AUTOSIZE );
   mList->SetColumnWidth( KeyComboColumn, 115 );

   topSizer->Add( mList, 1, 
                  wxGROW | wxALL, GENERIC_CONTROL_BORDER);

   //Add key combo text box
   mCurrentComboText = new SysKeyTextCtrl(
      this, CurrentComboID, "",
      wxDefaultPosition, wxSize(115, -1), 0 );

   wxButton *pSetButton = new wxButton(this, SetButtonID, _("Set"));
   wxButton *pClearButton = new wxButton(this, ClearButtonID, _("Clear"));

   wxBoxSizer * pComboLabelSizer = new wxBoxSizer( wxHORIZONTAL );

   pComboLabelSizer->Add( mCurrentComboText, 0,
                       wxALL, GENERIC_CONTROL_BORDER);
   pComboLabelSizer->Add( pSetButton, 0,
                       wxALL, GENERIC_CONTROL_BORDER);
   pComboLabelSizer->Add( pClearButton, 0,
                       wxALL, GENERIC_CONTROL_BORDER);
   topSizer->Add(pComboLabelSizer, 0,
                       wxALL, GENERIC_CONTROL_BORDER);

   #ifdef __WXMAC__
   wxBoxSizer * pMacSizer = new wxBoxSizer( wxHORIZONTAL );
   wxString warningStr = 
      _("Note: Pressing Cmd+Q will quit. All other keys are valid.");
   pMacSizer->Add(new wxStaticText(this, -1, warningStr),
                  0, 
                  wxALL, GENERIC_CONTROL_BORDER);
   topSizer->Add(pMacSizer, 0, wxALL, GENERIC_CONTROL_BORDER);
   #endif

   wxButton *pDefaultsButton =
      new wxButton(this, AssignDefaultsButtonID, _("Defaults"));
   wxButton *pSaveButton =
      new wxButton(this, SaveButtonID, _("Save..."));
   wxButton *pLoadButton =
      new wxButton(this, LoadButtonID, _("Load..."));

   pComboLabelSizer = new wxBoxSizer( wxHORIZONTAL );
   pComboLabelSizer->Add( pDefaultsButton, 0,
                       wxALL, GENERIC_CONTROL_BORDER);
   pComboLabelSizer->Add( pSaveButton, 0,
                       wxALL, GENERIC_CONTROL_BORDER);
   pComboLabelSizer->Add( pLoadButton, 0,
                       wxALL, GENERIC_CONTROL_BORDER);
   topSizer->Add(pComboLabelSizer, 0,
                 wxALL, GENERIC_CONTROL_BORDER);

   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);

   outSizer->Fit(this);
   outSizer->SetSizeHints(this);

   mCommandSelected = -1;
}

void KeyConfigPrefs::OnSave(wxCommandEvent& event)
{
   Apply();

   wxString fName = "Audacity-keys.xml";
   wxString path = gPrefs->Read("/DefaultExportPath",
                                FROMFILENAME(::wxGetCwd()));

   fName = wxFileSelector(_("Export Keyboard Shortcuts As:"),
                          NULL,
                          fName,
                          "xml",
                          "*.xml", wxSAVE | wxOVERWRITE_PROMPT, this);

   if (!fName)
      return;

   path = wxPathOnly(fName);
   gPrefs->Write("/DefaultExportPath", path);

   FILE *fp = fopen((const char *)FILENAME(fName), "wb");
   if (!fp || ferror(fp)) {
      wxMessageBox(_("Couldn't write to file: ") + fName,
                   _("Error saving keyboard shortcuts"),
                   wxOK | wxCENTRE, this);
      return;
   }

   mManager->WriteXML(0, fp);
   fclose(fp);
}

void KeyConfigPrefs::OnLoad(wxCommandEvent& event)
{
   wxString path = gPrefs->Read("/DefaultOpenPath",
                                FROMFILENAME(::wxGetCwd()));

   wxString fileName = wxFileSelector(_("Select an XML file containing Audacity keyboard shortcuts..."),
                                      path,     // Path
                                      "",       // Name
                                      "",       // Extension
                                      _("XML files (*.xml)|*.xml|"
                                        "All files (*.*)|*.*"),
                                      0,        // Flags
                                      this);    // Parent

   if (!fileName)
      return;

   path = wxPathOnly(fileName);
   gPrefs->Write("/DefaultOpenPath", path);

   XMLFileReader reader;
   if (!reader.Parse(mManager, fileName))
      wxMessageBox(reader.GetErrorStr(),
                   _("Error loading keyboard shortcuts"),
                   wxOK | wxCENTRE, this);

   RepopulateBindingsList();
}

void KeyConfigPrefs::OnSet(wxCommandEvent& event)
{
   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount())
      return;

   mList->SetItem( mCommandSelected, KeyComboColumn, mCurrentComboText->GetValue() );
}

void KeyConfigPrefs::OnClear(wxCommandEvent& event)
{
   mCurrentComboText->Clear();

   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount())
      return;

   mList->SetItem( mCommandSelected, KeyComboColumn, "" );
}

void KeyConfigPrefs::OnItemSelected(wxListEvent &event)
{
   mCommandSelected = event.GetIndex();
   if (mCommandSelected < 0 || mCommandSelected >= (int)mNames.GetCount()) {
      mCurrentComboText->SetLabel("");
      return;
   }
   
   wxString key = mManager->GetKeyFromName(mNames[mCommandSelected]);
   mCurrentComboText->Clear();
   mCurrentComboText->AppendText(key);
   mCurrentComboText->SetFocus();
}

void KeyConfigPrefs::RepopulateBindingsList()
{
   mList->DeleteAllItems(); // Delete contents, but not the column headers.
   mNames.Clear();
   mManager->GetAllCommandNames(mNames, false);
   unsigned int i;
   for(i=0; i<mNames.GetCount(); i++) {
      mList->InsertItem( i, "" );
      wxString label = mManager->GetLabelFromName(mNames[i]);
      label = wxMenuItem::GetLabelFromText(label.BeforeFirst('\t'));
      wxString key = mManager->GetKeyFromName(mNames[i]);

      #ifdef __WXMAC__
      // Replace Ctrl with Cmd
      if (key.Length() >= 5 && key.Left(5)=="Ctrl+") {
         key = "Cmd+"+key.Right(key.Length()-5);
      }
      #endif

      mList->SetItem( i, CommandColumn, label );
      mList->SetItem( i, KeyComboColumn, key );
   }
}


void KeyConfigPrefs::OnDefaults(wxCommandEvent& event)
{
   unsigned int i;

   for(i=0; i<mNames.GetCount(); i++) {
      mList->SetItem( i, KeyComboColumn, mManager->GetDefaultKeyFromName(mNames[i]) );
   }
}

bool KeyConfigPrefs::Apply()
{
   unsigned int i;
   wxListItem item;

   gPrefs->SetPath("/NewKeys");

   //
   // Only store the key in the preferences if it's different 
   // than the default value.
   //

   item.m_col = KeyComboColumn;
   item.m_mask = wxLIST_MASK_TEXT;
   for(i=0; i<mNames.GetCount(); i++) {
      item.SetId( i );
      mList->GetItem(item);
      wxString name = mNames[i];
      wxString key = item.m_text;

      #ifdef __WXMAC__
      // Replace Cmd with Ctrl
      if (key.Length() >= 4 && key.Left(4)=="Cmd+") {
         key = "Ctrl+"+key.Right(key.Length()-4);
      }
      #endif

      wxString defaultKey = mManager->GetDefaultKeyFromName(name);

      if (gPrefs->HasEntry(name)) {
         wxString oldKey = gPrefs->Read(name, key);
         if (oldKey != key)  {
            gPrefs->Write(name, key);
         }
         if (key == defaultKey)   {
            gPrefs->DeleteEntry(name);
         }
      }
      else {
         if (key != defaultKey){
            gPrefs->Write(name, key);
         }
      }
   }

   gPrefs->SetPath("/");

   for(i=0; i<gAudacityProjects.GetCount(); i++)
      if(gAudacityProjects[i])
         gAudacityProjects[i]->RebuildMenuBar();

   return true;
}

KeyConfigPrefs::~KeyConfigPrefs()
{
}


//BG: A quick and dirty override of wxTextCtrl to capture keys like Ctrl, Alt
BEGIN_EVENT_TABLE(SysKeyTextCtrl, wxTextCtrl)
   EVT_KEY_DOWN(SysKeyTextCtrl::OnKey)
   EVT_CHAR(SysKeyTextCtrl::OnChar)
END_EVENT_TABLE()

SysKeyTextCtrl::SysKeyTextCtrl(wxWindow *parent, wxWindowID id,
                              const wxString& value,
                              const wxPoint& pos,
                              const wxSize& size,
                              long style,
                              const wxValidator& validator,
                              const wxString& name)
: wxTextCtrl(parent, id, value, pos, size, style, validator, name)
{
}

SysKeyTextCtrl::~SysKeyTextCtrl()
{
}

//BG: It works on Windows, but we need to trap WM_CHAR
//DM: On Linux, now it works except for Ctrl+3...Ctrl+8 (April/2003)
void SysKeyTextCtrl::OnKey(wxKeyEvent& event)
{
   SetValue(KeyEventToKeyString(event));
}

//BG: Trap WM_CHAR
void SysKeyTextCtrl::OnChar(wxKeyEvent& event)
{
}
