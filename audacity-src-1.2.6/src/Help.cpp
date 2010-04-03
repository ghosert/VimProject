/**********************************************************************

  Audacity: A Digital Audio Editor

  Help.cpp

  Dominic Mazzoni

  Audacity uses the wxWindows HTML help controller and gets the
  HTML files from a zip archive with a "htb" extension, which
  is supposed to stand for "HTML Book".  It expects the help
  file to be called "audacity-1.2-help.htb".

  If you want to edit the help file, unzip audacity-1.2-help.htb
  (rename it to audacity-1.2-help.zip first if you have to), edit
  the files, and then zip them again.  Audacity asks the user
  for the location of the help file the first time (if necessary)
  and then remembers it from then on.

**********************************************************************/

#include <wx/defs.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/html/helpctrl.h>
#include <wx/intl.h>

#include "Audacity.h"
#include "AudacityApp.h"
#include "Help.h"
#include "Internat.h"
#include "Prefs.h"

wxHtmlHelpController *gHelp = NULL;

void InitHelp(wxWindow * parent)
{
   if (!gHelp) {
      wxString defaultLoc;
      wxArrayString helpFiles;

      wxGetApp().FindFilesInPathList("audacity-1.2-help.htb",
                                     wxGetApp().audacityPathList,
                                     wxFILE,
                                     helpFiles);

      if (helpFiles.GetCount() > 0)
         defaultLoc = helpFiles[0];
      else
         defaultLoc = INSTALL_PREFIX "/share/audacity/audacity-1.2-help.htb";

      wxString helpFilePath =
          gPrefs->Read("/Help/HelpFilePath1.2", defaultLoc);

      if (!::wxFileExists(FILENAME(helpFilePath))) {
         helpFilePath = defaultLoc;
      }
      if (!::wxFileExists(FILENAME(helpFilePath))) {
         helpFilePath = wxFileSelector(_("Where is audacity-1.2-help.htb?"), NULL,
                                       "audacity-1.2-help.htb",    // Name
                                       "",                     // Extension
                                       _("HTML Help Books (*.htb)|*.htb"),
                                       wxOPEN, parent);
         if (helpFilePath == "")
            return;
      }

      gHelp = new wxHtmlHelpController();
      if (!gHelp->AddBook(helpFilePath)) {
         wxMessageBox(_("Couldn't open the Audacity Help file."));
         delete gHelp;
         gHelp = NULL;
      }

      gPrefs->Write("/Help/HelpFilePath1.2", helpFilePath);
   }
}

void ShowHelp(wxWindow * parent)
{
   InitHelp(parent);

   if (gHelp)
      gHelp->Display("contents.htm");
}

void ShowHelpIndex(wxWindow * parent)
{
   InitHelp(parent);

   if (gHelp)
      gHelp->DisplayIndex();
}

void ShowHelp(wxWindow * parent, wxString topic)
{
   InitHelp(parent);

   if (gHelp)
      gHelp->KeywordSearch(topic);
}

void SearchHelp(wxWindow * parent)
{
   InitHelp(parent);

   if (gHelp) {
      wxString key = wxGetTextFromUser(_("Search for?"),
                                       _("Search help for keyword"),
                                       "",
                                       parent);

      if (!key.IsEmpty())
         gHelp->KeywordSearch(key);
   }
}

void QuitHelp()
{
   if (gHelp) {
      delete gHelp;
      gHelp = NULL;
   }
}
