/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportLOF.h

  David I. Murray

  Supports the opening of ".lof" files which are text files that contain
  a list of individual files to open in audacity in specific formats.

  (In BNF) The syntax for an LOF file, denoted by <lof>:

  <lof> ::= [<window> | <file> | <#>]*
  <window> ::= window [<window-parameter>]* <newline>
  <window-parameter> ::= offset <time> | duration <time>
  <time> ::= [<digit>]+ [ . [<digit>]* ]
  <file> ::= file [<file-parameter>]* <newline>
  <file-parameter> ::= offset <time>
  <#> ::= <comment> <newline>

  EXAMPLE LOF file:

  # everything following the hash character is ignored
  window # an initial window command is implicit and optional
  file "C:\folder1\sample1.wav"    # sample1.wav is displayed
  file "C:\sample2.wav" offset 5   # sample2 is displayed with a 5s offset
  File "C:\sample3.wav"            # sample3 is displayed with no offset
  window offset 5 duration 10      # open a new window, zoom to display 
  # 10 seconds total starting at 5 (ending at 15) seconds
  file "C:\sample3.wav" offset 2.5

  SEMANTICS:

  There are two commands: "window" creates a new window, and "file"
  appends a track to the current window and displays the file there. The
  first file is always placed in a new window, whether or not an initial
  "window" command is given.

  Commands have optional keyword parameters that may be listed in any
  order. A parameter should only occur once per command. The "offset"
  parameter specifies a time offset. For windows, this is the leftmost
  time displayed in the window. For files, the offset is an amount by
  which the file is shifted in time before display (only enabled for audio;
  not midi). The offset is specified as an integer or decimal number of
  seconds, and the default value is zero.

  Windows may also have a "duration" parameter, which specifies how much
  time should be displayed in the window. The default duration is equal
  to the duration of the longest track currently displayed.

**********************************************************************/

#include "../Audacity.h"

#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
#include <wx/textfile.h>
#include <wx/msgdlg.h>
#include <wx/tokenzr.h>

#include "ImportLOF.h"
#include "ImportMIDI.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"
#include "Import.h"
#include "../Internat.h"
#include "../NoteTrack.h"
#include "../Project.h"
#include "../FileFormats.h"
#include "../Prefs.h"
#include "../WaveTrack.h"
#include "../Internat.h"

class LOFImportPlugin : public ImportPlugin
{
public:
   LOFImportPlugin():
      ImportPlugin(wxStringList("lof", NULL))
   {
   }

   ~LOFImportPlugin() { }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);
};


class LOFImportFileHandle : public ImportFileHandle
{
public:
   LOFImportFileHandle(wxTextFile *file);
   ~LOFImportFileHandle();

   void SetProgressCallback(progress_callback_t function,
                            void *userData);
   wxString GetFileDescription();
   int GetFileUncompressedBytes();
   bool Import(TrackFactory *trackFactory, Track ***outTracks,
               int *outNumTracks);

private:
   // Takes a line of text in lof file and interprets it and opens files
   void lofOpenFiles(wxString* ln);
   void doDuration();
   void doScrollOffset();

   wxString             mName;
   wxTextFile           *mTextFile;
   void                 *mUserData;
   progress_callback_t  mProgressCallback;
   AudacityProject     *mProject;

   NoteTrack*        nTrack;

   // In order to know whether or not to create a new window
   bool              windowCalledOnce;

   // In order to zoom in, it must be done after files are opened
   bool              callDurationFactor;
   double            durationFactor;

   // In order to offset scrollbar, it must be done after files are opened
   bool              callScrollOffset;
   double            scrollOffset;
};

LOFImportFileHandle::LOFImportFileHandle(wxTextFile *file):
   mTextFile(file),
   mUserData(NULL),
   mProgressCallback(NULL)
{
   mProject = GetActiveProject();
   windowCalledOnce = false;
   callDurationFactor = false;
   durationFactor = 1;
   callScrollOffset = false;
   scrollOffset = 0;
}

void GetLOFImportPlugin(ImportPluginList *importPluginList,
                        UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new LOFImportPlugin);
}

wxString LOFImportPlugin::GetPluginFormatDescription()
{
    return _("List of Files in basic text format");
}

ImportFileHandle *LOFImportPlugin::Open(wxString filename)
{
   wxTextFile *file = new wxTextFile(FILENAME(filename));
   file->Open();

   if (!file->IsOpened())
   {
      delete file;
      return NULL;
   }

   return new LOFImportFileHandle(file);
}

void LOFImportFileHandle::SetProgressCallback(progress_callback_t progressCallback,
                                      void *userData)
{
   mProgressCallback = progressCallback;
   mUserData = userData;
}

wxString LOFImportFileHandle::GetFileDescription()
{
   return _("List of Files in basic text format");
}

int LOFImportFileHandle::GetFileUncompressedBytes()
{
   return 0;
}

bool LOFImportFileHandle::Import(TrackFactory *trackFactory, Track ***outTracks,
                                 int *outNumTracks)
{
   wxASSERT(mTextFile->IsOpened());

   wxString line = mTextFile->GetFirstLine();

   while (!mTextFile->Eof())
   {
      lofOpenFiles(&line);
      line = mTextFile->GetNextLine();  
   }

   // for last line
   lofOpenFiles(&line);

   // set any duration/offset factors for last window, as all files were called
   doDuration();
   doScrollOffset();

   // exited ok
   if(mTextFile->Close())
      return true;

   else
      return false;
}

static int CountNumTracks(AudacityProject *proj)
{
   int count = 0;
   Track *t;
   TrackListIterator iter(proj->GetTracks());
   
   t = iter.First();

   while(t) {
      count++;
      t = iter.Next();
   }

   return count;
}

void LOFImportFileHandle::lofOpenFiles(wxString* ln)
{  
   wxStringTokenizer tok(*ln, " ");
   wxStringTokenizer temptok1(*ln, "\"");
   wxStringTokenizer temptok2(*ln, " ");
   int tokenplace = 0;
   
   wxString targetfile;
   wxString tokenholder = tok.GetNextToken();
   
   if (tokenholder.IsSameAs("window", false))
   {
      // set any duration/offset factors for last window, as all files were called
      doDuration();
      doScrollOffset();
      
      if (windowCalledOnce)
      {
         mProject = CreateNewAudacityProject(gParentWindow);
      }

      windowCalledOnce = true;
      
      while (tok.HasMoreTokens())
      {
         tokenholder = tok.GetNextToken();
         
         if (tokenholder.IsSameAs("offset", false))
         {
            if (tok.HasMoreTokens())
               tokenholder = tok.GetNextToken();
            
            if (Internat::CompatibleToDouble(tokenholder, &scrollOffset))
            {
               callScrollOffset = true;
            }
            else
            {
               /* i18n-hint: You do not need to translate "LOF" */
               wxMessageBox(_("Invalid window offset in LOF file."),
                            /* i18n-hint: You do not need to translate "LOF" */
                            _("LOF Error"), wxOK | wxCENTRE, gParentWindow);
            }
               
            if (tok.HasMoreTokens())
               tokenholder = tok.GetNextToken();
         }
         
         if (tokenholder.IsSameAs("duration", false))
         {
            if (tok.HasMoreTokens())
               tokenholder = tok.GetNextToken();
            
            if (Internat::CompatibleToDouble(tokenholder, &durationFactor))
            {
               callDurationFactor = true;
            }
            else
            {
               /* i18n-hint: You do not need to translate "LOF" */
               wxMessageBox(_("Invalid duration in LOF file."),
                            /* i18n-hint: You do not need to translate "LOF" */
                            _("LOF Error"), wxOK | wxCENTRE, gParentWindow);
            }
         }     // End if statement

         if (tokenholder.IsSameAs("#"))
         {
            // # indicates comments; ignore line
            tok = wxStringTokenizer("", " ");
         }
      }     // End while loop
   }        // End if statement
   
   else if (tokenholder.IsSameAs("file", false))
   {

      // To identify filename and open it
      tokenholder = temptok1.GetNextToken();
      targetfile = temptok1.GetNextToken();
      
      // If file is a midi
      if (targetfile.AfterLast('.').IsSameAs("mid", false)
          ||  targetfile.AfterLast('.').IsSameAs("midi", false))
      {
         nTrack = new NoteTrack(mProject->GetDirManager());
         
         if (::ImportMIDI(targetfile, nTrack))
            mProject->GetTracks()->Add(nTrack);
      }
      
      // If not a midi, open audio file
      else
      {
         mProject->OpenFile(targetfile);
      }

      // Set tok to right after filename
      temptok2.SetString(targetfile);
      tokenplace = temptok2.CountTokens();
      
      for (int i = 0; i < tokenplace; i++)
         tokenholder = tok.GetNextToken();
      
      if (tok.HasMoreTokens())
      {
         tokenholder = tok.GetNextToken();
         
         if (tokenholder.IsSameAs("#"))
         {
            // # indicates comments; ignore line
            tok = wxStringTokenizer("", " ");
         }
         
         if (tokenholder.IsSameAs("offset", false))
         {
            if (tok.HasMoreTokens())
               tokenholder = tok.GetNextToken();
            double offset;
            
            if (Internat::CompatibleToDouble(tokenholder, &offset))
            {
               Track *t;
               TrackListIterator iter(mProject->GetTracks());
               
               t = iter.First();
               
               for (int i = 1; i < CountNumTracks(mProject) - 1; i++)
                  t = iter.Next();

               if (targetfile.AfterLast('.').IsSameAs("mid", false) ||
                   targetfile.AfterLast('.').IsSameAs("midi", false))
               {
                  wxMessageBox(_("MIDI tracks cannot be offset individually, "
                                 "only audio files may be."),
                               _("LOF Error"), wxOK | wxCENTRE, gParentWindow);
               }
               else
               {
                  if (CountNumTracks(mProject) == 1)
                     t->SetOffset(offset);
                  else
                  {
                     if (t->GetLinked())
                        t->SetOffset(offset);
                     
                     t = iter.Next();
                     t->SetOffset(offset);
                  }
               }
            }            
            else
            {
               /* i18n-hint: You do not need to translate "LOF" */
               wxMessageBox(_("Invalid track offset in LOF file."),
                            _("LOF Error"), wxOK | wxCENTRE, gParentWindow);
            }
         }     // End if statement
      }     // End if statement
   }     // End if statement
   
   else if (tokenholder.IsSameAs("#"))
   {
      // # indicates comments; ignore line
      tok = wxStringTokenizer("", " ");
   }
   else
   {
      // Couldn't parse a line
   }
}

void LOFImportFileHandle::doDuration()
{
   if (callDurationFactor)
   {
      double longestDuration = mProject->GetTracks()->GetEndTime();
      double realZoomValue = ((longestDuration/durationFactor)*(mProject->GetZoom()));
      mProject->Zoom(realZoomValue);
      callDurationFactor = false;
   }
}

void LOFImportFileHandle::doScrollOffset()
{
   if (callScrollOffset && (scrollOffset != 0))
   {
      mProject->TP_ScrollWindow(scrollOffset);
      callScrollOffset = false;
   }
}

LOFImportFileHandle::~LOFImportFileHandle()
{
   if(mTextFile)
   {
      if (mTextFile->IsOpened())
         if (mTextFile->Close())
            delete mTextFile;
   }
}
