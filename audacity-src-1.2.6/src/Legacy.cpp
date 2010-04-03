/**********************************************************************

  Audacity: A Digital Audio Editor

  Legacy.cpp

  Dominic Mazzoni

  These routines convert Audacity project files from the
  0.98...1.0 format into an XML format that's compatible with
  Audacity 1.2.0 and newer.

**********************************************************************/

#include "Audacity.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <wx/defs.h>
#include <wx/filefn.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/string.h>
#include <wx/textfile.h>

#include "Internat.h"
#include "Legacy.h"

class AutoRollbackRenamer {
public:
   AutoRollbackRenamer(wxString oldName, wxString newName) {
      mOldName = oldName;
      mNewName = newName;
      mRenameSucceeded = ::wxRenameFile(FILENAME(mOldName), FILENAME(mNewName));
      mFinished = false;
      mNewFile = NULL;
   }
   ~AutoRollbackRenamer()
   {
      if (mNewFile)
         fclose(mNewFile);

      if (mRenameSucceeded && !mFinished) {
         ::wxRemoveFile(FILENAME(mOldName));
         ::wxRenameFile(FILENAME(mNewName), FILENAME(mOldName));
      }
   }
   bool RenameSucceeded()
   {
      return mRenameSucceeded;
   }
   void Finished()
   {
      mFinished = true;
   }
   void SetNewFile(FILE *f)
   {
      mNewFile = f;
   }

   wxString mOldName, mNewName;
   bool mRenameSucceeded;
   bool mFinished;
   FILE *mNewFile;
};

bool ConvertLegacyTrack(wxTextFile *f, FILE *outf)
{
   wxString line;
   wxString kind;

   kind = (*f)[f->GetCurrentLine()];

   if (kind == "WaveTrack") {
      fprintf(outf, "\t<wavetrack name='%s'",
              (const char *)f->GetNextLine());

      wxString channel = f->GetNextLine();
      if (channel == "left") {
         fprintf(outf, " channel='0'");
         line = f->GetNextLine();
      }
      else if (channel == "right") {
         fprintf(outf, " channel='1'");
         line = f->GetNextLine();
      }
      else if (channel == "mono") {
         fprintf(outf, " channel='2'");
         line = f->GetNextLine();
      }
      else {
         fprintf(outf, " channel='2'");
         line = channel;
      }

      if (line == "linked") {
         fprintf(outf, " linked='1'");
         line = f->GetNextLine();
      }

      if (line != "offset")
         return false;
      fprintf(outf, " offset='%s'", (const char *)f->GetNextLine());

      long envLen;

      if (f->GetNextLine() != "EnvNumPoints")
         return false;
      line = f->GetNextLine();
      line.ToLong(&envLen);
      if (envLen < 0 || envLen > 10000)
         return false;

      size_t envStart = f->GetCurrentLine();
      if (f->GetLineCount() < envStart+(2*envLen)+1)
         return false;

      f->GoToLine(envStart+(2*envLen));
      if (f->GetNextLine() != "EnvEnd")
         return false;
      if (f->GetNextLine() != "numSamples")
         return false;

      wxString numSamples = f->GetNextLine();

      if (f->GetNextLine() != "rate")
         return false;
      fprintf(outf, " rate='%s'", (const char *)f->GetNextLine());
      fprintf(outf, ">\n");

      if (envLen > 0) {
         fprintf(outf, "\t\t<envelope numpoints='%d'>\n", (int)envLen);

         long i;
         for(i=0; i<envLen; i++) {
            fprintf(outf, "\t\t\t<controlpoint t='%s' val='%s'/>\n",
                    (const char *)f->GetLine(envStart + 2*i + 1),
                    (const char *)f->GetLine(envStart + 2*i + 2));
         }

         fprintf(outf, "\t\t</envelope>\n");
      }

      if (f->GetNextLine() != "numBlocks")
         return false;
      long numBlocks;
      line = f->GetNextLine();
      line.ToLong(&numBlocks);

      if (numBlocks < 0 || numBlocks > 131072)
         return false;

      fprintf(outf, "\t\t<sequence maxsamples='524288'");
      fprintf(outf, " sampleformat='131073' ");
      fprintf(outf, " numsamples='%s'>\n", (const char *)numSamples);

      long b;
      for(b=0; b<numBlocks; b++) {
         wxString start;
         wxString len;
         wxString name;

         if (f->GetNextLine() != "Block start")
            return false;
         start = f->GetNextLine();
         if (f->GetNextLine() != "Block len")
            return false;
         len = f->GetNextLine(); 
         if (f->GetNextLine() != "Block info")
            return false;
         name = f->GetNextLine();

         fprintf(outf, "\t\t\t<waveblock start='%s'>\n",
                 (const char *)start);

         if (name == "Alias") {
            wxString aliasPath = f->GetNextLine();
            wxString localLen = f->GetNextLine();
            wxString aliasStart = f->GetNextLine();
            wxString aliasLen = f->GetNextLine();
            wxString aliasChannel = f->GetNextLine();
            wxString localName = f->GetNextLine();

            fprintf(outf, "\t\t\t\t<legacyblockfile");
            fprintf(outf, " name='%s'", (const char *)localName);
            fprintf(outf, " alias='1'");
            fprintf(outf, " aliaspath='%s'", (const char *)aliasPath);
            fprintf(outf, " aliasstart='%s'", (const char *)aliasStart);
            fprintf(outf, " aliaslen='%s'", (const char *)aliasLen);
            fprintf(outf, " aliaschannel='%s'", (const char *)aliasChannel);
            fprintf(outf, " summarylen='%s'", (const char *)localLen);
            fprintf(outf, " norms='1'");
            fprintf(outf, " />\n");
         }
         else {
            fprintf(outf, "\t\t\t\t<legacyblockfile");
            fprintf(outf, " name='%s'", (const char *)name);
            fprintf(outf, " len='%s'", (const char *)len);
            fprintf(outf, " summarylen='8244'");
            fprintf(outf, " norms='1'");
            fprintf(outf, " />\n");
         }

         fprintf(outf, "\t\t\t</waveblock>\n");
      }

      fprintf(outf, "\t\t</sequence>\n");
      fprintf(outf, "\t</wavetrack>\n");
      
      return true;
   }
   else if (kind == "LabelTrack") {
      line = f->GetNextLine();
      if (line != "NumMLabels")
         return false;

      long numLabels, l;

      line = f->GetNextLine();
      line.ToLong(&numLabels);
      if (numLabels < 0 || numLabels > 1000000)
         return false;

      fprintf(outf, "\t<labeltrack name='Labels' numlabels='%ld'>\n",
              numLabels);

      for(l=0; l<numLabels; l++) {
         wxString t, title;

         t = f->GetNextLine();
         title = f->GetNextLine();

         fprintf(outf, "\t\t<label t='%s' title='%s' />\n",
                 (const char *)t, (const char *)title);
      }

      fprintf(outf, "\t</labeltrack>\n");

      line = f->GetNextLine();
      if (line != "MLabelsEnd")
         return false;

      return true;
   }
   else if (kind == "NoteTrack") {
      // Just skip over it - they didn't even work in version 1.0!

      do {
         line = f->GetNextLine();
         if (line == "WaveTrack" ||
             line == "NoteTrack" ||
             line == "LabelTrack" |\
             line == "EndTracks") {
            f->GoToLine(f->GetCurrentLine()-1);
            return true;
         }
      } while (f->GetCurrentLine() < f->GetLineCount());

      return false;
   }
   else
      return false;
}

bool ConvertLegacyProjectFile(wxFileName filename)
{
   wxTextFile f;
   FILE *outf;
   int index = 0;
   wxString backupName;

   do {
      index++;
      fflush(stdout);
      backupName = filename.GetPath() + wxFILE_SEP_PATH + filename.GetName() +
         "_bak" + wxString::Format("%d", index) + "." + filename.GetExt();
   } while(::wxFileExists(FILENAME(backupName)));

   // This will move the original file out of the way, but 
   // move it back if we exit from this function early.
   AutoRollbackRenamer renamer(filename.GetFullPath(), backupName);
   if (!renamer.RenameSucceeded())
      return false;

   f.Open(FILENAME(backupName));
   if (!f.IsOpened())
      return false;

   wxString name = filename.GetFullPath();

   outf = fopen((const char *)FILENAME(name), "wb");
   if (!outf)
      return false;

   fprintf(outf, "<?xml version='1.0'?>\n");

   renamer.SetNewFile(outf);

   wxString label;
   wxString value;

   if (f.GetFirstLine() != "AudacityProject")
      return false;
   if (f.GetNextLine() != "Version")
      return false;
   if (f.GetNextLine() != "0.95")
      return false;
   if (f.GetNextLine() != "projName")
      return false;

   fprintf(outf, "<audacityproject projname='%s'",
           (const char *)f.GetNextLine());
   fprintf(outf, " version='1.1.0' audacityversion='%s'",
           AUDACITY_VERSION_STRING);
   label = f.GetNextLine();
   while (label != "BeginTracks") {
      value = f.GetNextLine();
      fprintf(outf, " %s='%s'", (const char *)label, (const char *)value);
      label = f.GetNextLine();
   }
   fprintf(outf, ">\n");

   label = f.GetNextLine();
   while (label != "EndTracks") {
      bool success = ConvertLegacyTrack(&f, outf);
      if (!success)
         return false;
      label = f.GetNextLine();
   }

   fprintf(outf, "</audacityproject>\n");

   renamer.Finished();

   ::wxMessageBox(wxString::Format(_("Converted a 1.0 project file to the new format.\nThe old file has been saved as '%s'"), (const char *)backupName),
                  _("Opening Audacity Project"));

   return true;
}
