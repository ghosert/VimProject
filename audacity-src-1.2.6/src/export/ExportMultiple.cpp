/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMultiple.cpp

  Dominic Mazzoni

  Presents a dialog box allowing the user to export multiple files
  either by exporting each track as a separate file, or by
  exporting each label as a separate file.

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/dirdlg.h>
#include <wx/event.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/radiobut.h>

#include "Export.h"
#include "ExportPCM.h"
#include "ExportMP3.h"
#include "ExportOGG.h"
#include "ExportCL.h"

#include "../Internat.h"
#include "../FileFormats.h"
#include "../LabelTrack.h"
#include "../Project.h"
#include "../Prefs.h"
#include "../Tags.h"

enum {
   FormatID = 10001,
   DirID,
   ChooseID,
   LabelID,
   FirstID,
   FirstFileNameID,
   TrackID,
   ByNameID,
   ByNumberID,
   PrefixID,
   OverwriteID,
   ExplanationID
};

class ExportMultipleDialog : public wxDialog
{
 public:
   ExportMultipleDialog(wxWindow *parent, wxWindowID id);

   // input
   bool byLabelsAllowed;
   bool byTracksAllowed;

   // input and output
   wxString firstFileName;
   wxString prefix;

   // output
   int format; // PCM, MP3, Ogg
   wxString dir;

   bool byLabels;  // otherwise by tracks
   bool includeFirst;

   bool byName;  // otherwise by number

   bool overwrite;

   void CopyDataToControls();

 private:
   void EnableControls();

   void OnChoose(wxCommandEvent& event);

   void OnLabel(wxCommandEvent& event);
   void OnFirst(wxCommandEvent& event);
   void OnFirstFileName(wxCommandEvent& event);

   void OnTrack(wxCommandEvent& event);

   //

   void OnByName(wxCommandEvent& event);
   void OnByNumber(wxCommandEvent& event);

   //

   void OnPrefix(wxCommandEvent& event);

   //

   void OnCancel(wxCommandEvent& event);
   void OnExport(wxCommandEvent& event);

   wxChoice      *mFormat;
   wxTextCtrl    *mDir;
   wxButton      *mChoose;
   
   wxRadioButton *mLabel;
   wxCheckBox    *mFirst;
   wxTextCtrl    *mFirstFileName;
   wxRadioButton *mTrack;

   wxRadioButton *mByName;
   wxRadioButton *mByNumber;
   wxTextCtrl    *mPrefix;

   wxTextCtrl    *mExplanation;

   wxCheckBox    *mOverwrite;

   wxButton      *mCancel;
   wxButton      *mExport;

   DECLARE_EVENT_TABLE()
};

int CountNumExportableTracks(TrackList *tracks)
{
   int numTracks = 0;

   // Only count WaveTracks, and for linked pairs, only count the
   // second one of the pair

   TrackListIterator iter(tracks);
   Track *tr = iter.First();
   while (tr) {
      if (tr->GetKind() == Track::Wave &&
          tr->GetLinked()==false)
         numTracks++;
      tr = iter.Next();
   }

   return numTracks;
}

int CountNumLabels(TrackList *tracks)
{
   TrackListIterator iter(tracks);
   Track *tr = iter.First();
   while (tr) {
      if (tr->GetKind() == Track::Label) {
         LabelTrack *labelTrack = (LabelTrack *)tr;
         return labelTrack->GetNumLabels();
      }
      tr = iter.Next();
   }

   return 0;
}

LabelTrack *GetLabelTrack(TrackList *tracks)
{
   TrackListIterator iter(tracks);
   Track *tr = iter.First();
   while (tr) {
      if (tr->GetKind() == Track::Label)
         return (LabelTrack *)tr;
      tr = iter.Next();
   }

   return NULL;
}

bool *SaveSelected(TrackList *tracks)
{
   int numTracks = CountNumExportableTracks(tracks);
   bool *saveSelected = new bool[numTracks];
   int i = 0;

   TrackListIterator iter(tracks);
   Track *tr = iter.First();
   while (tr) {
      if (tr->GetKind() == Track::Wave) {
         saveSelected[i++] = tr->GetSelected();
         tr->SetSelected(false);
         if (tr->GetLinked()) {
            tr = iter.Next();
            if (tr)
               tr->SetSelected(false);
         }
      }
      if (tr)
         tr = iter.Next();
   }   
   
   return saveSelected;
}

void RestoreSelected(TrackList *tracks, bool *saveSelected)
{
   int i = 0;

   TrackListIterator iter(tracks);
   Track *tr = iter.First();
   while (tr) {
      if (tr->GetKind() == Track::Wave) {
         tr->SetSelected(saveSelected[i]);
         if (tr->GetLinked()) {
            tr = iter.Next();
            if (tr)
               tr->SetSelected(saveSelected[i]);
         }
         i++;
      }
      if (tr)
         tr = iter.Next();
   }   
   
   delete[] saveSelected;
}

wxString MakeFullPath(bool overwrite,
                      wxString dir, wxString name, wxString extension)
{
   wxString sep = wxFILE_SEP_PATH;

   if (overwrite)
      return dir+sep+name+extension;
   else {
      wxString fullPath = dir+sep+name+extension;
      int i = 2;
      while(::wxFileExists(fullPath)) {
         fullPath = dir+sep+
            wxString::Format("%s-%d", name.c_str(), i) +
            extension;
         i++;
      }
      return fullPath;
   }
}

static bool DoExport(AudacityProject *project,
                     int format, wxString dir,
                     bool stereo, wxString name,
                     bool selectionOnly, double t0, double t1,
                     bool overwrite,
                     int trackNumber)
{
   switch(format) {
   case 0:
   default: {
      int format = ReadExportFormatPref();
      wxString extension = "." +
         sf_header_extension(format & SF_FORMAT_TYPEMASK);
      wxString fullPath = MakeFullPath(overwrite,
                                       dir, name, extension);
      return ExportPCM(project, stereo, fullPath,
                       selectionOnly, t0, t1);
   } break;
   case 1: {
      Tags *tags = project->GetTags();
      tags->SetTitle(name);            
      tags->SetTrackNumber(trackNumber);
      wxString fullPath = MakeFullPath(overwrite,
                                       dir, name, ".mp3");
      return ExportMP3(project, stereo, fullPath,
                       selectionOnly, t0, t1); }
   case 2: {

     #ifdef USE_LIBVORBIS
      wxString fullPath = MakeFullPath(overwrite,
                                       dir, name, ".ogg");
      return ExportOGG(project, stereo, fullPath,
                       selectionOnly, t0, t1);
     #endif 
   }
   }
}

void MakeNameUnique(wxArrayString otherNames, wxString &newName)
{
   if (otherNames.Index(newName, false) >= 0) {
      int i=2;
      wxString orig = newName;
      do {
         newName.Printf("%s-%d", orig.c_str(), i);
         i++;
      } while (otherNames.Index(newName, false) >= 0);
   }
}

bool ExportMultipleByTrack(AudacityProject *project,
                           wxString dir, int format, bool byName,
                           wxString prefix, bool overwrite)
{
   TrackList *tracks = project->GetTracks();
   int numTracks = CountNumExportableTracks(tracks);
   bool *saveSelected = SaveSelected(tracks);
   TrackListIterator iter(tracks);
   Track *tr = iter.First();
   Track *tr2 = NULL;
   double t0, t1;
   bool stereo;
   bool ok = true;
   int i=0;
   wxString name;
   wxArrayString otherNames;
   while (tr) {
      if (tr->GetKind() == Track::Wave) {
         tr->SetSelected(true);
         tr2 = NULL;
         if (tr->GetLinked()) {
            tr2 = iter.Next();
            if (tr2)
               tr2->SetSelected(true);
         }

         t0 = ((WaveTrack *)tr)->GetStartTime();
         t1 = ((WaveTrack *)tr)->GetEndTime();
         if (tr2==NULL &&
             ((WaveTrack *)tr)->GetChannel() == WaveTrack::MonoChannel &&
             ((WaveTrack *)tr)->GetPan()==0.0)
            stereo = false;
         else
            stereo = true;
         if (byName) {
            name = tr->GetName();
         }
         else {
            if (numTracks > 9)
               name.Printf("%s-%02d", prefix.c_str(), i+1);
            else
               name.Printf("%s-%d", prefix.c_str(), i+1);
         }

         MakeNameUnique(otherNames, name);

         ok = DoExport(project, format, dir, stereo, name, true, t0, t1,
                       overwrite, i+1);
         if (!ok)
            break;
         otherNames.Add(name);
         
         tr->SetSelected(false);
         if (tr2) {
            tr2->SetSelected(false);
            tr2 = NULL;
         }
         
         i++;
         
         if (tr)
            tr = iter.Next();
      }
   }   
   
   RestoreSelected(tracks, saveSelected);

   ::wxMessageBox(wxString::Format(_("Successfully exported %d files."), i),
                  _("Export Multiple"),
                  wxOK | wxCENTRE, project);
   
   return ok;
}

bool ExportMultipleByLabel(AudacityProject *project,
                           wxString dir, int format,
                           bool byName, wxString prefix,
                           bool includeFirst, wxString firstFileName,
                           bool overwrite)
{
   TrackList *tracks = project->GetTracks();
   LabelTrack *labels = GetLabelTrack(tracks);
   if (!labels)
      return false;
   int numLabels = labels->GetNumLabels();
   int numFiles = numLabels;
   int l = 0;
   if (includeFirst) {
      l--;
      numFiles++;
   }

   // Figure out if we're exporting stereo or mono
   int numLeft = 0, numRight = 0, numMono = 0;
   TrackListIterator iter1(tracks);
   Track *tr = iter1.First();
   while (tr) {
      if (tr->GetKind() == Track::Wave) {
         if (tr->GetChannel() == Track::LeftChannel)
            numLeft++;
         else if (tr->GetChannel() == Track::RightChannel)
            numRight++;
         else if (tr->GetChannel() == Track::MonoChannel) {
            // It's a mono channel, but it may be panned
            float pan = ((WaveTrack*)tr)->GetPan();
            
            if (pan == -1.0)
               numLeft++;
            else if (pan == 1.0)
               numRight++;
            else if (pan == 0)
               numMono++;
            else {
               numLeft++;
               numRight++;
            }
         }
      }

      tr = iter1.Next();
   }

   bool stereo = false;
   if (numRight > 0 || numLeft > 0)
      stereo = true;

   wxArrayString otherNames;
   wxString name;
   double t0, t1;
   int count = 0;
   bool ok = true;

   while(l < numLabels) {
      const LabelStruct *info = 0;

      if (l < 0) {
         name = firstFileName;
         t0 = 0.0;
      }
      else {
         info = labels->GetLabel(l);
         name = info->title;
         t0 = info->t;
      }

      if (info && info->t < info->t1) {
         t1 = info->t1;
      }
      else if (l < numLabels-1) {
         const LabelStruct *info1 = labels->GetLabel(l+1);
         t1 = info1->t;
      }
      else
         t1 = tracks->GetEndTime();
      
      if (!byName) {
         if (numFiles > 9)
            name.Printf("%s-%02d", prefix.c_str(), count+1);
         else
            name.Printf("%s-%d", prefix.c_str(), count+1);
      }
      
      MakeNameUnique(otherNames, name);
      
      ok = DoExport(project, format, dir, stereo, name, false, t0, t1,
                    overwrite, count+1);
      if (!ok)
         break;
      otherNames.Add(name);
      count++;
      l++;
   }
   
   ::wxMessageBox(wxString::Format(_("Successfully exported %d files."),
                                   count),
                  _("Export Multiple"),
                  wxOK | wxCENTRE, project);
   
   return ok;
}

bool ExportMultiple(AudacityProject *project)
{
   TrackList *tracks = project->GetTracks();
   int numTracks = CountNumExportableTracks(tracks);
   int numLabels = CountNumLabels(tracks);

   if (numTracks < 2 && numLabels < 1) {
      ::wxMessageBox(_("If you have more than one Audio Track, you can export "
                       "each track as a separate file,\n"
                       "or if you have a Label Track, you can export a new "
                       "file for each label.\n\n"
                       "This project does not have multiple tracks or a "
                       "Label Track, so you cannot export multiple files."),
                     _("Can't export multiple files"),
                     wxOK | wxCENTRE, project);
      return false;
   }

   ExportMultipleDialog dlog(project, -1);
   dlog.byLabelsAllowed = (numLabels > 0);
   dlog.byTracksAllowed = (numTracks > 1);
   dlog.firstFileName = project->GetName();
   dlog.prefix = project->GetName();

   dlog.CopyDataToControls();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   if (dlog.format == 1) { // MP3
      Tags *tags = project->GetTags();
      if (tags->IsEmpty()) {
         wxString saveTitle = tags->GetTitle();
         int saveTrackNumber = tags->GetTrackNumber();
         tags->SetTitle("(automatic)");
         tags->SetTrackNumber(0);
         tags->AllowEditTitle(false);
         tags->AllowEditTrackNumber(false);
         bool rval = tags->ShowEditDialog(project,
                                          _("Edit the ID3 tags "
                                            "for all MP3 files"));
         tags->AllowEditTitle(true);
         tags->AllowEditTrackNumber(true);
         if (!rval) {
            tags->SetTitle(saveTitle);
            tags->SetTrackNumber(saveTrackNumber);
            return false;
         }
      }
   }

   if (dlog.byLabels)
      return ExportMultipleByLabel(project, dlog.dir, dlog.format,
                                   dlog.byName, dlog.prefix,
                                   dlog.includeFirst, dlog.firstFileName,
                                   dlog.overwrite);
   else
      return ExportMultipleByTrack(project, dlog.dir, dlog.format,
                                   dlog.byName, dlog.prefix,
                                   dlog.overwrite);
}

//
// ExportMultipleDialog methods
//

BEGIN_EVENT_TABLE(ExportMultipleDialog, wxDialog)
   EVT_BUTTON(wxID_OK, ExportMultipleDialog::OnExport)
   EVT_BUTTON(wxID_CANCEL, ExportMultipleDialog::OnCancel)
   EVT_BUTTON(ChooseID, ExportMultipleDialog::OnChoose)
   EVT_RADIOBUTTON(LabelID, ExportMultipleDialog::OnLabel)
   EVT_RADIOBUTTON(TrackID, ExportMultipleDialog::OnTrack)
   EVT_RADIOBUTTON(ByNameID, ExportMultipleDialog::OnByName)
   EVT_RADIOBUTTON(ByNumberID, ExportMultipleDialog::OnByNumber)
   EVT_CHECKBOX(FirstID, ExportMultipleDialog::OnFirst)
   EVT_TEXT(FirstFileNameID, ExportMultipleDialog::OnFirstFileName)
   EVT_TEXT(PrefixID, ExportMultipleDialog::OnPrefix)
END_EVENT_TABLE()

ExportMultipleDialog::ExportMultipleDialog(wxWindow *parent, wxWindowID id):
   wxDialog(parent, id, (wxString)_("Export Multiple"))
{
   wxBoxSizer *vSizer, *hSizer, *bigHSizer;
   wxStaticBoxSizer *group;
   wxStaticBox *staticBox;

   vSizer = new wxBoxSizer(wxVERTICAL);

   //
   // Format and location
   //

   hSizer = new wxBoxSizer(wxHORIZONTAL);

   hSizer->Add(new wxStaticText(this, -1, _("Export format:")),
               0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

   int numFormats = 3;
   wxString formatList[3];
   int format = ReadExportFormatPref();
   formatList[0] = sf_header_shortname(format & SF_FORMAT_TYPEMASK);
   formatList[1] = "MP3";
   formatList[2] = "Ogg Vorbis";
   mFormat = new wxChoice(this, FormatID,
                          wxDefaultPosition, wxDefaultSize,
                          numFormats, formatList);
   hSizer->Add(mFormat, 0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

   vSizer->Add(hSizer, 0, wxALL, 5);

   hSizer = new wxBoxSizer(wxHORIZONTAL);

   hSizer->Add(new wxStaticText(this, -1, _("Export location:")),
               0, wxALL | wxALIGN_CENTER_VERTICAL, 5);

   mDir = new wxTextCtrl(this, DirID);
   hSizer->Add(mDir, 1, wxALL | wxEXPAND, 5);

   mChoose = new wxButton(this, ChooseID, _("Choose..."));
   hSizer->Add(mChoose, 0, wxALL, 5);

   vSizer->Add(hSizer, 0, wxLEFT | wxRIGHT | wxEXPAND, 5);

   //
   // By labels or by tracks?
   //

   bigHSizer = new wxBoxSizer(wxHORIZONTAL);

   staticBox = new wxStaticBox(this, -1,
                               _("Split files based on:"));
   group = new wxStaticBoxSizer(staticBox, wxVERTICAL);

   mLabel = new wxRadioButton(this, LabelID, _("Labels"),
                              wxDefaultPosition, wxDefaultSize,
                              wxRB_GROUP);
   group->Add(mLabel, 0, wxALL, 5);

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   hSizer->Add(20, 5); // indent
   mFirst = new wxCheckBox(this, FirstID,
                           _("Include audio before first label"));
   hSizer->Add(mFirst, 0, wxLEFT | wxRIGHT, 5);
   group->Add(hSizer, 0, wxLEFT | wxRIGHT, 5);

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   hSizer->Add(20, 5); // indent
   hSizer->Add(new wxStaticText(this, -1, _("First file name:")),
               0, wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL, 5);
   mFirstFileName = new wxTextCtrl(this, FirstFileNameID);
   hSizer->Add(mFirstFileName, 0,
               wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL, 5);
   group->Add(hSizer, 0, wxLEFT | wxRIGHT, 5);

   mTrack = new wxRadioButton(this, TrackID, _("Tracks"));
   group->Add(mTrack, 0, wxALL, 5);
                                  
   bigHSizer->Add(group, 0, wxALL, 5);

   //
   // How to name files?
   //

   staticBox = new wxStaticBox(this, -1,
                               _("Name files:"));
   group = new wxStaticBoxSizer(staticBox, wxVERTICAL);

   mByName= new wxRadioButton(this, ByNameID, _("Using Label/Track Name"),
                              wxDefaultPosition, wxDefaultSize,
                              wxRB_GROUP);
   group->Add(mByName, 0, wxALL, 5);   

   mByNumber = new wxRadioButton(this, ByNumberID,
                                 _("Numbering consecutively"));
   group->Add(mByNumber, 0, wxALL, 5);

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   hSizer->Add(20, 5); // indent
   hSizer->Add(new wxStaticText(this, -1, _("File name prefix:")),
               0, wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL, 5);
   mPrefix = new wxTextCtrl(this, PrefixID);
   hSizer->Add(mPrefix, 0,
               wxLEFT | wxRIGHT | wxALIGN_CENTER_VERTICAL, 5);
   group->Add(hSizer, 0, wxLEFT | wxRIGHT, 5);   

   bigHSizer->Add(group, 0, wxALL, 5);

   vSizer->Add(bigHSizer, 0);

   //
   // Overwrite existing files?
   //

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   mOverwrite = new wxCheckBox(this, ExplanationID, 
                               _("Overwrite existing files"));
   mOverwrite->SetValue(true);
   hSizer->Add(mOverwrite, 1, wxALL | wxEXPAND, 5);
   vSizer->Add(hSizer, 0, wxALL | wxEXPAND, 5);

   //
   // Explanation area
   //
   //hSizer = new wxBoxSizer(wxHORIZONTAL);
   //mExplanation = new wxTextCtrl(this, ExplanationID, "Text\nText\nText");
   //hSizer->Add(mExplanation, 1, wxALL | wxEXPAND, 5);
   //vSizer->Add(hSizer, 0, wxALL | wxEXPAND, 5);

   //
   // Cancel / Export
   //

   hSizer = new wxBoxSizer(wxHORIZONTAL);

   mCancel = new wxButton(this, wxID_CANCEL, _("Cancel"));
   hSizer->Add(mCancel, 0, wxALL, 5);

   mExport = new wxButton(this, wxID_OK, _("Export"));
   mExport->SetDefault();
   hSizer->Add(mExport, 0, wxALL, 5);

   vSizer->Add(hSizer, 0, wxALL | wxALIGN_CENTER, 5);

   //
   // Finish
   //

   SetAutoLayout(true);
   SetSizer(vSizer);
   vSizer->Fit(this);
   vSizer->SetSizeHints(this);

   EnableControls();
}

void ExportMultipleDialog::EnableControls()
{
   mFirst->Enable(mLabel->GetValue() && mByName->GetValue());
   mFirstFileName->Enable(mLabel->GetValue() &&
                          mByName->GetValue() &&
                          mFirst->GetValue());

   mPrefix->Enable(mByNumber->GetValue());

   bool ok = true;

   if (mLabel->GetValue() && mFirst->GetValue() &&
       mFirstFileName->GetValue() == "")
      ok = false;
   if (mByNumber->GetValue() &&
       mPrefix->GetValue() == "")
      ok = false;

   mExport->Enable(ok);
}

void ExportMultipleDialog::CopyDataToControls()
{
   wxString defaultPath = gPrefs->Read("/DefaultExportPath",
                                       FROMFILENAME(::wxGetCwd()));
   wxString path = gPrefs->Read("/DefaultMultiplExportPath",
                                defaultPath);
   mDir->SetValue(path);

   mFirstFileName->SetValue(firstFileName);
   mPrefix->SetValue(prefix);

   if (!byLabelsAllowed) {
      mLabel->Enable(false);
      mTrack->SetValue(true);
      mLabel->SetValue(false);
   }
   if (!byTracksAllowed) {
      mTrack->Enable(false);
      mLabel->SetValue(true);
      mTrack->SetValue(false);
   }

   EnableControls();
}

void ExportMultipleDialog::OnChoose(wxCommandEvent& event)
{
   wxDirDialog dlog(this,
                    _("Choose a location to save the exported files"),
                    mDir->GetValue());
   dlog.ShowModal();
   if (dlog.GetPath() != "")
      mDir->SetValue(dlog.GetPath());
}

void ExportMultipleDialog::OnLabel(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultipleDialog::OnFirst(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultipleDialog::OnFirstFileName(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultipleDialog::OnTrack(wxCommandEvent& event)
{
   EnableControls();
}


   //

void ExportMultipleDialog::OnByName(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultipleDialog::OnByNumber(wxCommandEvent& event)
{
   EnableControls();
}


   //

void ExportMultipleDialog::OnPrefix(wxCommandEvent& event)
{
   EnableControls();
}

   //

void ExportMultipleDialog::OnCancel(wxCommandEvent& event)
{
   EndModal(0);
}

void ExportMultipleDialog::OnExport(wxCommandEvent& event)
{
   format = mFormat->GetSelection();
   dir = mDir->GetValue();
   byLabels = mLabel->GetValue();
   includeFirst = mFirst->GetValue();
   byName = mByName->GetValue();
   firstFileName = mFirstFileName->GetValue();
   prefix = mPrefix->GetValue();
   overwrite = mOverwrite->GetValue();

   gPrefs->Write("/DefaultMultiplExportPath", dir);

   EndModal(1);
}

