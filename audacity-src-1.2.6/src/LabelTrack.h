/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _LABELTRACK_
#define _LABELTRACK_

#include <wx/brush.h>
#include <wx/font.h>
#include <wx/pen.h>
#include <wx/dynarray.h>
#include <wx/string.h>

#include "Track.h"

class wxKeyEvent;
class wxTextFile;
class wxWindow;
class TrackList;

class AudacityProject;
class DirManager;

struct LabelStruct {
   double t;
   double t1;
   wxString title;
   int width;
};

WX_DEFINE_ARRAY(LabelStruct *, LabelArray);

class LabelTrack:public Track {
   friend class BouncePane;
   friend bool ExportPCM(AudacityProject *project,
               wxString format, bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1);

 public:
   LabelTrack(DirManager * projDirManager);
   LabelTrack(const LabelTrack &orig);

   virtual ~ LabelTrack();

   void Draw(wxDC & dc, wxRect & r, double h, double pps,
             double sel0, double sel1);

   virtual int GetKind() const { return Label; } 

   virtual double GetStartTime();
   virtual double GetEndTime();

   virtual Track *Duplicate() { return new LabelTrack(*this); }

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

#if LEGACY_PROJECT_FILE_SUPPORT
   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);
#endif

   virtual bool Cut  (double t0, double t1, Track ** dest);
   virtual bool Copy (double t0, double t1, Track ** dest) const;
   virtual bool Clear(double t0, double t1);
   virtual bool Paste(double t, const Track * src);

   virtual bool Silence(double t0, double t1);
   virtual bool InsertSilence(double t, double len);

   void MouseDown(int x, int y, wxRect & r, double h, double pps,
                           double *newSel0, double *newSel1);

   void KeyEvent(double sel0, double sel1, wxKeyEvent & event);

   void Import(wxTextFile & f);
   void Export(wxTextFile & f);

   void Unselect();

   bool IsSelected() const;

   int GetNumLabels() const;
   const LabelStruct *GetLabel(int index) const;

   void AddLabel(double t, double t1, const wxString &title = "");

   static void ResetFont();

 private:

   int mSelIndex;

   LabelArray mLabels;

   // Used only for a LabelTrack on the clipboard
   double mClipLen;

   static wxFont msFont;
};

#endif
