/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_NOTETRACK__
#define __AUDACITY_NOTETRACK__

#include <wx/string.h>

#include "Track.h"

class wxDC;
class wxRect;

class DirManager;
class Seq;   // from "allegro.h"


class NoteTrack:public Track {
 public:
   friend class TrackArtist;

   NoteTrack(DirManager * projDirManager);

   virtual Track *Duplicate();
   
   virtual int GetKind() const { return Note; } 

   virtual double GetStartTime() { return 0.0; }
   virtual double GetEndTime() { return mLen; }

   void DrawLabelControls(wxDC & dc, wxRect & r);
   bool LabelClick(wxRect & r, int x, int y, bool right);

   void SetSequence(Seq *seq);

   int GetBottomNote() const { return mBottomNote; }
   void SetBottomNote(int note) 
   { 
      if (note < 0)
         note = 0;
      else if (note > 96)
         note = 96;

      mBottomNote = note; 
   }

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

 private:
   Seq *mSeq;
   double mLen;

   DirManager *mDirManager;

   int mBottomNote;

   int mVisibleChannels;

   void CalcLen();
};

#endif
