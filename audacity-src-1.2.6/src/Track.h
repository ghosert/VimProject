/**********************************************************************

  Audacity: A Digital Audio Editor

  Track.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK__
#define __AUDACITY_TRACK__

#include <wx/string.h>
#include <wx/dynarray.h>

#include "SampleFormat.h"
#include "xml/XMLTagHandler.h"

class wxTextFile;
class DirManager;
class UndoStack;
class TimeTrack;
class WaveTrack;

WX_DEFINE_ARRAY(WaveTrack*, WaveTrackArray);

class Track: public XMLTagHandler {

 // To be TrackDisplay
 protected:
   int        mHeight;
   wxString   mName;

   bool       mSelected;

   int        mDirty;

   bool       mLinked;

 public:
   int GetHeight() const { return mHeight; }
   void SetHeight( int h ) { mHeight = h; }

 // Keep in Track

 protected:
   int                 mChannel;
   double              mOffset;
   bool                mMute;
   bool                mSolo;

   mutable DirManager *mDirManager;

 public:

   enum {
      LeftChannel = 0,
      RightChannel = 1,
      MonoChannel = 2
   };

   enum {
      None,
      Wave,
      Note,
      Label,
      Time
   } TrackKindEnum;

   Track(DirManager * projDirManager);
   Track(const Track &orig);

   virtual ~ Track();
   
   void Init(const Track &orig);
   virtual Track *Duplicate() = 0;

   wxString GetName() const { return mName; }
   void SetName( wxString n ) { mName = n; }

   bool GetSelected() const { return mSelected; }
   bool GetMute    () const { return mMute;     }
   bool GetLinked  () const { return mLinked;   }
   bool GetSolo    () const { return mSolo;     }

   void SetSelected(bool s) { mSelected = s; }
   void SetMute    (bool m) { mMute     = m; }
   void SetLinked  (bool l) { mLinked   = l; }
   void SetSolo    (bool s) { mSolo     = s; }

   int    GetChannel() const { return mChannel; }
   double GetOffset () const { return mOffset; }
   int    GetDirty  () const { return mDirty;   }

   void Offset(double t) { SetOffset(mOffset + t); }
   virtual void SetOffset (double o) { mOffset = o; }

   void MarkChanged()  { mDirty++; }
   void SetChannel(int    c) { mChannel = c; }

   // AS: Note that the dirManager is mutable.  This is
   // mostly to support "Duplicate" of const objects,
   // but in general, mucking with the dir manager is
   // separate from the Track.
   DirManager* GetDirManager() const { return mDirManager; }

   virtual bool Cut  (double t0, double t1, Track ** dest) {return false;}
   virtual bool Copy (double t0, double t1, Track ** dest) {return false;}
   virtual bool Clear(double t0, double t1) {return false;}
   virtual bool Paste(double t, const Track * src) {return false;}

   virtual bool Silence(double t0, double t1) {return false;}
   virtual bool InsertSilence(double t, double len) {return false;}

   virtual int GetKind() const { return None; }

   // XMLTagHandler callback methods

   virtual bool HandleXMLTag(const char *tag, const char **attrs) = 0;
   virtual XMLTagHandler *HandleXMLChild(const char *tag) = 0;
   virtual void WriteXML(int depth, FILE *fp) = 0;

   // Returns true if an error was encountered while trying to
   // open the track from XML
   virtual bool GetErrorOpening() { return false; }

   virtual double GetStartTime() { return 0.0; }
   virtual double GetEndTime() { return 0.0; }
};

/*
  TrackList is a flat linked list of tracks supporting Add,
  Remove, Clear, and Contains, plus serialization of the
  list of tracks.
*/
struct TrackListNode {
   Track *t;
   TrackListNode *next;
   TrackListNode *prev;
};

class TrackList;

class TrackListIterator {
 public:
  TrackListIterator(TrackList * val);
  
  // Iterate functions
  Track *First();
  Track *Next();
  Track *RemoveCurrent();     // returns next
  
 private:
  TrackList * l;
  TrackListNode *cur;
};

class TrackList {
 public:
  // Create an empty TrackList
  TrackList();
  
  // Copy constructor
  TrackList(TrackList * t);
  
  // Destructor
  virtual ~TrackList();
  
  friend class TrackListIterator;
  friend class ConstTrackListIterator;
  
  // Add a this Track or all children of this TrackGroup
  void Add(Track * t);
  void AddToHead(Track * t);
  
  // Remove this Track or all children of this TrackGroup
  void Remove(Track * t);
  
  // Make the list empty
  void Clear(bool deleteTracks = false);
  
  // Select a track, and if it is linked to another track,
  // select it, too.
  void Select(Track * t, bool selected = true);
  
  // If this track is linked to another track (the track
  // immediately before or after it), return its partner.
  // Otherwise return null.
  Track *GetLink(Track * t) const;
  
  Track *GetPrev(Track * t) const;
  Track *GetNext(Track * t) const;
  
  bool CanMoveUp(Track * t) const;
  bool CanMoveDown(Track * t) const;
  
  bool MoveUp(Track * t);
  bool MoveDown(Track * t);
  bool Move(Track * t, bool up) { return up ? MoveUp(t) : MoveDown(t); }

  TimeTrack *GetTimeTrack();

  WaveTrackArray GetWaveTrackArray(bool selectionOnly);
  /* Consider this function depricated in favor of the above function */
  void GetWaveTracks(bool selectionOnly, int *num, WaveTrack ***tracks);
  
  // Test
  bool Contains(Track * t) const;
  
  bool IsEmpty() const;
  
  double GetStartTime() const;
  double GetEndTime() const;

  double GetMinOffset() const;
  int GetHeight() const;

#if LEGACY_PROJECT_FILE_SUPPORT  
  // File I/O
  virtual bool Load(wxTextFile * in, DirManager * dirManager);
  virtual bool Save(wxTextFile * out, bool overwrite);
#endif
  
  unsigned int GetSpaceUsage();
  unsigned int GetAdditionalSpaceUsage(UndoStack *stack);
 private:
  void Swap(TrackListNode * s1, TrackListNode * s2);
  
  TrackListNode *head;
  TrackListNode *tail;
};

class ConstTrackListIterator {
 public:
    ConstTrackListIterator(const TrackList * val) : l(val), cur(NULL) {}

    // Iterate functions
    Track *First() const
    {
       cur = l->head;

       if (cur) return cur->t;
       return NULL;
    }

    Track *Next() const
    {
       if (cur) cur = cur->next;
       if (cur) return cur->t;
       return NULL;
    }

 private:
    const TrackList * l;
    mutable TrackListNode *cur;
};


class WaveTrack;
class NoteTrack;
class LabelTrack;
class TimeTrack;
class DirManager;

class TrackFactory
{
 private:
   TrackFactory(DirManager *dirManager):
      mDirManager(dirManager)
   {
   }

   DirManager *mDirManager;
   friend class AudacityProject;
   friend class BenchmarkDialog;

 public:
   // These methods are defined in WaveTrack.cpp, NoteTrack.cpp,
   // LabelTrack.cpp, and TimeTrack.cpp respectively
   // and LabelTrack.cpp respectively
   WaveTrack *NewWaveTrack(sampleFormat format = (sampleFormat)0,
                           double rate = 0);
   NoteTrack *NewNoteTrack();
   LabelTrack *NewLabelTrack();
   TimeTrack *NewTimeTrack();
};

#endif
