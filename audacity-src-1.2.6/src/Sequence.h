/**********************************************************************

  Audacity: A Digital Audio Editor

  Sequence.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SEQUENCE__
#define __AUDACITY_SEQUENCE__

#include <wx/string.h>

#include "SampleFormat.h"
#include "xml/XMLTagHandler.h"

typedef int sampleCount;

class DirManager;
class BlockArray;
class SeqBlock;

class Sequence: public XMLTagHandler {
 public:

   // Temporary: only until we delete TrackArtist once and for all
   friend class TrackArtist;

   //
   // Static methods
   //

   static void SetMaxDiskBlockSize(int bytes);

   //
   // Constructor / Destructor / Duplicator
   //

   Sequence(DirManager * projDirManager, sampleFormat format);
   Sequence(const Sequence &orig);
   virtual ~Sequence();
   Sequence *Duplicate() const {return new Sequence(*this);}

   //
   // Editing
   //

   sampleCount GetNumSamples() const { return mNumSamples; }

   bool Get(samplePtr buffer, sampleFormat format,
            sampleCount start, sampleCount len) const;
   bool Set(samplePtr buffer, sampleFormat format,
            sampleCount start, sampleCount len);

   bool GetWaveDisplay(float *min, float *max, float *rms,
                       int len, sampleCount *where,
                       double samplesPerPixel);

   bool Copy(sampleCount s0, sampleCount s1, Sequence **dest);
   bool Paste(sampleCount s0, const Sequence *src);

   sampleCount GetIdealAppendLen();
   bool Append(samplePtr buffer, sampleFormat format, sampleCount len);
   bool Delete(sampleCount start, sampleCount len);
   bool AppendAlias(wxString fullPath,
                    sampleCount start,
                    sampleCount len, int channel);

   bool SetSilence(sampleCount s0, sampleCount len);
   bool InsertSilence(sampleCount s0, sampleCount len);

   //
   // XMLTagHandler callback methods for loading and saving
   //

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual void HandleXMLEndTag(const char *tag);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

   bool GetErrorOpening() { return mErrorOpening; }

   //
   // Lock/Unlock all of this sequence's BlockFiles, keeping them
   // from being moved.  Call this if you want to copy a
   // track to a different DirManager.  See BlockFile.h
   // for details.
   // 

   bool Lock();
   bool Unlock();

   //
   // Manipulating Sample Format
   //

   sampleFormat GetSampleFormat() const;
   bool SetSampleFormat(sampleFormat format);
   bool ConvertToSampleFormat(sampleFormat format);

   //
   // Retrieving summary info
   //

   bool GetMinMax(sampleCount start, sampleCount len,
                  float * min, float * max) const;

   //
   // Getting block size information
   //

   sampleCount GetBestBlockSize(sampleCount start) const;
   sampleCount GetMaxBlockSize() const;
   sampleCount GetIdealBlockSize() const;

   //
   // Temporary: to be deleted after TrackList::GetSpaceUsage()
   // is reimplemented
   //

   BlockArray *GetBlockArray() {return mBlock;}

 private:

   //
   // Private static variables
   // 

   static int    sMaxDiskBlockSize;

   //
   // Private variables
   //

   DirManager   *mDirManager;

   BlockArray   *mBlock;
   sampleFormat  mSampleFormat;
   sampleCount   mNumSamples;

   sampleCount   mMinSamples;
   sampleCount   mMaxSamples;

   bool          mErrorOpening;

   //
   // Private methods
   //

   void CalcSummaryInfo();

   int FindBlock(sampleCount pos) const;
   int FindBlock(sampleCount pos, sampleCount lo,
                 sampleCount guess, sampleCount hi) const;

   bool AppendBlock(SeqBlock *b);

   bool Read(samplePtr buffer, sampleFormat format,
             SeqBlock * b,
             sampleCount start, sampleCount len) const;

   // These are the two ways to write data to a block
   bool FirstWrite(samplePtr buffer, SeqBlock * b, sampleCount len);
   bool CopyWrite(samplePtr buffer, SeqBlock * b,
                  sampleCount start, sampleCount len);

   // Both block-writing methods and AppendAlias call this
   // method to write the summary data
   void *GetSummary(samplePtr buffer, sampleCount len,
                    float *min, float *max, float *rms);

   BlockArray *Blockify(samplePtr buffer, sampleCount len);

 public:

   //
   // Public methods intended for debugging only
   //

   // This function makes sure that the track isn't messed up
   // because of inconsistent block starts & lengths
   bool ConsistencyCheck(const char *whereStr);

   // This function prints information to stdout about the blocks in the
   // tracks and indicates if there are inconsistencies.
   void Debug();
   void DebugPrintf(wxString *dest);
};

#endif // __AUDACITY_SEQUENCE__

