/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile.h

  Joshua Haberman
  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_BLOCKFILE__
#define __AUDACITY_BLOCKFILE__

#include <wx/string.h>
#include <wx/ffile.h>
#include <wx/filename.h>

#include "WaveTrack.h"

#include "xml/XMLTagHandler.h"

class wxFFile;

class SummaryInfo {
 public:
   SummaryInfo(sampleCount samples);

   int            fields; /* Usually 3 for Min, Max, RMS */
   sampleFormat   format;
   int            bytesPerFrame;
   sampleCount    frames64K;
   int            offset64K;
   sampleCount    frames256;
   int            offset256;
   int            totalSummaryBytes;
};

/// A BlockFile is a chunk of immutable audio data.

/// A BlockFile represents a chunk of audio data.  These chunks are
/// assembled into sequences by the class Sequence.  These classes
/// are at the heart of how Audacity stores audio data.
///
/// BlockFile is an abstract base class that can be implemented in
/// many different ways.  However it does have a fairly large amount
/// of shared code that deals with the physical file and manipulating
/// the summary data.
///
/// BlockFile should be thought of as an immutable class.  After it
/// is constructed, it is essentially never changed (though there are
/// a few exceptions).  Most notably, the audio data and summary data
/// are never altered once it is constructed.  This is important to
/// some of the derived classes that are actually aliases to audio
/// data stored in existing files.
///
/// BlockFiles are reference-counted, and deleted when their reference
/// count hits zero.  DirManager is the class responsible for
/// constructing and managing BlockFiles and managing their reference
/// counts.

class BlockFile {
 public:

   // Constructor / Destructor

   /// Construct a BlockFile.
   BlockFile(wxFileName fileName, sampleCount samples);
   virtual ~BlockFile();

   // Reading

   /// Retrieves audio data from this BlockFile
   virtual int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len) = 0;

   // Other Properties

   /// Stores a representation of this file in XML
   virtual void SaveXML(int depth, wxFFile &xmlFile) = 0;
   /// Gets the filename of the disk file associated with this BlockFile
   virtual wxFileName GetFileName();
   virtual sampleCount GetLength() { return mLen; }

   /// Locks this BlockFile, to prevent it from being moved
   virtual void Lock();
   /// Unlock this BlockFile, allowing it to be moved
   virtual void Unlock();
   /// Returns TRUE if this BlockFile is locked
   virtual bool IsLocked();

   /// Gets extreme values for the specified region
   virtual void GetMinMax(sampleCount start, sampleCount len,
                          float *outMin, float *outMax, float *outRMS);
   /// Gets extreme values for the entire block
   virtual void GetMinMax(float *outMin, float *outMax, float *outRMS);
   /// Returns the 256 byte summary data block
   virtual bool Read256(float *buffer, sampleCount start, sampleCount len);
   /// Returns the 64K summary data block
   virtual bool Read64K(float *buffer, sampleCount start, sampleCount len);

   /// Returns TRUE if this block references another disk file
   virtual bool IsAlias() { return false; }
   /// Create a new BlockFile identical to this, using the given filename
   virtual BlockFile *Copy(wxFileName newFileName) = 0;

   virtual int GetSpaceUsage() = 0;
 private:

   friend class DirManager;

   virtual void Ref();
   virtual bool Deref();

 protected:
   /// Calculate summary data for the given sample data
   virtual void *CalcSummary(samplePtr buffer, sampleCount len,
                             sampleFormat format);
   /// Read the summary section of the file.  Derived classes implement.
   virtual bool ReadSummary(void *data) = 0;

   /// Byte-swap the summary data, in case it was saved by a system
   /// on a different platform
   virtual void FixSummary(void *data);

 private:
   int mLockCount;
   int mRefCount;

 protected:
   wxFileName mFileName;
   sampleCount mLen;
   SummaryInfo mSummaryInfo;
   float mMin, mMax, mRMS;

};

/// A BlockFile that refers to data in an existing file

/// An AliasBlockFile references an existing disk file for its storage
/// instead of copying the data.  It still writes a file to disk, but
/// only stores summary data in it.
///
/// This is a common base class for all alias block files.  It handles
/// reading and writing summary data, leaving very little for derived
/// classes to need to implement.
class AliasBlockFile : public BlockFile
{
 public:

   // Constructor / Destructor

   /// Constructs an AliasBlockFile
   AliasBlockFile(wxFileName baseFileName,
                  wxFileName aliasedFileName, sampleCount aliasStart,
                  sampleCount aliasLen, int aliasChannel);
   AliasBlockFile(wxFileName existingSummaryFile,
                  wxFileName aliasedFileName, sampleCount aliasStart,
                  sampleCount aliasLen, int aliasChannel,
                  float min, float max, float RMS);
   virtual ~AliasBlockFile();

   // Reading

   /// Retrieves audio data from the aliased file.
   virtual int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len) = 0;

   virtual int GetSpaceUsage();
 private:
   // These methods are only for use by DirManager
   friend class DirManager;

   /// Gets the name of the aliased file.
   wxFileName GetAliasedFile();
   /// Modifies the name of the aliased file.
   void ChangeAliasedFile(wxFileName newAliasedFile);
   /// Returns TRUE if this is an AliasBlockFile
   bool IsAlias() { return true; }

 protected:
   /// Write the summary to disk, using the derived ReadData() to get the data
   virtual void WriteSummary();
   /// Read the summary into a buffer
   virtual bool ReadSummary(void *data);

   wxFileName  mAliasedFileName;
   sampleCount mAliasStart;
   int         mAliasChannel;
};

#endif

