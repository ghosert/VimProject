/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile.cpp

  Joshua Haberman
  Dominic Mazzoni

**********************************************************************/

#include <float.h>
#include <math.h>

#include <wx/utils.h>
#include <wx/ffile.h>

#include "BlockFile.h"
#include "Internat.h"

const int headerTagLen = 20;
char headerTag[headerTagLen + 1] = "AudacityBlockFile112";

SummaryInfo::SummaryInfo(sampleCount samples)
{
   format = floatSample;

   fields = 3; /* min, max, rms */

   bytesPerFrame = sizeof(float) * fields;

   frames64K = (samples + 65535) / 65536;
   frames256 = frames64K * 256;

   offset64K = headerTagLen;
   offset256 = offset64K + (frames64K * bytesPerFrame);
   totalSummaryBytes = offset256 + (frames256 * bytesPerFrame);
}

/// Initializes the base BlockFile data.  The block is initially
/// unlocked and its reference count is 1.
///
/// @param fileName The name of the disk file associated with this
///                 BlockFile.  Not all BlockFiles will store their
///                 sample data here (for example, AliasBlockFiles
///                 read their data from elsewhere), but all BlockFiles
///                 will store at least the summary data here.
///
/// @param samples  The number of samples this BlockFile contains.
BlockFile::BlockFile(wxFileName fileName, sampleCount samples):
   mLockCount(0),
   mRefCount(1),
   mFileName(fileName),
   mLen(samples),
   mSummaryInfo(samples)
{
}

BlockFile::~BlockFile()
{
   if (!IsLocked() && mFileName.HasName())
      wxRemoveFile(FILENAME(mFileName.GetFullPath()));
}

/// Returns the file name of the disk file associated with this
/// BlockFile.  Not all BlockFiles store their sample data here,
/// but all BlockFiles have at least their summary data here.
wxFileName BlockFile::GetFileName()
{
   return mFileName;
}

/// Marks this BlockFile as "locked."  A locked BlockFile may not
/// be moved or deleted, only copied.  Locking a BlockFile prevents
/// it from disappearing if the project is saved in a different location.
/// When doing a "Save As," Audacity locks all blocks belonging
/// to the already-existing project, to ensure that the existing
/// project remains valid with all the blocks it needs.  Audacity
/// also locks the blocks of the last saved version of a project when
/// the project is deleted so that the files aren't deleted when their
/// refcount hits zero.
void BlockFile::Lock()
{
   mLockCount++;
}

/// Marks this BlockFile as "unlocked."
void BlockFile::Unlock()
{
   mLockCount--;
}

/// Returns true if the block is locked.
bool BlockFile::IsLocked()
{
   return mLockCount > 0;
}

/// Increases the reference count of this block by one.  Only
/// DirManager should call this method.
void BlockFile::Ref()
{
   mRefCount++;
}

/// Decreases the reference count of this block by one.  If this
/// causes the count to become zero, deletes the associated disk
/// file and deletes this object
bool BlockFile::Deref()
{
   mRefCount--;
   if (mRefCount <= 0) {
      delete this;
      return true;
   } else
      return false;
}

/// Get a buffer containing a summary block describing this sample
/// data.  This must be called by derived classes when they
/// are constructed, to allow them to construct their summary data,
/// after which they should write that data to their disk file.
///
/// This method also has the side effect of setting the mMin, mMax,
/// and mRMS members of this class.
///
/// You must not delete the returned buffer; it is static to this
/// method.
///
/// @param buffer A buffer containing the sample data to be analyzed
/// @param len    The length of the sample data
/// @param format The format of the sample data.
void *BlockFile::CalcSummary(samplePtr buffer, sampleCount len,
                             sampleFormat format)
{
   // all we have to do is ensure that the real summary is never bigger
   // than this
   static char fullSummary[200000];

   memcpy(fullSummary, headerTag, headerTagLen);

   float *summary64K = (float *)(fullSummary + mSummaryInfo.offset64K);
   float *summary256 = (float *)(fullSummary + mSummaryInfo.offset256);

   float *fbuffer = new float[len];
   CopySamples(buffer, format,
               (samplePtr)fbuffer, floatSample, len);

   sampleCount sumLen;
   sampleCount i, j, jcount;

   float min, max;
   float sumsq;

   // Recalc 256 summaries
   sumLen = (len + 255) / 256;

   for (i = 0; i < sumLen; i++) {
      min = fbuffer[i * 256];
      max = fbuffer[i * 256];
      sumsq = ((float)min) * ((float)min);
      jcount = 256;
      if (i * 256 + jcount > len)
         jcount = len - i * 256;
      for (j = 1; j < jcount; j++) {
         float f1 = fbuffer[i * 256 + j];
         sumsq += ((float)f1) * ((float)f1);
         if (f1 < min)
            min = f1;
         else if (f1 > max)
            max = f1;
      }

      float rms = (float)sqrt(sumsq / jcount);

      summary256[i * 3] = min;
      summary256[i * 3 + 1] = max;
      summary256[i * 3 + 2] = rms;
   }
   for (i = sumLen; i < mSummaryInfo.frames256; i++) {
      summary256[i * 3] = 0.0f;
      summary256[i * 3 + 1] = 0.0f;
      summary256[i * 3 + 2] = 0.0f;
   }

   // Recalc 64K summaries
   sumLen = (len + 65535) / 65536;

   for (i = 0; i < sumLen; i++) {
      min = summary256[3 * i * 256];
      max = summary256[3 * i * 256 + 1];
      sumsq = (float)summary256[3 * i * 256 + 2];
      sumsq *= sumsq;

      for (j = 1; j < 256; j++) {
         if (summary256[3 * (i * 256 + j)] < min)
            min = summary256[3 * (i * 256 + j)];
         if (summary256[3 * (i * 256 + j) + 1] > max)
            max = summary256[3 * (i * 256 + j) + 1];
         float r1 = summary256[3 * (i * 256 + j) + 2];
         sumsq += r1*r1;
      }

      float rms = (float)sqrt(sumsq / 256);

      summary64K[i * 3] = min;
      summary64K[i * 3 + 1] = max;
      summary64K[i * 3 + 2] = rms;
   }
   for (i = sumLen; i < mSummaryInfo.frames64K; i++) {
      summary64K[i * 3] = 0.0f;
      summary64K[i * 3 + 1] = 0.0f;
      summary64K[i * 3 + 2] = 0.0f;
   }

   // Recalc block-level summary
   min = summary64K[0];
   max = summary64K[1];
   sumsq = (float)summary64K[2];
   sumsq *= sumsq;

   for (i = 1; i < sumLen; i++) {
      if (summary64K[3*i] < min)
         min = summary64K[3*i];
      else if (summary64K[3*i+1] > max)
         max = summary64K[3*i+1];
      float r1 = (float)summary64K[3*i+2];
      sumsq += (r1*r1);
   }

   mMin = min;
   mMax = max;
   mRMS = sqrt(sumsq / sumLen);

   delete[] fbuffer;

   return fullSummary;
}

static void ComputeMinMax256(float *summary256,
                             float *outMin, float *outMax, int *outBads)
{
   float min, max;
   int i;
   int bad = 0;

   min = 1.0;
   max = -1.0;
   for(i=0; i<256; i++) {
      if (summary256[3*i] < min)
         min = summary256[3*i];
      else if (!(summary256[3*i] >= min))
         bad++;
      if (summary256[3*i+1] > max)
         max = summary256[3*i+1];
      else if (!(summary256[3*i+1] <= max))
         bad++;
   }

   *outMin = min;
   *outMax = max;
   *outBads = bad;
}

/// Byte-swap the summary data, in case it was saved by a system
/// on a different platform
void BlockFile::FixSummary(void *data)
{
   if (mSummaryInfo.format != floatSample ||
       mSummaryInfo.fields != 3)
      return;

   float *summary64K = (float *)((char *)data + mSummaryInfo.offset64K);
   float *summary256 = (float *)((char *)data + mSummaryInfo.offset256);

   float min, max;
   int bad;
   int i;

   ComputeMinMax256(summary256, &min, &max, &bad);

   if (min != summary64K[0] || max != summary64K[1] || bad > 0) {
      unsigned int *buffer = (unsigned int *)data;
      int len = mSummaryInfo.totalSummaryBytes / 4;

      for(i=0; i<len; i++)
         buffer[i] = wxUINT32_SWAP_ALWAYS(buffer[i]);

      ComputeMinMax256(summary256, &min, &max, &bad);
      if (min == summary64K[0] && max == summary64K[1] && bad == 0) {
         // The byte-swapping worked!
         return;
      }

      // Hmmm, no better, we should swap back

      for(i=0; i<len; i++)
         buffer[i] = wxUINT32_SWAP_ALWAYS(buffer[i]);
   }   
}

/// Retrieves the minimum, maximum, and maximum RMS of the
/// specified sample data in this block.
///
/// @param start The offset in this block where the region should begin
/// @param len   The number of samples to include in the region
/// @param *outMin A pointer to where the minimum value for this region
///                should be stored
/// @param *outMax A pointer to where the maximum value for this region
///                should be stored
/// @param *outRMS A pointer to where the maximum RMS value for this
///                region should be stored.
void BlockFile::GetMinMax(sampleCount start, sampleCount len,
                  float *outMin, float *outMax, float *outRMS)
{
   // TODO: actually use summaries
   samplePtr blockData = NewSamples(len, floatSample);
   this->ReadData(blockData, floatSample, start, len);

   float min = FLT_MAX;
   float max = -FLT_MAX;
   float sumsq = 0;

   for( int i = 0; i < len; i++ )
   {
      float sample = ((float*)blockData)[i];

      if( sample > max )
         max = sample;
      if( sample < min )
         min = sample;
      sumsq = (sample*sample);
   }

   DeleteSamples(blockData);

   *outMin = min;
   *outMax = max;
   *outRMS = sqrt(sumsq/len);
}

/// Retrieves the minimum, maximum, and maximum RMS of this entire
/// block.  This is faster than the other GetMinMax function since
/// these values are already computed.
///
/// @param *outMin A pointer to where the minimum value for this block
///                should be stored
/// @param *outMax A pointer to where the maximum value for this block
///                should be stored
/// @param *outRMS A pointer to where the maximum RMS value for this
///                block should be stored.
void BlockFile::GetMinMax(float *outMin, float *outMax, float *outRMS)
{
   *outMin = mMin;
   *outMax = mMax;
   *outRMS = mRMS;
}

/// Retrieves a portion of the 256-byte summary buffer from this BlockFile.  This
/// data provides information about the minimum value, the maximum
/// value, and the maximum RMS value for every group of 256 samples in the
/// file.
///
/// @param *buffer The area where the summary information will be
///                written.  It must be at least len*3 long.
/// @param start   The offset in 256-sample increments
/// @param len     The number of 256-sample summary frames to read
bool BlockFile::Read256(float *buffer,
                        sampleCount start, sampleCount len)
{
   wxASSERT(start >= 0);

   char *summary = new char[mSummaryInfo.totalSummaryBytes];
   if (!this->ReadSummary(summary)) {
      // TODO handle missing file
      return false;
   }

   if (start+len > mSummaryInfo.frames256)
      len = mSummaryInfo.frames256 - start;

   CopySamples(summary + mSummaryInfo.offset256 + (start * mSummaryInfo.bytesPerFrame),
               mSummaryInfo.format,
               (samplePtr)buffer, floatSample, len * mSummaryInfo.fields);

   if (mSummaryInfo.fields == 2) {
      // No RMS info
      int i;
      for(i=len-1; i>=0; i--) {
         buffer[3*i+2] = (fabs(buffer[2*i]) + fabs(buffer[2*i+1]))/4.0;
         buffer[3*i+1] = buffer[2*i+1];
         buffer[3*i] = buffer[2*i];
      }
   }

   delete[] summary;

   return true;
}

/// Retrieves a portion of the 64K summary buffer from this BlockFile.  This
/// data provides information about the minimum value, the maximum
/// value, and the maximum RMS value for every group of 64K samples in the
/// file.
///
/// @param *buffer The area where the summary information will be
///                written.  It must be at least len*3 long.
/// @param start   The offset in 64K-sample increments
/// @param len     The number of 64K-sample summary frames to read
bool BlockFile::Read64K(float *buffer,
                        sampleCount start, sampleCount len)
{
   wxASSERT(start >= 0);

   char *summary = new char[mSummaryInfo.totalSummaryBytes];
   if (!this->ReadSummary(summary)) {
      // TODO handle missing file
      return false;
   }

   if (start+len > mSummaryInfo.frames64K)
      len = mSummaryInfo.frames64K - start;

   CopySamples(summary + mSummaryInfo.offset64K + (start * mSummaryInfo.bytesPerFrame),
               mSummaryInfo.format,
               (samplePtr)buffer, floatSample, len*mSummaryInfo.fields);

   if (mSummaryInfo.fields == 2) {
      // No RMS info; make guess
      int i;
      for(i=len-1; i>=0; i--) {
         buffer[3*i+2] = (fabs(buffer[2*i]) + fabs(buffer[2*i+1]))/4.0;
         buffer[3*i+1] = buffer[2*i+1];
         buffer[3*i] = buffer[2*i];
      }
   }

   delete[] summary;

   return true;
}

/// Constructs an AliasBlockFile based on the given information about
/// the aliased file.
///
/// Note that derived classes /must/ call AliasBlockFile::WriteSummary()
/// in their constructors for the summary file to be correctly written
/// to disk.
///
/// @param baseFileName The name of the summary file to be written, but
///                     without an extension.  This constructor will add
///                     the appropriate extension before passing it to
///                     BlockFile::BlockFile
/// @param aliasedFileName The name of the file where the audio data for
///                     this block actually exists.
/// @param aliasStart   The offset in the aliased file where this block's
///                     data begins
/// @param aliasLen     The length of this block's data in the aliased
///                     file.
/// @param aliasChannel The channel where this block's data is located in
///                     the aliased file
AliasBlockFile::AliasBlockFile(wxFileName baseFileName,
                               wxFileName aliasedFileName,
                               sampleCount aliasStart,
                               sampleCount aliasLen, int aliasChannel):
   BlockFile(wxFileName(baseFileName.GetFullPath() + ".auf"), aliasLen),
   mAliasedFileName(aliasedFileName),
   mAliasStart(aliasStart),
   mAliasChannel(aliasChannel)
{
}

AliasBlockFile::AliasBlockFile(wxFileName existingSummaryFile,
                               wxFileName aliasedFileName, sampleCount aliasStart,
                               sampleCount aliasLen, int aliasChannel,
                               float min, float max, float rms):
   BlockFile(existingSummaryFile, aliasLen),
   mAliasedFileName(aliasedFileName),
   mAliasStart(aliasStart),
   mAliasChannel(aliasChannel)
{
   if( !wxFileExists(FILENAME(existingSummaryFile.GetFullPath())))
   {
      // Throw an exception?
      return;
   }

   mMin = min;
   mMax = max;
   mRMS = rms;
}
/// Write the summary to disk.  Derived classes must call this method
/// from their constructors for the summary to be correctly written.
/// It uses the derived class's ReadData() to retrieve the data to
/// summarize.
void AliasBlockFile::WriteSummary()
{
   wxASSERT( !wxFileExists(FILENAME(mFileName.GetFullPath())));
   // I would much rather have this code as part of the constructor, but
   // I can't call virtual functions from the constructor.  So we just
   // need to ensure that every derived class calls this in *its* constructor
   wxFFile summaryFile;

   if( !summaryFile.Open(FILENAME(mFileName.GetFullPath()), "wb") )
      // failed.  what to do?
      return;

   // To build the summary data, call ReadData (implemented by the
   // derived classes) to get the sample data
   samplePtr sampleData = NewSamples(mLen, floatSample);
   this->ReadData(sampleData, floatSample, 0, mLen);

   void *summaryData = BlockFile::CalcSummary(sampleData, mLen,
                                            floatSample);
   summaryFile.Write(summaryData, mSummaryInfo.totalSummaryBytes);

   DeleteSamples(sampleData);
}

AliasBlockFile::~AliasBlockFile()
{
}

/// Read the summary of this alias block from disk.  Since the audio data
/// is elsewhere, this consists of reading the entire summary file.
///
/// @param *data The buffer where the summary data will be stored.  It must
///              be at least mSummaryInfo.totalSummaryBytes long.
bool AliasBlockFile::ReadSummary(void *data)
{
   wxFFile summaryFile;

   if( !summaryFile.Open(FILENAME(mFileName.GetFullPath()), "rb") )
      return false;

   int read = summaryFile.Read(data, (size_t)mSummaryInfo.totalSummaryBytes);

   FixSummary(data);

   return (read == mSummaryInfo.totalSummaryBytes);
}

/// Get the name of the file where the audio data for this block is
/// stored.
wxFileName AliasBlockFile::GetAliasedFile()
{
   return mAliasedFileName;
}

/// Modify this block to point at a different file.  This is generally
/// looked down on, but it is necessary in one case: see
/// DirManager::EnsureSafeFilename().
void AliasBlockFile::ChangeAliasedFile(wxFileName newAliasedFile)
{
   mAliasedFileName = newAliasedFile;
}

int AliasBlockFile::GetSpaceUsage()
{
   wxFFile summaryFile(FILENAME(mFileName.GetFullPath()));
   return summaryFile.Length();
}

