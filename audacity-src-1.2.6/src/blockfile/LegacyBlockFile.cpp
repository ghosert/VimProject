/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyBlockFile.cpp

  Dominic Mazzoni

  Audacity 1.1.0 block file format:

  1. Header tag: 20 bytes
     "AudacityBlockFile110"
  2. 64K summaries (min, max, RMS, each a 4-byte float)
  3. 256 summaries (min, max, RMS, each a 4-byte float)

**********************************************************************/

#include <float.h>
#include <math.h>

#include <wx/ffile.h>
#include <wx/utils.h>

#include "LegacyBlockFile.h"
#include "../FileFormats.h"
#include "../Internat.h"

#include "sndfile.h"

void ComputeLegacySummaryInfo(wxFileName fileName,
                              int summaryLen,
                              sampleFormat format,
                              SummaryInfo *info,
                              bool noRMS,
                              float *min, float *max, float *rms)
{
   int fields = 3; /* min, max, rms */

   if (noRMS)
      fields = 2;

   info->fields = fields;
   info->format = format;
   info->bytesPerFrame =
      SAMPLE_SIZE(info->format) * fields;
   info->totalSummaryBytes = summaryLen;
   info->offset64K = 20; /* legacy header tag len */
   info->frames64K = (summaryLen-20) /
      (info->bytesPerFrame * 256);
   info->offset256 = info->offset64K +
      (info->frames64K * info->bytesPerFrame);
   info->frames256 =
      (summaryLen - 20 -
       (info->frames64K * info->bytesPerFrame)) /
      info->bytesPerFrame;

   //
   // Compute the min, max, and RMS of the block from the
   // 64K summary data
   //

   float *summary = new float[info->frames64K * fields];
   samplePtr data = NewSamples(info->frames64K * fields,
                               info->format);

   wxFFile summaryFile;
   if( !summaryFile.Open(fileName.GetFullPath(), "rb") ) {
      delete[] data;
      return;
   }
   summaryFile.Seek(info->offset64K);
   int read = summaryFile.Read(data,
                               info->frames64K *
                               info->bytesPerFrame);

   int count = read / info->bytesPerFrame;

   CopySamples(data, info->format,
               (samplePtr)summary, floatSample, count);

   (*min) = FLT_MAX;
   (*max) = FLT_MIN;
   float sumsq = 0;

   for(int i=0; i<count; i++) {
      if (summary[fields*i] < (*min))
         (*min) = summary[fields*i];
      if (summary[fields*i+1] > (*max))
         (*max) = summary[fields*i+1];
      if (fields >= 3)
         sumsq += summary[fields*i+2]*summary[fields*i+2];
   }
   if (fields >= 3)
      (*rms) = sqrt(sumsq / count);
   else
      (*rms) = 0;

   DeleteSamples(data);
   delete[] summary;   
}

/// Construct a LegacyBlockFile memory structure that will point to an
/// existing block file.  This file must exist and be a valid block file.
///
/// @param existingFile The disk file this LegacyBlockFile should use.
LegacyBlockFile::LegacyBlockFile(wxFileName existingFile,
                                 sampleFormat format,
                                 sampleCount summaryLen,
                                 sampleCount len,
                                 bool noRMS):
   BlockFile(existingFile, len),
   mFormat(format)
{
   if( !wxFileExists(FILENAME(existingFile.GetFullPath())) )
      // throw an exception?
      ;

   sampleFormat summaryFormat;

   if (noRMS)
      summaryFormat = int16Sample;
   else
      summaryFormat = floatSample;

   ComputeLegacySummaryInfo(existingFile,
                            summaryLen, summaryFormat,
                            &mSummaryInfo, noRMS,
                            &mMin, &mMax, &mRMS);
}

LegacyBlockFile::~LegacyBlockFile()
{
}

/// Read the summary section of the disk file.
///
/// @param *data The buffer to write the data to.  It must be at least
/// mSummaryinfo.totalSummaryBytes long.
bool LegacyBlockFile::ReadSummary(void *data)
{
   wxFFile summaryFile;

   if( !summaryFile.Open(mFileName.GetFullPath(), "rb") )
      return false;

   int read = summaryFile.Read(data, (size_t)mSummaryInfo.totalSummaryBytes);

   return (read == mSummaryInfo.totalSummaryBytes);
}

/// Read the data portion of the block file using libsndfile.  Convert it
/// to the given format if it is not already.
///
/// @param data   The buffer where the data will be stored
/// @param format The format the data will be stored in
/// @param start  The offset in this block file
/// @param len    The number of samples to read
int LegacyBlockFile::ReadData(samplePtr data, sampleFormat format,
                              sampleCount start, sampleCount len)
{
   SF_INFO info;

   memset(&info, 0, sizeof(info));

   switch(mFormat) {
   case int16Sample:
      info.format =
         SF_FORMAT_RAW | SF_FORMAT_PCM_16 | SF_ENDIAN_CPU;
      break;
   default:
   case floatSample:
      info.format =
         SF_FORMAT_RAW | SF_FORMAT_FLOAT | SF_ENDIAN_CPU;
      break;
   case int24Sample:
      info.format = SF_FORMAT_RAW | SF_FORMAT_PCM_32 | SF_ENDIAN_CPU;
      break;
   }
   info.samplerate = 44100; // Doesn't matter
   info.channels = 1;
   info.frames = mLen + (mSummaryInfo.totalSummaryBytes /
                         SAMPLE_SIZE(mFormat));
   
   SNDFILE *sf = sf_open(FILENAME(mFileName.GetFullPath()), SFM_READ, &info);

   if (!sf)
      return 0;

   sf_count_t seekstart = start +
      (mSummaryInfo.totalSummaryBytes / SAMPLE_SIZE(mFormat));
   sf_seek(sf, seekstart , SEEK_SET);

   samplePtr buffer = NewSamples(len, floatSample);

   int framesRead = 0;

   // If both the src and dest formats are integer formats,
   // read integers from the file (otherwise we would be
   // converting to float and back, which is unneccesary)
   if (format == int16Sample &&
       sf_subtype_is_integer(info.format)) {
      framesRead = sf_readf_short(sf, (short *)data, len);
   }
   else
   if (format == int24Sample &&
       sf_subtype_is_integer(info.format))
   {
      framesRead = sf_readf_int(sf, (int *)data, len);

      // libsndfile gave us the 3 byte sample in the 3 most
      // significant bytes -- we want it in the 3 least
      // significant bytes.
      int *intPtr = (int *)data;
      for( int i = 0; i < framesRead; i++ )
         intPtr[i] = intPtr[i] >> 8;
   }
   else {
      // Otherwise, let libsndfile handle the conversion and
      // scaling, and pass us normalized data as floats.  We can
      // then convert to whatever format we want.
      framesRead = sf_readf_float(sf, (float *)buffer, len);
      CopySamples(buffer, floatSample,
                  (samplePtr)data, format, framesRead);
   }

   DeleteSamples(buffer);

   sf_close(sf);

   return framesRead;
}

void LegacyBlockFile::SaveXML(int depth, wxFFile &xmlFile)
{
   for(int i = 0; i < depth; i++)
      xmlFile.Write("\t");
   xmlFile.Write("<legacyblockfile ");
   xmlFile.Write(wxString::Format("name='%s' ",
                                  XMLTagHandler::XMLEsc(mFileName.GetFullName()).c_str()));
   xmlFile.Write(wxString::Format("len='%d' ", mLen));
   if (mSummaryInfo.fields < 3)
      xmlFile.Write(wxString::Format("norms='1' "));
   xmlFile.Write(wxString::Format("summarylen='%d' ", mSummaryInfo.totalSummaryBytes));
   xmlFile.Write("/>\n");
}

/// static
BlockFile *LegacyBlockFile::BuildFromXML(wxString projDir, const char **attrs,
                                         sampleCount len, sampleFormat format)
{
   wxFileName fileName;
   sampleCount summaryLen = 0;
   int noRMS = 0;

   while(*attrs)
   {
       const char *attr =  *attrs++;
       const char *value = *attrs++;

       if( !strcmp(attr, "name") )
          fileName.Assign(projDir, value);
       if( !strcmp(attr, "len") )
          len = atoi(value);
       if( !strcmp(attr, "norms") )
          noRMS = (bool)atoi(value);
       if( !strcmp(attr, "format") )
          format = (sampleFormat)atoi(value);
       if( !strcmp(attr, "summarylen") )
          summaryLen = atoi(value);
   }

   return new LegacyBlockFile(fileName, format, summaryLen, len, noRMS);
}

/// Create a copy of this BlockFile, but using a different disk file.
///
/// @param newFileName The name of the new file to use.
BlockFile *LegacyBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile = new LegacyBlockFile(newFileName,
                                                 mFormat,
                                                 mSummaryInfo.totalSummaryBytes,
                                                 mLen,
                                                 mSummaryInfo.fields < 3);

   return newBlockFile;
}

int LegacyBlockFile::GetSpaceUsage()
{
   wxFFile dataFile(mFileName.GetFullPath());
   return dataFile.Length();
}

