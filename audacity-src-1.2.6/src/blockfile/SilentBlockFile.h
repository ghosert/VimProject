/**********************************************************************

  Audacity: A Digital Audio Editor

  SilentBlockFile.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SILENT_BLOCKFILE__
#define __AUDACITY_SILENT_BLOCKFILE__

#include <wx/string.h>
#include <wx/filename.h>

#include "../BlockFile.h"

/// A BlockFile containing nothing but silence.  Saves disk space.
class SilentBlockFile : public BlockFile {
 public:

   // Constructor / Destructor

   SilentBlockFile(sampleCount sampleLen);

   virtual ~SilentBlockFile();

   // Reading

   /// Read the summary section of the disk file
   virtual bool ReadSummary(void *data);
   /// Read the data section of the disk file
   virtual int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len);

   /// Create a new block file identical to this one
   virtual BlockFile *Copy(wxFileName newFileName);
   /// Write an XML representation of this file
   virtual void SaveXML(int depth, wxFFile &xmlFile);
   virtual int GetSpaceUsage();

   static BlockFile *BuildFromXML(wxString dir, const char **attrs);
};

#endif

