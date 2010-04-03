/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyAliasBlockFile.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LEGACYALIASBLOCKFILE__
#define __AUDACITY_LEGACYALIASBLOCKFILE__

#include "../BlockFile.h"
#include "PCMAliasBlockFile.h"

/// An AliasBlockFile that references uncompressed data in an existing file
class LegacyAliasBlockFile : public PCMAliasBlockFile
{
 public:

   // Constructor / Destructor

   /// Constructs a LegacyAliasBlockFile, writing the summary to disk
   LegacyAliasBlockFile(wxFileName fileName,
                        wxFileName aliasedFile,
                        sampleCount aliasStart,
                        sampleCount aliasLen,
                        int aliasChannel,
                        sampleCount summaryLen,
                        bool noRMS);
   virtual ~LegacyAliasBlockFile();

   virtual void SaveXML(int depth, wxFFile &xmlFile);
   virtual BlockFile *Copy(wxFileName fileName);

   static BlockFile *BuildFromXML(wxString projDir, const char **attrs);
};

#endif

