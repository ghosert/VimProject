/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.h

  Dominic Mazzoni

  This file contains a general function which will import almost
  any type of sampled audio file (i.e. anything except MIDI)
  and return the tracks that were imported.  This function just
  figures out which one to call; the actual importers are in
  ImportPCM, ImportMP3, ImportOGG, ImportRawData, and ImportLOF.

**********************************************************************/

#ifndef _IMPORT_
#define _IMPORT_

#include <wx/string.h>
#include <wx/list.h>
#include <wx/listimpl.cpp>

class TrackFactory;
class Track;
class ImportPlugin;
class ImportFileHandle;
class UnusableImportPlugin;
typedef bool (*progress_callback_t)( void *userData, float percent );

class Format {
public:
   wxString formatName;
   wxStringList formatExtensions;

   Format(wxString _formatName, wxStringList _formatExtensions):
      formatName(_formatName),
      formatExtensions(_formatExtensions)
   {
   }
};

class ImportPluginList;
class UnusableImportPluginList;

WX_DECLARE_LIST(Format, FormatList);


class Importer {
public:
   Importer();
   ~Importer();

   void GetSupportedImportFormats(FormatList *formatList);

   // returns number of tracks imported
   // if zero, the import failed and errorMessage will be set.
   int Import(wxString fName,
              TrackFactory *trackFactory,
              Track *** tracks,
              wxString &errorMessage,
              progress_callback_t progressCallback,
              void *userData);

   // get a possibly more detailed description of the kind of file
   // that is being opened.  ONLY callable from INSIDE THE CALLBACK.
   wxString GetFileDescription();

private:
   ImportPluginList *mImportPluginList;
   UnusableImportPluginList *mUnusableImportPluginList;
   ImportFileHandle *mInFile;
};

#endif

