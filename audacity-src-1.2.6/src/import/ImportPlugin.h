/**********************************************************************

  Audacity: A Digital Audio Editor

  Importer.h

  Joshua Haberman

  The interface that all file import "plugins" (if you want to call
  them that) must implement.

  Since this is part of libaudacity, it must not use any GUI parts
  of wxWindows.

**********************************************************************/

#ifndef __AUDACITY_IMPORTER__
#define __AUDACITY_IMPORTER__

#include <wx/string.h>
#include <wx/list.h>

class TrackFactory;
class Track;

typedef bool (*progress_callback_t)( void *userData, float percent );

class ImportFileHandle;

class ImportPlugin
{
public:

   // Get a description of the file type this importer can import.
   // Examples: "Ogg Vorbis", "MP3", "Uncompressed PCM"
   virtual wxString GetPluginFormatDescription() = 0;

   // Get a list of extensions this plugin expects to be able to
   // import.  If a filename matches any of these extensions,
   // this importer will get first dibs on importing it.
   virtual wxStringList GetSupportedExtensions()
   {
      return mExtensions;
   }

   bool SupportsExtension(wxString extension)
   {
      return mExtensions.Member(extension);
   }

   // Open the given file, returning true if it is in a recognized
   // format, false otherwise.  This puts the importer into the open
   // state.
   virtual ImportFileHandle *Open(wxString Filename) = 0;

   virtual ~ImportPlugin() { }

protected:

   ImportPlugin(wxStringList supportedExtensions):
      mExtensions(supportedExtensions)
   {
   }

   wxStringList mExtensions;
};


class ImportFileHandle
{
public:
   virtual void SetProgressCallback(progress_callback_t function,
                                    void *userData) = 0;

   // This is similar to GetImporterDescription, but if possible the
   // importer will return a more specific description of the
   // specific file that is open.
   virtual wxString GetFileDescription() = 0;

   // Return an estimate of how many bytes the file will occupy once
   // imported
   virtual int GetFileUncompressedBytes() = 0;

   // do the actual import, creating whatever tracks are necessary with
   // the TrackFactory and calling the progress callback every iteration
   // through the importing loop
   virtual bool Import(TrackFactory *trackFactory, Track ***outTracks,
                       int *outNumTracks) = 0;

   virtual ~ImportFileHandle() { }
};



class UnusableImportPlugin
{
public:
   UnusableImportPlugin(wxString formatName, wxStringList extensions):
      mFormatName(formatName),
      mExtensions(extensions)
   {
   }

   wxString GetPluginFormatDescription()
   {
      return mFormatName;
   }

   bool SupportsExtension(wxString extension)
   {
      return mExtensions.Member(extension);
   }

private:
   wxString mFormatName;
   wxStringList mExtensions;
};

WX_DECLARE_LIST(ImportPlugin, ImportPluginList);
WX_DECLARE_LIST(UnusableImportPlugin, UnusableImportPluginList);

#endif

