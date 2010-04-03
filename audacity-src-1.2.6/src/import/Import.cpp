/**********************************************************************

  Audacity: A Digital Audio Editor

  Import.cpp

  Dominic Mazzoni

  This file contains a general function which will import almost
  any type of sampled audio file (i.e. anything except MIDI)
  and return the tracks that were imported.  This function just
  figures out which one to call; the actual importers are in
  ImportPCM, ImportMP3, ImportOGG, ImportRawData, and ImportLOF.

**********************************************************************/

#include <wx/textctrl.h>
#include <wx/msgdlg.h>
#include <wx/string.h>
#include <wx/intl.h>
#include <wx/listimpl.cpp>

#include "../Audacity.h"

#include "Import.h"
#include "ImportPlugin.h"
#include "ImportPCM.h"
#include "ImportMP3.h"
#include "ImportOGG.h"
#include "ImportRaw.h"
#include "ImportLOF.h"
#include "../Track.h"

WX_DEFINE_LIST(ImportPluginList);
WX_DEFINE_LIST(UnusableImportPluginList);

Importer::Importer()
{
   mImportPluginList = new ImportPluginList;
   mUnusableImportPluginList = new UnusableImportPluginList;

   // build the list of import plugin and/or unusableImporters.
   // order is significant.  If none match, they will all be tried
   // in the order defined here.
   GetPCMImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetOGGImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetMP3ImportPlugin(mImportPluginList, mUnusableImportPluginList);
   GetLOFImportPlugin(mImportPluginList, mUnusableImportPluginList);
}

Importer::~Importer()
{
   mImportPluginList->DeleteContents(true);
   delete mImportPluginList;
   delete mUnusableImportPluginList;
}

void Importer::GetSupportedImportFormats(FormatList *formatList)
{
   ImportPluginList::Node *importPluginNode = mImportPluginList->GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *importPlugin = importPluginNode->GetData();
      formatList->Append(new Format(importPlugin->GetPluginFormatDescription(),
                                    importPlugin->GetSupportedExtensions()));
      importPluginNode = importPluginNode->GetNext();
   }
}

// returns number of tracks imported
int Importer::Import(wxString fName,
                     TrackFactory *trackFactory,
                     Track *** tracks,
                     wxString &errorMessage,
                     progress_callback_t progressCallback,
                     void *userData)
{
   int numTracks = 0;

   wxString extension = fName.AfterLast('.');
   // create a string with the file name extension in it

   // see if any of the plugins expect this extension and if so give
   // that plugin first dibs
   ImportPluginList::Node *importPluginNode = mImportPluginList->GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *plugin = importPluginNode->GetData();
      if( plugin->SupportsExtension(extension) )
      {
         mInFile = plugin->Open(fName);
         if( mInFile != NULL )
         {
            mInFile->SetProgressCallback(progressCallback, userData);
            if( mInFile->Import(trackFactory, tracks, &numTracks) == true )
            {
               // LOF ("list-of-files") has different semantics
               if (extension.IsSameAs("lof", false))
                  return 1;

               if (numTracks > 0) {
                  // success!
                  delete mInFile;
                  return numTracks;
               }
            }
            delete mInFile;
         }
      }
      importPluginNode = importPluginNode->GetNext();
   }

   // no importPlugin that recognized the extension succeeded.  However, the
   // file might be misnamed.  So this time we try all the importPlugins
   // in order and see if any of them can handle the file
   importPluginNode = mImportPluginList->GetFirst();
   while(importPluginNode)
   {
      ImportPlugin *plugin = importPluginNode->GetData();
      mInFile = plugin->Open(fName);
      if( mInFile != NULL )
      {
         mInFile->SetProgressCallback(progressCallback, userData);
         numTracks = 0;
         if( mInFile->Import(trackFactory, tracks, &numTracks) == true )
         {
            if (numTracks > 0) {
               // success!
               delete mInFile;
               return numTracks;
            }
         }
         delete mInFile;

         // This will happen if the user cancelled, or if we
         // tried and got an error partially through.  Either way,
         // no need to try any other formats at this point!
         if (numTracks > 0)
            return 0;
      }
      importPluginNode = importPluginNode->GetNext();
   }

   // None of our plugins can handle this file.  It might be that
   // Audacity supports this format, but support was not compiled in.
   // If so, notify the user of this fact
   UnusableImportPluginList::Node *unusableImporterNode
      = mUnusableImportPluginList->GetFirst();
   while(unusableImporterNode)
   {
      UnusableImportPlugin *unusableImportPlugin = unusableImporterNode->GetData();
      if( unusableImportPlugin->SupportsExtension(extension) )
      {
         errorMessage.Printf(_("This version of Audacity was not "
                               "compiled with %s support."),
                             (const char *)
                             unusableImportPlugin->
                             GetPluginFormatDescription());
         return 0;
      }
      unusableImporterNode = unusableImporterNode->GetNext();
   }
   /* warnings for unsupported data types */
   // if someone has sent us a .cda file, send them away
   if (extension.IsSameAs("cda", false)) {
      errorMessage = "\"" + fName + "\"" + 
         _(" is an audio CD file. \n"
            "Audacity does not open this type of file.\n"
            "Try ripping it to a native audio format that Audacity can import.");
      return 0;
   }
   // playlist type files
   if ((extension.IsSameAs(wxT("m3u"), false))||(extension.IsSameAs(wxT("ram"), false))||(extension.IsSameAs(wxT("pls"), false))) {
      errorMessage = wxT("\"") + fName + wxT("\"") + 
         _(" is a playlist file.\nAudacity cannot open this file because it only contains links to other files.\nYou may be able to open it in a text editor and download the actual audio files.");
      return 0;
   }
   //WMA files of various forms
   if ((extension.IsSameAs(wxT("wmv"), false))||(extension.IsSameAs(wxT("wma"), false))||(extension.IsSameAs(wxT("asf"), false))) {
      errorMessage = wxT("\"") + fName + wxT("\"") + 
         _(" is an Windows Media Audio file. \nAudacity cannot open this type of file to due to patent restrictions.\nYou need to convert it to a supported audio format.");
      return 0;
   }
   //AAC files of various forms (probably not encrypted)
   if ((extension.IsSameAs(wxT("aac"), false))||(extension.IsSameAs(wxT("m4a"), false))||(extension.IsSameAs(wxT("mpa"), false))||(extension.IsSameAs(wxT("mp4"), false))) {
      errorMessage = wxT("\"") + fName + wxT("\"") + 
         _(" is an Advanced Audio Coding file. \nAudacity cannot open this type of file.\nYou need to convert it to a supported audio format.");
      return 0;
   }
   // encrypted itunes files
   if ((extension.IsSameAs(wxT("m4p"), false))) {
      errorMessage = wxT("\"") + fName + wxT("\"") + 
         _(" is an encrypted audio file, typically from an online music store. \nAudacity cannot open this type of file due to the encryption.\nYou need to convert it to a supported, unencrypted audio format.");
      return 0;
   }
   // Real files of various sorts
   if ((extension.IsSameAs(wxT("ra"), false))||(extension.IsSameAs(wxT("rm"), false))||(extension.IsSameAs(wxT("rpm"), false))||(extension.IsSameAs(wxT("rv"), false))) {
      errorMessage = wxT("\"") + fName + wxT("\"") + 
         _(" is a RealPlayer media file. \nAudacity cannot open this proprietary format.\nYou need to convert it to a supported audio format.");
      return 0;
   }
		
   // we were not able to recognize the file type
   errorMessage = _("Audacity did not recognize the type "
                    "of this file.\n"
                    "If it is uncompressed, try importing it "
                    "using \"Import Raw\"" );
   return 0;
}

wxString Importer::GetFileDescription()
{
   return mInFile->GetFileDescription();
}


