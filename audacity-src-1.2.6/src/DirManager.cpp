/**********************************************************************

  Audacity: A Digital Audio Editor

  DirManager.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/log.h>
#include <wx/filefn.h>
#include <wx/hash.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/timer.h>
#include <wx/intl.h>
#include <wx/file.h>
#include <wx/filename.h>
#include <wx/object.h>

// chmod
#ifdef __UNIX__
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include "Audacity.h"
#include "AudacityApp.h"
#include "BlockFile.h"
#include "blockfile/LegacyBlockFile.h"
#include "blockfile/LegacyAliasBlockFile.h"
#include "blockfile/SimpleBlockFile.h"
#include "blockfile/SilentBlockFile.h"
#include "blockfile/PCMAliasBlockFile.h"
#include "DirManager.h"
#include "Internat.h"
#include "Prefs.h"
#include "widgets/Warning.h"

#include "prefs/PrefsDialog.h"

// Static class variables

int DirManager::numDirManagers = 0;
int DirManager::fileIndex = 0;
bool DirManager::dontDeleteTempFiles = false;

unsigned int DirManager::defaultHashTableSize = 10000;

wxString DirManager::temp;

// Methods

DirManager::DirManager()
{
   wxLogDebug("DirManager: Created new instance");

   mRef = 1; // MM: Initial refcount is 1 by convention

   numDirManagers++;

   projPath = "";
   projName = "";

   mLoadingTarget = NULL;

   hashTableSize = defaultHashTableSize;
   blockFileHash = new wxHashTable(wxKEY_STRING, hashTableSize);

   // Make sure there is plenty of space for temp files

   //BG: wxWindows 2.3.2 and higher claim to support this,
   //through a function called wxGetDiskSpace

   wxLongLong freeSpace;
   wxGetDiskSpace(temp, NULL, &freeSpace);
   if (freeSpace >= 0) {
      if (freeSpace < 1048576) {
         ShowWarningDialog(NULL, "DiskSpaceWarning",
              _("Warning: there is very little free disk space left on this "
                "volume. Please select another temporary directory in your "
                "preferences."));
      }
   }
}

DirManager::~DirManager()
{
   wxASSERT(mRef == 0); // MM: Otherwise, we shouldn't delete it

   if (blockFileHash)
      delete blockFileHash;

   numDirManagers--;
   if (numDirManagers == 0) {
      CleanTempDir(false);
      //::wxRmdir(temp);
   }
}

// static
void DirManager::CleanTempDir(bool startup)
{
   wxString fname;
   wxStringList fnameList;
   int count = 0;

   if (dontDeleteTempFiles)
      return;

   // XXX: is this too destructive, to delete everything?
   fname = wxFindFirstFile((const char *) (temp + wxFILE_SEP_PATH + "b*"));
   while (fname != "") {
      count++;
      fnameList.Add(fname);
      fname = wxFindNextFile();
   }

   if (count == 0)
      return;

   if (startup) {
      wxString prompt =
         _("Audacity found temporary files that were not deleted or saved\n"
           "the last time you used Audacity.\n\n"
           "Audacity can't recover them automatically, but if you choose not\n"
           "to delete them, you can recover them manually.\n\n"
           "Delete temporary files?");
      
      int action = wxMessageBox(prompt,
                                "Warning",
                                wxYES_NO | wxICON_EXCLAMATION,
                                NULL);
      
      if (action != wxYES) {
         dontDeleteTempFiles = true;
         return;
      }
   }

   wxChar **array = fnameList.ListToArray();

   wxProgressDialog *progress = NULL;

   //wxYield();
   wxStartTimer();

   for (int i = 0; i < count; i++) {
      wxString fileName = array[i];
      wxRemoveFile(FILENAME(fileName));

      if (!progress && wxGetElapsedTime(false) > 500)
         progress =
             new wxProgressDialog(_("Progress"),
                                  _("Cleaning up temporary files..."),
                                  1000,
                                  NULL,
                                  wxPD_REMAINING_TIME | wxPD_AUTO_HIDE);

      if (progress)
         progress->Update(int ((i * 1000.0) / count));
   }

   if (progress)
      delete progress;

   delete [] array;
}

bool DirManager::SetProject(wxString & projPath, wxString & projName,
                            bool create)
{
   wxString oldPath = this->projPath;
   wxString oldName = this->projName;
   wxString oldFull = this->projFull;
   wxString oldLoc = this->projFull;
   if (oldLoc == "")
      oldLoc = temp;
   
   if (projPath == "")
      projPath = FROMFILENAME(::wxGetCwd());

   this->projPath = projPath;
   this->projName = projName;
   this->projFull = projPath + wxFILE_SEP_PATH + projName;

   if (create) {
      if (!wxPathExists(FILENAME(projFull)))
         if (!wxMkdir(FILENAME(projFull)))
            return false;

      #ifdef __UNIX__
      chmod(FILENAME(projFull), 0775);
      #endif

   } else {
      #ifndef __WXMAC__
      if (!wxPathExists(FILENAME(projFull)))
         return false;
      #endif
   }

   /* Move all files into this new directory.  Files which are
      "locked" get copied instead of moved.  (This happens when
      we perform a Save As - the files which belonged to the last
      saved version of the old project must not be moved,
      otherwise the old project would not be safe. */

   blockFileHash->BeginFind();
   wxNode *n = blockFileHash->Next();
   bool success = true;
   while(n && success) {
      BlockFile *b = (BlockFile *)n->GetData();

      if (b->IsLocked())
         success = CopyToNewProjectDirectory(b);
      else
         success = MoveToNewProjectDirectory(b);

      n = blockFileHash->Next();
   }

   if (!success) {
      // If the move failed, we try to move/copy as many files
      // back as possible so that no damage was done.  (No sense
      // in checking for errors this time around - there's nothing
      // that could be done about it.)
      // Likely causes: directory was not writeable, disk was full

      projFull = oldLoc;

      blockFileHash->BeginFind();
      wxNode *n = blockFileHash->Next();
      while(n) {
         BlockFile *b = (BlockFile *)n->GetData();
         MoveToNewProjectDirectory(b);         
         n = blockFileHash->Next();
      }

      projFull = oldFull;
      projPath = oldPath;
      projName = oldName;

      return false;
   }

   return true;
}

wxString DirManager::GetProjectName()
{
   return projName;
}

wxLongLong DirManager::GetFreeDiskSpace()
{
   wxLongLong freeSpace = -1;
   wxString path = projPath;

   if (projPath == "")
      path = temp;

   if (!wxGetDiskSpace(FILENAME(path), NULL, &freeSpace))
      freeSpace = -1;

   return freeSpace;
}

wxFileName DirManager::MakeBlockFileName(wxString projDir)
{
   wxString baseFileName;

   do {
      baseFileName.Printf("b%05d", fileIndex++);
   } while ( blockFileHash->Get(baseFileName) );

   return wxFileName(projDir, baseFileName);
}

BlockFile *DirManager::NewSimpleBlockFile(
                                 samplePtr sampleData, sampleCount sampleLen,
                                 sampleFormat format)
{
   wxString loc = (projFull != ""? projFull: temp);
   wxFileName fileName = MakeBlockFileName(loc);

   BlockFile *newBlockFile =
       new SimpleBlockFile(fileName, sampleData, sampleLen, format);

   blockFileHash->Put(fileName.GetName(), (wxObject *) newBlockFile);

   CheckHashTableSize();

   return newBlockFile;
}

BlockFile *DirManager::NewAliasBlockFile(
                                 wxString aliasedFile, sampleCount aliasStart,
                                 sampleCount aliasLen, int aliasChannel)
{
   wxString loc = (projFull != ""? projFull: temp);
   wxFileName fileName = MakeBlockFileName(loc);

   BlockFile *newBlockFile =
       new PCMAliasBlockFile(fileName,
                             aliasedFile, aliasStart, aliasLen, aliasChannel);

   blockFileHash->Put(fileName.GetName(), (wxObject *) newBlockFile);
   aliasList.Add(aliasedFile);

   CheckHashTableSize();

   return newBlockFile;
}

// Adds one to the reference count of the block file,
// UNLESS it is "locked", then it makes a new copy of
// the BlockFile.
BlockFile *DirManager::CopyBlockFile(BlockFile *b)
{
   if (!b->IsLocked()) {
      b->Ref();
      return b;
   }

   wxString dir = (projFull != ""? projFull: temp);
   wxFileName newFile = MakeBlockFileName(dir);

   // We assume that the new file should have the same extension
   // as the existing file
   newFile.SetExt(b->GetFileName().GetExt());

   if( !wxCopyFile(FILENAME(b->GetFileName().GetFullPath()),
                   FILENAME(newFile.GetFullPath())) )
      return NULL;

   BlockFile *b2 = b->Copy(newFile);

   if (b2 == NULL)
      return NULL;

   blockFileHash->Put(newFile.GetName(), (wxObject *) b2);
   aliasList.Add(newFile.GetFullPath());

   CheckHashTableSize();

   return b2;
}

bool DirManager::HandleXMLTag(const char *tag, const char **attrs)
{
   if( mLoadingTarget == NULL )
      return false;

   if( !wxStricmp(tag, "silentblockfile") ) {
      // Silent blocks don't actually have a file associated, so
      // we don't need to worry about the hash table at all
      *mLoadingTarget = SilentBlockFile::BuildFromXML(projFull, attrs);
      return true;
   }

   else if ( !wxStricmp(tag, "simpleblockfile") )
      *mLoadingTarget = SimpleBlockFile::BuildFromXML(projFull, attrs);
   else if( !wxStricmp(tag, "pcmaliasblockfile") )
      *mLoadingTarget = PCMAliasBlockFile::BuildFromXML(projFull, attrs);
   else if( !wxStricmp(tag, "blockfile") ||
            !wxStricmp(tag, "legacyblockfile") ) {
      // Support Audacity version 1.1.1 project files

      int i=0;
      bool alias = false;

      while(attrs[i]) {
         if (!wxStricmp(attrs[i], "alias")) {
            if (atoi(attrs[i+1])==1)
               alias = true;
         }
         i++;
         if (attrs[i])
            i++;
      }

      if (alias)
         *mLoadingTarget = LegacyAliasBlockFile::BuildFromXML(projFull, attrs);
      else      
         *mLoadingTarget = LegacyBlockFile::BuildFromXML(projFull, attrs,
                                                         mLoadingBlockLen,
                                                         mLoadingFormat);
   }
   else
      return false;

   //
   // If the block we loaded is already in the hash table, then the
   // object we just loaded is a duplicate, so we delete it and
   // return a reference to the existing object instead.
   //

   wxString name = (*mLoadingTarget)->GetFileName().GetName();    
   BlockFile *retrieved = (BlockFile *) blockFileHash->Get(name);
   if (retrieved) {
      // Lock it in order to delete it safely, i.e. without having
      // it delete the file, too...
      (*mLoadingTarget)->Lock();
      delete (*mLoadingTarget);

      Ref(retrieved); // Add one to its reference count
      *mLoadingTarget = retrieved;
      return true;
   }

   // This is a new object
   blockFileHash->Put( name, (wxObject*) *mLoadingTarget );
   CheckHashTableSize();

   return true;
}

#if LEGACY_PROJECT_FILE_SUPPORT
void DirManager::SaveBlockFile(BlockFile * f, wxTextFile * out)
{
   out->AddLine(wxString::Format("%d", f->mSummaryLen));
   if (f->IsAlias()) {
      out->AddLine("Alias");
      out->AddLine(f->mAliasFullPath);
      out->AddLine(wxString::Format("%d", f->mStart));
      out->AddLine(wxString::Format("%d", f->mLen));
      out->AddLine(wxString::Format("%d", f->mChannel));
   }
   out->AddLine(f->mName);
}

BlockFile *DirManager::LoadBlockFile(wxTextFile * in, sampleFormat format)
{
   wxASSERT(projFull != "");

   long summaryLen;

   if (!(in->GetNextLine().ToLong(&summaryLen)))
      return NULL;

   wxString blockName = in->GetNextLine();

   bool alias = false;
   wxString aliasFullPath;
   long localLen, start, len, channel;

   if (blockName == "Alias") {
      alias = true;
      aliasFullPath = in->GetNextLine();
      
      //if (!(in->GetNextLine().ToLong(&localLen)))
      //   return NULL;

      if (!(in->GetNextLine().ToLong(&start)))
         return NULL;
      if (!(in->GetNextLine().ToLong(&len)))
         return NULL;
      if (!(in->GetNextLine().ToLong(&channel)))
         return NULL;

      blockName = in->GetNextLine();
   }

   wxString pathName = projFull + wxFILE_SEP_PATH + blockName;
   BlockFile *retrieved = (BlockFile *) blockFileHash->Get(blockName);
   if (retrieved) {
      wxASSERT(retrieved->IsAlias() == alias);
      retrieved->Ref();
      return retrieved;
   } else {
      BlockFile *newBlockFile =
         new BlockFile(blockName, pathName, summaryLen);

      if (alias) {
         newBlockFile->SetAliasedData(aliasFullPath, start, len, channel);
         aliasList.Add(aliasFullPath);
      }

      newBlockFile->mSampleFormat = format;

      blockFileHash->Put(blockName, (wxObject *) newBlockFile);

      CheckHashTableSize();

      if (!wxFileExists(FILENAME(pathName)))
         return 0;
      return newBlockFile;
   }
}
#endif // if LEGACY_PROJECT_FILE_SUPPORT

bool DirManager::MoveToNewProjectDirectory(BlockFile *f)
{
   wxFileName newFileName(projFull, f->mFileName.GetFullName());

   if ( !(newFileName == f->mFileName) ) {
      bool ok = wxRenameFile(FILENAME(f->mFileName.GetFullPath()),
                             FILENAME(newFileName.GetFullPath()));

      if (ok)
         f->mFileName = newFileName;
      else {
         ok = wxCopyFile(FILENAME(f->mFileName.GetFullPath()),
                         FILENAME(newFileName.GetFullPath()));
         if (ok) {
            wxRemoveFile(FILENAME(f->mFileName.GetFullPath()));
            f->mFileName = newFileName;
         }
         else
            return false;
      }
   }

   return true;
}

bool DirManager::CopyToNewProjectDirectory(BlockFile *f)
{
   wxFileName newFileName(projFull, f->mFileName.GetFullName());
   if ( !(newFileName == f->mFileName) ) {
      bool ok = wxCopyFile(FILENAME(f->mFileName.GetFullPath()),
                           FILENAME(newFileName.GetFullPath()));
      if (ok) {
         f->mFileName = newFileName;
      }
      else
         return false;
   }

   return true;
}

void DirManager::Ref(BlockFile * f)
{
   f->Ref();
   //  printf("Ref(%d): %s\n",f->refCount, (const char *)(f->fullPath));
}

int DirManager::GetRefCount(BlockFile * f)
{
   return f->mRefCount;
}

void DirManager::Deref(BlockFile * f)
{
   wxString theFileName = f->GetFileName().GetName();

   //  printf("Deref(%d): %s\n",f->mRefCount-1, (const char *)f->mRullPath);

   if (f->Deref()) {
      // If Deref() returned true, the reference count reached zero
      // and this block is no longer needed.  Remove it from the hash
      // table.

      blockFileHash->Delete(theFileName);
   }
}

void DirManager::CheckHashTableSize()
{
   // This method makes sure that our hash table doesn't fill up.
   // When it's about halfway full (i.e. starting to exceed its
   // capacity), we create a new hash table with double the size,
   // and copy everything over.

   if (blockFileHash->GetCount() >= hashTableSize/2) {
      wxBusyCursor busy;
      hashTableSize *= 2;

      wxHashTable *newHash = new wxHashTable(wxKEY_STRING, hashTableSize);
      blockFileHash->BeginFind();
      wxNode *n = blockFileHash->Next();
      while(n) {
         BlockFile *b = (BlockFile *)n->GetData();
         newHash->Put(b->mFileName.GetName(), (wxObject *) b);
         n = blockFileHash->Next();
      }

      delete blockFileHash;
      blockFileHash = newHash;
   }
}

bool DirManager::EnsureSafeFilename(wxFileName fName)
{
   // Quick check: If it's not even in our alias list,
   // then the file name is A-OK.

   #if 0
   printf("file name: %s\n", fName.GetFullPath().c_str());
   printf("string list:\n");
   wxStringListNode *node = aliasList.GetFirst();
   while (node)
   {
      wxString string = node->GetData();
      printf("%s\n", string.c_str());
      node = node->GetNext();
   }
   #endif

   if (!aliasList.Member(fName.GetFullPath()))
      return true;

   // If any of the following commands fail, your guess is as
   // good as mine why.  The following error message is the
   // best we can do - we'll use it if any of the renames,
   // creates, or deletes fail.
   wxString errStr =
     _( "Error: is directory write-protected or disk full?" );

   /* i18n-hint: 'old' is part of a filename used when a file is renamed. */
   // Figure out what the new name for the existing file would be.  
   /* i18n-hint: e.g. Try to go from "mysong.wav" to "mysong-old1.wav". */
   // Keep trying until we find a filename that doesn't exist.

   wxFileName renamedFile = fName;
   int i = 0;
   do {
      i++;
      /* i18n-hint: This is the pattern for filenames that are created
         when a file needs to be backed up to a different name.  For
         example, mysong would become mysong-old1, mysong-old2, etc. */
      renamedFile.SetName(wxString::Format(_("%s-old%d"), fName.GetName().c_str(), i));
   } while (wxFileExists(FILENAME(renamedFile.GetFullPath())));

   // Test creating a file by that name to make sure it will
   // be possible to do the rename

   wxFile testFile(FILENAME(renamedFile.GetFullPath()), wxFile::write);
   if (!testFile.IsOpened()) {
      wxMessageBox(errStr);
      return false;
   }
   if (!wxRemoveFile(FILENAME(renamedFile.GetFullPath()))) {
      wxMessageBox(errStr);
      return false;
   }

   printf(_("Renamed file: %s\n"), (const char *)renamedFile.GetFullPath());

   // Go through our block files and see if any indeed point to
   // the file we're concerned about.  If so, point the block file
   // to the renamed file and when we're done, perform the rename.

   bool needToRename = false;
   wxBusyCursor busy;
   blockFileHash->BeginFind();
   wxNode *n = blockFileHash->Next();
   while(n) {
      BlockFile *b = (BlockFile *)n->GetData();
      // don't worry, we don't rely on this cast unless IsAlias is true
      AliasBlockFile *ab = (AliasBlockFile*)b;

      if (b->IsAlias() && ab->GetAliasedFile() == fName) {
         needToRename = true;
         printf(_("Changing block %s\n"), (const char *)b->GetFileName().GetFullName());
         ab->ChangeAliasedFile(renamedFile);
      }

      n = blockFileHash->Next();
   }

   if (needToRename) {
      if (!wxRenameFile(FILENAME(fName.GetFullPath()),
                        FILENAME(renamedFile.GetFullPath()))) {
         // ACK!!! The renaming was unsuccessful!!!
         // (This shouldn't happen, since we tried creating a
         // file of this name and then deleted it just a
         // second earlier.)  But we'll handle this scenario
         // just in case!!!

         // Put things back where they were
         blockFileHash->BeginFind();
         n = blockFileHash->Next();
         while(n) {
            BlockFile *b = (BlockFile *)n->GetData();
            AliasBlockFile *ab = (AliasBlockFile*)b;

            if (b->IsAlias() && ab->GetAliasedFile() == renamedFile)
               ab->ChangeAliasedFile(fName);
            n = blockFileHash->Next();
         }

         // Print error message and cancel the export
         wxMessageBox(errStr);
         return false;
      }

      aliasList.Delete(fName.GetFullPath());
      aliasList.Add(renamedFile.GetFullPath());
   }

   // Success!!!  Either we successfully renamed the file,
   // or we didn't need to!
   return true;
}

void DirManager::Ref()
{
   wxASSERT(mRef > 0); // MM: If mRef is smaller, it should have been deleted already
   ++mRef;
}

void DirManager::Deref()
{
   wxASSERT(mRef > 0); // MM: If mRef is smaller, it should have been deleted already
   
   --mRef;

   // MM: Automatically delete if refcount reaches zero
   if (mRef == 0)
   {
//      wxLogDebug("DirManager::Deref: Automatically deleting 'this'");
      delete this;
   }
}
