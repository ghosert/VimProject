/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVSTMac.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../../Audacity.h"

#if USE_VST

#define __MOVIES__   /* Apple's Movies.h not compatible with Audacity */
/* #define __MACHELP__ */

#include <wx/mac/private.h>

#ifdef __MACOS9__
#include <Resources.h>
#include <Files.h>
#include <Memory.h>
#include <CodeFragments.h>
#endif

#ifdef __MACOSX__
#include <CoreServices/CoreServices.h>
#endif

#include "AEffect.h"
#include "AudioEffect.hpp"        // VST API

#include "VSTEffect.h"
#include "LoadVSTMac.h"   
#include "../../AudacityApp.h"
#include "../../Internat.h"

int audacityVSTID = 1;

extern "C" {
   
   long audioMaster(AEffect * effect, long opcode, long index,
                    long value, void *ptr, float opt)
   {
      switch (opcode) {
      case audioMasterVersion:
         return 2;
      case audioMasterCurrentId:
         return audacityVSTID;
      default:
         return 0;
      }
   }

   typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

   void LoadVSTPlugins()
   {
#ifdef __MACOSX__
      audioMasterCallback audioMasterFPtr =
         (audioMasterCallback)NewCFMFromMachO(audioMaster);
#else
      // What is the corrct way of creating an audioMasterCallback
      // in OS 9/Carbon???
      // audioMasterCallback audioMasterFPtr = NULL; 
      audioMasterCallback audioMasterFPtr = audioMaster;
#endif      

      wxArrayString audacityPathList = wxGetApp().audacityPathList;
      wxArrayString pathList;
      wxArrayString files;
      unsigned int i;
      
      for(i=0; i<audacityPathList.GetCount(); i++) {
         wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
         wxGetApp().AddUniquePathToPathList(prefix + "VST",
                                            pathList);
         wxGetApp().AddUniquePathToPathList(prefix + "Plugins",
                                            pathList);
         wxGetApp().AddUniquePathToPathList(prefix + "Plug-Ins",
                                            pathList);
      }

      #ifdef __MACOSX__
      wxGetApp().AddUniquePathToPathList("/Library/Audio/Plug-Ins/VST",
                              pathList);
      wxString vstPath;
      vstPath.Printf("/Users/%s/Library/Audio/Plug-Ins/VST",
                     wxGetenv("USER"));
      wxGetApp().AddUniquePathToPathList(vstPath,
                                         pathList);
      #endif

      wxGetApp().FindFilesInPathList("*", pathList, wxFILE, files);
      
      for(i=0; i<files.GetCount(); i++) {
         short   resFileID;
         FSSpec  spec;
         
         wxMacFilename2FSSpec(FILENAME(files[i]), &spec);
         resFileID = FSpOpenResFile(&spec, fsRdPerm);
         short cResCB = Count1Resources('aEff');

         for (int i = 0; i < cResCB; i++) {
            Handle             codeH;
            CFragConnectionID  connID;
            Ptr                mainAddr;
            Str255             errName;
            Str255             fragName;
            char               fragNameCStr[256];
            short              resID;
            OSType             resType;
            OSErr              err;

            codeH = Get1IndResource('aEff', short(i+1));
            if (!codeH)
               continue;

            GetResInfo(codeH, &resID, &resType, fragName);
            DetachResource(codeH);
            HLock(codeH);

            err = GetMemFragment(*codeH,
                                 GetHandleSize(codeH),
                                 fragName,
                                 kPrivateCFragCopy,
                                 &connID, (Ptr *) & mainAddr, errName);

            if (!err) {
               vstPluginMain   pluginMain;
               AEffect        *theEffect;

               #ifdef __MACOSX__
               pluginMain = (vstPluginMain)NewMachOFromCFM(mainAddr);
               #else
               pluginMain = (vstPluginMain)mainAddr;
               #endif

               theEffect = pluginMain(audioMasterFPtr);

               if (theEffect->magic == kEffectMagic) {
                  
                  memcpy(fragNameCStr, &fragName[1], fragName[0]);
                  fragNameCStr[fragName[0]] = 0;
                  
                  VSTEffect *vst =
                     new VSTEffect(wxString(fragNameCStr), theEffect);
                  Effect::RegisterEffect(vst);
               }

               #ifdef __MACOSX__
               DisposeMachOFromCFM(pluginMain);
               #endif
               
               audacityVSTID++;
            }
         }
         
         CloseResFile(resFileID);

      }
         
#ifdef __MACOSX__
      DisposeCFMFromMachO(audioMasterFPtr);
#endif  //   __MACOSX__
   }

};                              // extern "C"

#else

extern "C" {
   void LoadVSTPlugins() {}
}

#endif // USE_VST

