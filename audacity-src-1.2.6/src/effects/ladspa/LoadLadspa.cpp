/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadLadspa.cpp

  Dominic Mazzoni

  From the ladspa docs:
  "To allow multiple hosts to
   share plugin types, hosts may wish to check for environment
   variable LADSPA_PATH. If present, this should contain a
   colon-separated path indicating directories that should be searched
   (in order) when loading plugin types."

**********************************************************************/

#define descriptorFnName "ladspa_descriptor"

// For Mac and Linux, use dlopen
#if defined(__WXMAC__) || defined(__WXGTK__)
#include <dlfcn.h>
// For Windows, use the wxWindows Dynamic Library Loader
#else
#include <wx/dynlib.h>
#endif

#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "ladspa.h"

#include "../../Audacity.h"
#include "../../AudacityApp.h"
#include "../../Internat.h"
#include "LadspaEffect.h"

void LoadLadspaEffect(wxString fname)
{
   wxLogNull logNo;
   LADSPA_Descriptor_Function mainFn = NULL;

   // As a courtesy to some plug-ins that might be bridges to
   // open other plug-ins, we set the current working
   // directory to be the plug-in's directory.

   wxString saveOldCWD = FROMFILENAME(::wxGetCwd());
   wxString prefix = ::wxPathOnly(fname);
   ::wxSetWorkingDirectory(FILENAME(prefix));

#if defined(__WXGTK__) || defined(__WXMAC__)

   void *libHandle = NULL;

   libHandle = dlopen(FILENAME(fname), RTLD_LAZY);
   
   mainFn = (LADSPA_Descriptor_Function)
      dlsym(libHandle, descriptorFnName);

#else
   // The following code uses the wxWindows DLL class, which does
   // not allow us to control the flags passed to dlopen().  This
   // leads to potential segfault bugs when plugins have conflicting
   // symbols, so until wxWindows adds this capability we are calling
   // dlopen() by hand under WXGTK, above...

   wxDllType libHandle = NULL;
     
   libHandle = wxDllLoader::LoadLibrary(FILENAME(fname));
   mainFn = (LADSPA_Descriptor_Function)
      wxDllLoader::GetSymbol(libHandle, descriptorFnName);
#endif

   if (mainFn) {
      int index = 0;
      const LADSPA_Descriptor *data;

      data = mainFn(index);
      while(data) {
         LadspaEffect *effect = new LadspaEffect(data);
         Effect::RegisterEffect(effect);
         
         // Get next plugin
         index++;
         data = mainFn(index);            
      }
   }

   ::wxSetWorkingDirectory(saveOldCWD);
}

void LoadLadspaPlugins()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;
   wxString pathVar;
   unsigned int i;

   pathVar = wxGetenv("LADSPA_PATH");
   if (pathVar != "")
      wxGetApp().AddMultiPathsToPathList(pathVar, pathList);

   #ifdef __WXGTK__
   wxGetApp().AddUniquePathToPathList(INSTALL_PREFIX "/ladspa", pathList);
   wxGetApp().AddUniquePathToPathList("/usr/local/lib/ladspa", pathList);
   wxGetApp().AddUniquePathToPathList("/usr/lib/ladspa", pathList);
   #endif

   for(i=0; i<audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + "ladspa",
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + "plugins",
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + "plug-ins",
                                         pathList);
   }

   #ifdef __WXMSW__
   wxGetApp().FindFilesInPathList("*.dll", pathList, wxFILE, files);   
   #else
   wxGetApp().FindFilesInPathList("*.so", pathList, wxFILE, files);
   #endif

   for(i=0; i<files.GetCount(); i++)
      LoadLadspaEffect(files[i]);
}
