/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadNyquist.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/filefn.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "../../Audacity.h"
#include "../../AudacityApp.h"
#include "Nyquist.h"

void LoadNyquistEffect(wxString fname)
{
   EffectNyquist *effect = new EffectNyquist(fname);
   if (effect->LoadedNyFile())
      Effect::RegisterEffect(effect);
   else
      delete effect;
}

void LoadNyquistPlugins()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;
   unsigned int i;

   // Create one "interactive Nyquist"
   EffectNyquist *effect = new EffectNyquist("");
   Effect::RegisterEffect(effect);

   // Load .ny plug-ins
   for(i=0; i<audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + "nyquist",
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + "plugins",
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + "plug-ins",
                                         pathList);
   }

   wxGetApp().FindFilesInPathList("*.ny", pathList, wxFILE, files);

   for(i=0; i<files.GetCount(); i++)
      LoadNyquistEffect(files[i]);
}
