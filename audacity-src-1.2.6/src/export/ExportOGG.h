/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOGG.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_EXPORTOGG__
#define __AUDACITY_EXPORTOGG__

class AudacityProject;

#include <wx/string.h>

bool ExportOGG(
   AudacityProject *project,
   bool stereo,
   wxString fName,
   bool selectionOnly,
   double t0, double t1);

#endif

