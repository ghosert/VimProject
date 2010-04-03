/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportCL.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_EXPORTCL__
#define __AUDACITY_EXPORTCL__

#include <wx/string.h>

bool ExportCL(AudacityProject *project, bool stereo, wxString fName,
              bool selectionOnly, double t0, double t1);

#endif
