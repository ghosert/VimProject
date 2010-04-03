/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportPCM.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORTPCM__
#define __AUDACITY_EXPORTPCM__

class wxString;

class AudacityProject;
class DirManager;
class WaveTrack;

bool ExportPCM(AudacityProject *project,
               bool stereo, wxString fName,
               bool selectionOnly, double t0, double t1);


#endif
