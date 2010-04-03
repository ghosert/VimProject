/**********************************************************************

  Audacity: A Digital Audio Editor

  Audacity.h

  Dominic Mazzoni
  Joshua Haberman

  This is the main include file for Audacity.  All files which need
  any Audacity-specific #defines or need to access any of Audacity's
  global functions should #include this file.

**********************************************************************/

#ifndef __AUDACITY_H__
#define __AUDACITY_H__

// Increment this every time you release a new version
#define AUDACITY_VERSION_STRING "1.2.6"

// Increment this every time the prefs need to be reset
// the first part (before the r) indicates the version the reset took place
// the second part (after the r) indicates the number of times the prefs have been reset within the same version
#define AUDACITY_PREFS_VERSION_STRING "1.1.1r1"

// Don't change this unless the file format changes
// in an irrevocable way
#define AUDACITY_FILE_FORMAT_VERSION "1.1.0"

class wxWindow;
class ToolBarStub;

extern wxWindow *gParentWindow;
extern ToolBarStub *gControlToolBarStub;
extern ToolBarStub *gMixerToolBarStub;
extern ToolBarStub *gEditToolBarStub;
extern ToolBarStub *gMeterToolBarStub;

void QuitAudacity(bool bForce);
void QuitAudacity();

#ifdef __WXMAC__
#include "configmac.h"
#endif

#ifdef __WXGTK__
#include "configunix.h"
#endif

#ifdef __WXX11__
#include "configunix.h"
#endif

#ifdef __WXMSW__
#include "configwin.h"
#endif

#endif // __AUDACITY_H__
