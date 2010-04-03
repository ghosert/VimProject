/**********************************************************************

  Audacity: A Digital Audio Editor

  FormatSelection.h

  Greg Mekkes

  Returns a formatted string describing the selection, with a
  variety of possible selection formats.

  If "snap-to" mode is selected, snaps the selection to the nearest
  unit as well (by modifying a ViewInfo structure in-place).

**********************************************************************/

#include <wx/defs.h>
#include <wx/string.h>

#include "ViewInfo.h"

enum {
   SELECTION_FORMAT_RULER_MIN_SEC,
   SELECTION_FORMAT_RULER_SEC,
   SELECTION_FORMAT_RULER_HR_MIN_SEC,
   SELECTION_FORMAT_RULER_FILM_FRAMES,
   SELECTION_FORMAT_RULER_FILM_HMMSSFF,
   SELECTION_FORMAT_RULER_PAL_FRAMES,
   SELECTION_FORMAT_RULER_PAL_HMMSSFF,
   SELECTION_FORMAT_RULER_NTSC_FRAMES,
   SELECTION_FORMAT_RULER_NTSC_DF_HMMSSFF,
   SELECTION_FORMAT_RULER_NTSC_NDF_HMMSSFF,
   SELECTION_FORMAT_RULER_CDDA_MIN_SEC_FRAMES,
   SELECTION_FORMAT_SAMPLES,
   SELECTION_FORMAT_MIN_SEC,
   SELECTION_FORMAT_SEC,
   SELECTION_FORMAT_MIN_SEC_SAMPLES,
   SELECTION_FORMAT_SEC_SAMPLES,
   SELECTION_FORMAT_CDDA_SECTORS_BYTES
};

wxArrayString GetSelectionFormats();

wxString FormatSelection(int iformat, int iSnapTo, 
                         double samplerate, ViewInfo *viewInfo);


