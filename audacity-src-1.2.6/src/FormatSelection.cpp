/**********************************************************************

  Audacity: A Digital Audio Editor

  FormatSelection.cpp

  Greg Mekkes

**********************************************************************/

#include "Audacity.h"

#include <math.h>

#include <wx/intl.h>

#include "FormatSelection.h"

wxArrayString GetSelectionFormats()
{
   wxArrayString a;

   a.Add(_("min:sec"));
   a.Add(_("sec"));
   a.Add(_("hr:min:sec"));
   /* i18n-hint: fps is "frames per second" */
   a.Add(_("film frames 24 fps"));
   /* i18n-hint: fps is "frames per second",
    and the other letters are for hours, minutes, seconds, frames */
   a.Add(_("film h:mm:ss:ff 24 fps"));
   /* i18n-hint: fps is "frames per second" */
   a.Add(_("PAL frames 25 fps"));
   /* i18n-hint: fps is "frames per second",
    and the other letters are for hours, minutes, seconds, frames */
   a.Add(_("PAL h:mm:ss:ff 25 fps"));
   /* i18n-hint: fps is "frames per second" */
   a.Add(_("NTSC frames 29.97 fps"));
   /* i18n-hint: hours, minutes, seconds, frames */
   a.Add(_("NTSC drop-frame h:mm:ss:ff"));
   /* i18n-hint: hours, minutes, seconds, frames */
   a.Add(_("NTSC non-drop-frame h:mm:ss:ff"));
   /* i18n-hint: fps is "frames per second",
    cdda is "Compact Disc Digital Audio - don't translate */
   a.Add(_("cdda min:sec:frames 75 fps"));
   a.Add(_("samples (snap to samples)"));
   a.Add(_("min:sec (snap to samples)"));
   a.Add(_("sec (snap to samples)"));
   a.Add(_("min:sec+samples (snap to samples)"));
   a.Add(_("sec+samples (snap to samples)"));

   /* i18n-hint: cdda is "Compact Disc Digital Audio - don't translate.
    A "sector" is a technical term - don't translate unless you're sure
    you know the correct term. */
   a.Add(_("cdda sectors+bytes (snap to samples)"));

   return a;
}

wxString FormatSelection(int iformat, int iSnapTo,
                         double samplerate, ViewInfo *viewInfo)
{
   double start = viewInfo->sel0;
   double end = viewInfo->sel1;
   double length = end - start;

   // Items required for adding a new format:
   //
   //   1. add enum entry for iformat in FormatSelection.h
   //   2. add wxArrayString a entry in GetSelectionFormats()
   //   3. add logic in FormatSelection()

   // use interger iformat to control selection output format
   //
   // formats that are based on ruler time
   //   iformat = SELECTION_FORMAT_RULER_MIN_SEC --> use min:sec.xxxxxx
   //   iformat = SELECTION_FORMAT_RULER_SEC --> use sec.xxxxxx
   //   iformat = SELECTION_FORMAT_RULER_HR_MIN_SEC --> use hr:min:sec.xxxxxx
   //   iformat = SELECTION_FORMAT_RULER_FILM_FRAMES --> use film frames 24 fps
   //   iformat = SELECTION_FORMAT_RULER_FILM_HMMSSFF --> use film h:mm:ss:ff 24 fps
   //   iformat = SELECTION_FORMAT_RULER_PAL_FRAMES --> use PAL frames 25 fps
   //   iformat = SELECTION_FORMAT_RULER_PAL_HMMSSFF --> use PAL h:mm:ss:ff 25 fps
   //   iformat = SELECTION_FORMAT_RULER_NTSC_FRAMES --> use NTSC frames 29.97 fps
   //   iformat = SELECTION_FORMAT_RULER_NTSC_DF_HMMSSFF --> use NTSC drop-frame h:mm:ss:ff
   //   iformat = SELECTION_FORMAT_RULER_NTSC_NDF_HMMSSFF --> use NTSC non-drop-frame h:mm:ss:ff
   //   iformat = SELECTION_FORMAT_RULER_CDDA_MIN_SEC_FRAMES --> use cdda min:sec:frames 75 fps
   // formats that are based on rate and samples and snap to samples irregardless of iSnapTo
   //   iformat = SELECTION_FORMAT_SAMPLES --> use samples
   //   iformat = SELECTION_FORMAT_MIN_SEC --> use min:sec.xxxxxx
   //   iformat = SELECTION_FORMAT_SEC --> use sec.xxxxxx
   //   iformat = SELECTION_FORMAT_MIN_SEC_SAMPLES --> use min:sec+samples
   //   iformat = SELECTION_FORMAT_SEC_SAMPLES --> use sec+samples
   //   iformat = SELECTION_FORMAT_CDDA_SECTORS_BYTES --> use cdda sectors+bytes (2352 byte blocks)
   //                   (only applicable for projects with rate = 44100)
   //
   // iformat now gets passed into FormatSelection as an arguement
   // which is now changed from the View menu of Audacity
   //
   // iSnapTo also gets passed into FormatSelection and is changed
   // from the Audacity View menu
   //
   // samplerate is now set to the project sample rate

   // Issues related to rounding functions:
   //   rint of a double is a double
   //   need to use rintf of a float to get a float
   //   lrint produces a long int from a double - but is not cross platform
   //   use (long int)rint() to get a long int out of a double

   wxString result;
   // TODO: Check whether we need a 'const' in the char * somewhere.
   char * SnapTo[2];
   /* i18n-hint: This snap-to string will appear in the status bar. */
   SnapTo[0] = (char *)_("[Snap-To Off]");
   /* i18n-hint: This snap-to string will appear in the status bar. */
   SnapTo[1] = (char *)_("[Snap-To On]");

   // variables used
   int ihr1, ihr2, ihrtot;
   int itenmin1, itenmin2, itenmintot;
   int iaddmin1, iaddmin2, iaddmintot;
   int imin1, imin2, imintot;
   int isec1, isec2, isectot;
   int isamp1, isamp2, isamptot;
   int isector1, isector2, isectortot;
   int ibyte1, ibyte2, ibytetot;
   long int isamples1, isamples2, isamplestot;
   double dsec1, dsec2, dsectot;
   double dframes1, dframes2, dframestot;
   double dframes1keep, dframestotkeep;

   switch (iformat) {

   case SELECTION_FORMAT_RULER_MIN_SEC:
      // use min:sec.xxxxxx (from ruler)
      imin1 = int(start/60.0);
      imin2 = int(end/60.0);
      imintot = int(length/60.0);
      dsec1 = start - double(imin1*60);
      dsec2 = end - double(imin2*60);
      dsectot = length - double(imintot*60);
      if(iSnapTo == 1) {
         // temporarily put total not leftover sec in dsec variables
         dsec1 = rint(dsec1) + double(imin1*60);
         dsec2 = rint(dsec2) + double(imin2*60);
         dsectot = rint(dsec2 - dsec1);
         viewInfo->sel0 = dsec1;
         viewInfo->sel1 = dsec2;
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
         // now get the real ones to display
         imin1 = int(dsec1/60.0);
         imin2 = int(dsec2/60.0);
         imintot = int(dsectot/60.0);
         dsec1 = dsec1 - double(imin1*60);
         dsec2 = dsec2 - double(imin2*60);
         dsectot = dsectot - double(imintot*60);
      }
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %i:%09.6f min:sec   %s"),
                       imin1, dsec1, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %i:%09.6f - %i:%09.6f (%i:%09.6f min:sec)   %s"),
                       imin1, dsec1, imin2, dsec2, imintot, dsectot, SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_SEC:
      // use sec.xxxxxx (from ruler)
      dsec1 = start;
      dsec2 = end;
      dsectot = length;
      if(iSnapTo == 1) {
         dsec1 = rint(dsec1);
         dsec2 = rint(dsec2);
         dsectot = rint(dsec2 - dsec1);
         viewInfo->sel0 = dsec1;
         viewInfo->sel1 = dsec2;
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
      }
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %lf sec   %s"),
                       dsec1, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %lf - %lf (%lf sec)   %s"),
                       dsec1, dsec2, dsectot, SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_HR_MIN_SEC:
      // use hr:min:sec.xxxxxx (from ruler)
      ihr1 = int(start/3600.0);
      ihr2 = int(end/3600.0);
      ihrtot = int(length/3600.0);
      dsec1 = start - double(ihr1*3600);
      dsec2 = end - double(ihr2*3600);
      dsectot = length - double(ihrtot*3600);
      imin1 = int(dsec1/60.0);
      imin2 = int(dsec2/60.0);
      imintot = int(dsectot/60.0);
      dsec1 -= double(imin1*60);
      dsec2 -= double(imin2*60);
      dsectot -= double(imintot*60);
      if(iSnapTo == 1) {
         // temporarily put total not leftover sec in dsec variables
         dsec1 = rint(dsec1) + double(imin1*60) + double(ihr1*3600);
         dsec2 = rint(dsec2) + double(imin2*60) + double(ihr2*3600);
         dsectot = rint(dsec2 - dsec1);
         viewInfo->sel0 = dsec1;
         viewInfo->sel1 = dsec2;
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
         // now get the real ones to display
         ihr1 = int(dsec1/3600.0);
         ihr2 = int(dsec2/3600.0);
         ihrtot = int(dsectot/3600.0);
         dsec1 -= double(ihr1*3600);
         dsec2 -= double(ihr2*3600);
         dsectot -= double(ihrtot*3600);
         imin1 = int(dsec1/60.0);
         imin2 = int(dsec2/60.0);
         imintot = int(dsectot/60.0);
         dsec1 -= double(imin1*60);
         dsec2 -= double(imin2*60);
         dsectot -= double(imintot*60);
      }
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %1i:%02i:%09.6f hr:min:sec   %s"),
                       ihr1, imin1, dsec1, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %1i:%02i:%09.6f - %1i:%02i:%09.6f (%1i:%02i:%09.6f hr:min:sec)   %s"),
                       ihr1, imin1, dsec1, ihr2, imin2, dsec2, ihrtot, imintot, dsectot, SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_FILM_FRAMES:
      // use film frames 24 fps (from ruler)
      dframes1 = start * 24.0;
      dframes2 = end * 24.0;
      dframestot = length * 24.0;
      if(iSnapTo == 1) {
         dframes1 = rint(dframes1);
         dframes2 = rint(dframes2);
         dframestot = rint(dframes2 - dframes1);
         viewInfo->sel0 = dframes1 / 24.0;
         viewInfo->sel1 = dframes2 / 24.0;
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
      }
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %.3lf film frames (24 fps)   %s"),
                       dframes1, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %.3lf - %.3lf (%.3lf film frames)   %s"),
                       dframes1, dframes2, dframestot, SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_FILM_HMMSSFF:
      // use film h:mm:ss:ff 24 fps (from ruler)
      dframes1 = start * 24.0;
      dframes2 = end * 24.0;
      dframestot = length * 24.0;
      dframes1keep = dframes1;
      dframestotkeep = dframestot;
      if(iSnapTo == 1) {
         dframes1 = rint(dframes1);
         dframes2 = rint(dframes2);
         dframestot = rint(dframes2 - dframes1);
         dframes1keep = dframes1;
         dframestotkeep = dframestot;
         viewInfo->sel0 = dframes1 / 24.0;
         viewInfo->sel1 = dframes2 / 24.0;
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
      }

      // one film hour = 86400 film frames
      ihr1 = int(dframes1/86400.0);
      ihr2 = int(dframes2/86400.0);
      ihrtot = int(dframestot/86400.0);
      dframes1 -= double(ihr1*86400);
      dframes2 -= double(ihr2*86400);
      dframestot -= double(ihrtot*86400);

      // One minute of film = 1440 film frames.
      imin1 = int(dframes1/1440.0);
      imin2 = int(dframes2/1440.0);
      imintot = int(dframestot/1440.0);
      dframes1 -= double(imin1*1440);
      dframes2 -= double(imin2*1440);
      dframestot -= double(imintot*1440);

      // One second of film = 24 frames.
      isec1 = int(dframes1/24.0);
      isec2 = int(dframes2/24.0);
      isectot = int(dframestot/24.0);
      dframes1 -= double(isec1*24);
      dframes2 -= double(isec2*24);
      dframestot -= double(isectot*24);

      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1) 
         result.Printf(_("Cursor: %1i:%02i:%02i:%06.3lf film h:mm:ss:ff 24 fps (%.3lf film frames)   %s"),
                       ihr1, imin1, isec1, dframes1, dframes1keep, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %1i:%02i:%02i:%06.3lf - %1i:%02i:%02i:%06.3lf (%1i:%02i:%02i:%06.3lf film h:mm:ss:ff) (%.3lf film frames)   %s"),
                       ihr1, imin1, isec1, dframes1, ihr2, imin2, isec2, dframes2, ihrtot, imintot, isectot, dframestot, dframestotkeep,
                       SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_PAL_FRAMES:
      // use PAL frames 25 fps (from ruler)
      dframes1 = start * 25.0;
      dframes2 = end * 25.0;
      dframestot = length * 25.0;
      if(iSnapTo == 1) {
         dframes1 = rint(dframes1);
         dframes2 = rint(dframes2);
         dframestot = rint(dframes2 - dframes1);
         viewInfo->sel0 = dframes1 / 25.0;
         viewInfo->sel1 = dframes2 / 25.0;
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
      }
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %.3lf PAL frames (25 fps)   %s"),
                       dframes1, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %.3lf - %.3lf (%.3lf PAL frames)   %s"),
                       dframes1, dframes2, dframestot, SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_PAL_HMMSSFF:
      // use PAL h:mm:ss:ff 25 fps (from ruler)
      dframes1 = start * 25.0;
      dframes2 = end * 25.0;
      dframestot = length * 25.0;
      dframes1keep = dframes1;
      dframestotkeep = dframestot;
      if(iSnapTo == 1) {
         dframes1 = rint(dframes1);
         dframes2 = rint(dframes2);
         dframestot = rint(dframes2 - dframes1);
         dframes1keep = dframes1;
         dframestotkeep = dframestot;
         viewInfo->sel0 = dframes1 / 25.0;
         viewInfo->sel1 = dframes2 / 25.0;
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
      }

      // one PAL hour = 90000 PAL frames
      ihr1 = int(dframes1/90000.0);
      ihr2 = int(dframes2/90000.0);
      ihrtot = int(dframestot/90000.0);
      dframes1 -= double(ihr1*90000);
      dframes2 -= double(ihr2*90000);
      dframestot -= double(ihrtot*90000);

      // One minute of PAL video = 1500 PAL frames.
      imin1 = int(dframes1/1500.0);
      imin2 = int(dframes2/1500.0);
      imintot = int(dframestot/1500.0);
      dframes1 -= double(imin1*1500);
      dframes2 -= double(imin2*1500);
      dframestot -= double(imintot*1500);

      // One second of PAL video = 25 PAL frames.
      isec1 = int(dframes1/25.0);
      isec2 = int(dframes2/25.0);
      isectot = int(dframestot/25.0);
      dframes1 -= double(isec1*25);
      dframes2 -= double(isec2*25);
      dframestot -= double(isectot*25);

      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %1i:%02i:%02i:%06.3lf PAL h:mm:ss:ff 25 fps (%.3lf PAL frames)   %s"),
                       ihr1, imin1, isec1, dframes1, dframes1keep, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %1i:%02i:%02i:%06.3lf - %1i:%02i:%02i:%06.3lf (%1i:%02i:%02i:%06.3lf PAL h:mm:ss:ff) (%.3lf PAL frames)   %s"),
                       ihr1, imin1, isec1, dframes1, ihr2, imin2, isec2, dframes2, ihrtot, imintot, isectot, dframestot, dframestotkeep,
                       SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_NTSC_FRAMES:
      // use NTSC frames 29.97 fps (from ruler)
      dframes1 = start * (30000.0/1001.0);
      dframes2 = end * (30000.0/1001.0);
      dframestot = length * (30000.0/1001.0);
      if(iSnapTo == 1) {
         dframes1 = rint(dframes1);
         dframes2 = rint(dframes2);
         dframestot = rint(dframes2 - dframes1);
         viewInfo->sel0 = dframes1 * (1001.0/30000.0);
         viewInfo->sel1 = dframes2 * (1001.0/30000.0);
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
      }
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %.3lf NTSC frames (29.97 fps)   %s"),
                       dframes1, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %.3lf - %.3lf (%.3lf NTSC frames)   %s"),
                       dframes1, dframes2, dframestot, SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_NTSC_DF_HMMSSFF:
      // use NTSC frop-frame h:mm:ss:ff (from ruler)
      dframes1 = start * (30000.0/1001.0);
      dframes2 = end * (30000.0/1001.0);
      dframestot = length * (30000.0/1001.0);
      dframes1keep = dframes1;
      dframestotkeep = dframestot;
      if(iSnapTo == 1) {
         dframes1 = rint(dframes1);
         dframes2 = rint(dframes2);
         dframestot = rint(dframes2 - dframes1);
         dframes1keep = dframes1;
         dframestotkeep = dframestot;
         viewInfo->sel0 = dframes1 * (1001.0/30000.0);
         viewInfo->sel1 = dframes2 * (1001.0/30000.0);
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
               viewInfo->sel0 = viewInfo->sel1;
      }

      // one NTSC drop frame hour = 107892 NTSC frames
      // with all dropped frames accounted for
      ihr1 = int(dframes1/107892.0);
      ihr2 = int(dframes2/107892.0);
      ihrtot = int(dframestot/107892.0);
      dframes1 -= double(ihr1*107892);
      dframes2 -= double(ihr2*107892);
      dframestot -= double(ihrtot*107892);
      // ten minutes of NTSC drop frame = 17982 NTSC frames
      // with all dropped frames accounted for
      itenmin1 = int(dframes1/17982.0);
      itenmin2 = int(dframes2/17982.0);
      itenmintot = int(dframestot/17982.0);
      imin1 = itenmin1 * 10;
      imin2 = itenmin2 * 10;
      imintot = itenmintot * 10;
      dframes1 -= double(itenmin1*17982);
      dframes2 -= double(itenmin2*17982);
      dframestot -= double(itenmintot*17982);

      // One minute of NTSC drop frame video = 1800 NTSC frames for every tenth minute.
      // One minute of NTSC drop frame video = 1798 NTSC frames for all other minutes.
      // The first minute of a ten minute section has all frames, the other nine have
      // two frames dropped (only on the timeline frame index).
      // To drop two frames on the timeline index, need to add 2 frames, effectively
      // skipping two timeline frames by incrementing over them.
      // This is accomplished when the amount of remaining frames at this point is
      // greater than or equal to 1800.  If so, then 1800 is subtracted from the
      // remaining frames, and the minute count is incremented by 1.  Then to drop
      // the 2 frames on the timeline index, 2 is later added to the
      // subsequent remaining frame count.  There are 30 frames in an NTSC drop-frame
      // second because the two dropped frames in the timeline index effectively make the
      // rate adjustment to the actual 29.97 frame rate, which is really 30000/1001.

      if(dframes1 >= 1800.0) {
         dframes1 -= 1800.0;
         imin1 += 1;
         iaddmin1 = int(dframes1/1798.0);
         dframes1 -= double(iaddmin1*1798);
         imin1 += iaddmin1;
         isec1 = int(dframes1/30.0);
         dframes1 -= double(isec1*30);
         dframes1 += 2.0;
         if(dframes1 >= 30.0) {
            isec1 += 1;
            dframes1 -= 30.0;
         }
      }
      else {
         isec1 = int(dframes1/30.0);
         dframes1 -= double(isec1*30);
      }

      if(dframes2 >= 1800.0) {
         dframes2 -= 1800.0;
         imin2 += 1;
         iaddmin2 = int(dframes2/1798.0);
         dframes2 -= double(iaddmin2*1798);
         imin2 += iaddmin2;
         isec2 = int(dframes2/30.0);
         dframes2 -= double(isec2*30);
         dframes2 += 2.0;
         if(dframes2 >= 30.0) {
            isec2 += 1;
            dframes2 -= 30.0;
         }
      }
      else {
         isec2 = int(dframes2/30.0);
         dframes2 -= double(isec2*30);
      }
      
      if(dframestot >= 1800.0) {
         dframestot -= 1800.0;
         imintot += 1;
         iaddmintot = int(dframestot/1798.0);
         dframestot -= double(iaddmintot*1798);
         imintot += iaddmintot;
         isectot = int(dframestot/30.0);
         dframestot -= double(isectot*30);
         dframestot += 2.0;
         if(dframestot >= 30.0) {
            isectot += 1;
            dframestot -= 30.0;
         }
      }
      else {
         isectot = int(dframestot/30.0);
         dframestot -= double(isectot*30);
      }

      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %1i:%02i:%02i:%06.3lf NTSC drop-frame h:mm:ss:ff (%.3lf NTSC frames)   %s"),
                       ihr1, imin1, isec1, dframes1, dframes1keep, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %1i:%02i:%02i:%06.3lf - %1i:%02i:%02i:%06.3lf (%1i:%02i:%02i:%06.3lf NTSC drop-frame h:mm:ss:ff) (%.3lf NTSC frames)   %s"),
                       ihr1, imin1, isec1, dframes1, ihr2, imin2, isec2, dframes2, ihrtot, imintot, isectot, dframestot, dframestotkeep,
                       SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_NTSC_NDF_HMMSSFF:
      // use NTSC non-frop-frame h:mm:ss:ff (from ruler)
      dframes1 = start * (30000.0/1001.0);
      dframes2 = end * (30000.0/1001.0);
      dframestot = length * (30000.0/1001.0);
      dframes1keep = dframes1;
      dframestotkeep = dframestot;
      if(iSnapTo == 1) {
         dframes1 = rint(dframes1);
         dframes2 = rint(dframes2);
         dframestot = rint(dframes2 - dframes1);
         dframes1keep = dframes1;
         dframestotkeep = dframestot;
         viewInfo->sel0 = dframes1 * (1001.0/30000.0);
         viewInfo->sel1 = dframes2 * (1001.0/30000.0);
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
      }
      
      // one NTSC non drop frame hour = 108000 NTSC frames
      ihr1 = int(dframes1/108000.0);
      ihr2 = int(dframes2/108000.0);
      ihrtot = int(dframestot/108000.0);
      dframes1 -= double(ihr1*108000);
      dframes2 -= double(ihr2*108000);
      dframestot -= double(ihrtot*108000);

      // One minute of NTSC non drop frame video = 1800 NTSC frames.
      // Note that the frames will always play at 29.97 (really 30000/1001) fps,
      // and the non drop frame timeline indexing will not match real time.
      // NTSC non drop frame is just an alternate timeline frame indexing scheme.

      imin1 = int(dframes1/1800.0);
      imin2 = int(dframes2/1800.0);
      imintot = int(dframestot/1800.0);
      dframes1 -= double(imin1*1800);
      dframes2 -= double(imin2*1800);
      dframestot -= double(imintot*1800);

      // One second of NTSC non drop frame video = 30 NTSC frames.
      isec1 = int(dframes1/30.0);
      isec2 = int(dframes2/30.0);
      isectot = int(dframestot/30.0);
      dframes1 -= double(isec1*30);
      dframes2 -= double(isec2*30);
      dframestot -= double(isectot*30);

      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %1i:%02i:%02i:%06.3lf NTSC non-drop-frame h:mm:ss:ff (%.3lf NTSC frames)   %s"),
                       ihr1, imin1, isec1, dframes1, dframes1keep, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %1i:%02i:%02i:%06.3lf - %1i:%02i:%02i:%06.3lf (%1i:%02i:%02i:%06.3lf NTSC non-drop-frame h:mm:ss:ff) (%.3lf NTSC frames)   %s"),
                       ihr1, imin1, isec1, dframes1, ihr2, imin2, isec2, dframes2, ihrtot, imintot, isectot, dframestot, dframestotkeep,
                       SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_RULER_CDDA_MIN_SEC_FRAMES:
      // use cdda min:sec:frames.xxxxxx (from ruler)
      imin1 = int(start/60.0);
      imin2 = int(end/60.0);
      imintot = int(length/60.0);
      isec1 = int(start) - (imin1*60);
      isec2 = int(end) - (imin2*60);
      isectot = int(length) - (imintot*60);
      dframes1 = (start - double(int(start))) * 75.0;
      dframes2 = (end - double(int(end))) * 75.0;
      dframestot = (length - double(int(length))) * 75.0;
      if(iSnapTo == 1) {
         // temporarily put total not leftover frames in dframes variables
         dframes1 = rint(dframes1) + double(isec1*75) + double(imin1*60*75);
         dframes2 = rint(dframes2) + double(isec2*75) + double(imin2*60*75);
         dframestot = dframes2 - dframes1;
         viewInfo->sel0 = dframes1/75.0;
         viewInfo->sel1 = dframes2/75.0;
         // if rounding beyond end happens fix sel0 for proper cursor info
         if(viewInfo->sel0 > viewInfo->sel1)
            viewInfo->sel0 = viewInfo->sel1;
         // now get the real ones to display
         imin1 = int(dframes1/75.0/60.0);
         imin2 = int(dframes2/75.0/60.0);
         imintot = int(dframestot/75.0/60.0);
         isec1 = int(dframes1/75.0) - (imin1*60);
         isec2 = int(dframes2/75.0) - (imin2*60);
         isectot = int(dframestot/75.0) - (imintot*60);
         dframes1 = dframes1 - double(imin1*60*75) - double(isec1*75);
         dframes2 = dframes2 - double(imin2*60*75) - double(isec2*75);
         dframestot = dframestot - double(imintot*60*75) - double(isectot*75);
      }
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %02i:%02i:%06.3lf cdda min:sec:frames (75 fps)   %s"),
                       imin1, isec1, dframes1, SnapTo[iSnapTo]);
      else
         result.Printf(_("Selection: %02i:%02i:%06.3lf - %02i:%02i:%06.3lf (%02i:%02i:%06.3lf cdda min:sec:frames)   %s"),
                       imin1, isec1, dframes1, imin2, isec2, dframes2, imintot, isectot, dframestot, SnapTo[iSnapTo]);
      break;

   case SELECTION_FORMAT_SAMPLES:
      // use samples (in single sample time increments)
      isamples1 = (long int)rint(samplerate*start); 
      isamples2 = (long int)rint(samplerate*end);
      isamplestot = isamples2 - isamples1;
      // snap to samples irregardless of snap-to mode
      viewInfo->sel0 = double(isamples1)/samplerate;
      viewInfo->sel1 = double(isamples2)/samplerate;
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %li samples   [Snap-To Samples]"),
                       isamples1);
      else
         result.Printf(_("Selection: %li - %li (%li samples)   [Snap-To Samples]"),
                       isamples1, isamples2, isamplestot);
      break;

   case SELECTION_FORMAT_MIN_SEC:
      // use min:sec.xxxxxx (in single sample time increments)
      isamples1 = (long int)rint(samplerate*start); 
      isamples2 = (long int)rint(samplerate*end);
      isamplestot = isamples2 - isamples1;
      // based on samples get min and sec
      imin1 = int((double(isamples1)/samplerate)/60.0);
      imin2 = int((double(isamples2)/samplerate)/60.0);
      imintot = int((double(isamplestot)/samplerate)/60.0);
      dsec1 = double(isamples1)/samplerate - double(imin1*60);
      dsec2 = double(isamples2)/samplerate - double(imin2*60);
      dsectot = double(isamplestot)/samplerate - double(imintot*60);
      // snap to samples irregardless of snap-to mode
      viewInfo->sel0 = double(isamples1)/samplerate;
      viewInfo->sel1 = double(isamples2)/samplerate;
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %i:%09.6lf min:sec   [Snap-To Samples]"),
                       imin1, dsec1);
      else
         result.Printf(_("Selection: %i:%09.6lf - %i:%09.6lf (%i:%09.6lf min:sec)   [Snap-To Samples]"),
                       imin1, dsec1, imin2, dsec2, imintot, dsectot);
      break;

   case SELECTION_FORMAT_SEC:
      // use sec.xxxxxx (in sample time increments)
      isamples1 = (long int)rint(samplerate*start); 
      isamples2 = (long int)rint(samplerate*end);
      isamplestot = isamples2 - isamples1;
      // based on samples get sec
      dsec1 = double(isamples1)/samplerate;
      dsec2 = double(isamples2)/samplerate;
      dsectot = double(isamplestot)/samplerate;
      // snap to samples irregardless of snap-to mode
      viewInfo->sel0 = double(isamples1)/samplerate;
      viewInfo->sel1 = double(isamples2)/samplerate;
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %lf sec   [Snap-To Samples]"),
                       dsec1);
      else
         result.Printf(_("Selection: %lf - %lf (%lf sec)   [Snap-To Samples]"),
                       dsec1, dsec2, dsectot);
      break;

   case SELECTION_FORMAT_MIN_SEC_SAMPLES:
      // use min:sec+samples (in single sample time increments)
      isamples1 = (long int)rint(samplerate*start); 
      isamples2 = (long int)rint(samplerate*end);
      isamplestot = isamples2 - isamples1;
      // based on samples get min sec samp
      imin1 = int((double(isamples1)/samplerate)/60.0);
      imin2 = int((double(isamples2)/samplerate)/60.0);
      imintot = int((double(isamplestot)/samplerate)/60.0);
      isec1 = int(double(isamples1)/samplerate - double(imin1*60));
      isec2 = int(double(isamples2)/samplerate - double(imin2*60));
      isectot = int(double(isamplestot)/samplerate - double(imintot*60));
      isamp1 = int(isamples1-(long int)(double(imin1)*60.0*samplerate)-(long int)(double(isec1)*samplerate));
      isamp2 = int(isamples2-(long int)(double(imin2)*60.0*samplerate)-(long int)(double(isec2)*samplerate));
      isamptot = int(isamplestot-(long int)(double(imintot)*60.0*samplerate)-(long int)(double(isectot)*samplerate));
      // snap to samples irregardless of snap-to mode
      viewInfo->sel0 = double(isamples1)/samplerate;
      viewInfo->sel1 = double(isamples2)/samplerate;
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %i:%02i+%i min:sec+samples   [Snap-To Samples]"),
                       imin1, isec1, isamp1);
      else
         result.Printf(_("Selection: %i:%02i+%i - %i:%02i+%i (%i:%02i+%i min:sec+samples)   [Snap-To Samples]"),
                       imin1, isec1, isamp1, imin2, isec2, isamp2, imintot, isectot, isamptot);
      break;

   case SELECTION_FORMAT_SEC_SAMPLES:
      // use sec+samples (in single sample time increments)
      isamples1 = (long int)rint(samplerate*start); 
      isamples2 = (long int)rint(samplerate*end);
      isamplestot = isamples2 - isamples1;
      // based on samples get sec samp
      isec1 = int(double(isamples1)/samplerate);
      isec2 = int(double(isamples2)/samplerate);
      isectot = int(double(isamplestot)/samplerate);
      isamp1 = int(isamples1-(long int)(double(isec1)*samplerate));
      isamp2 = int(isamples2-(long int)(double(isec2)*samplerate));
      isamptot = int(isamplestot-(long int)(double(isectot)*samplerate));
      // snap to samples irregardless of snap-to mode
      viewInfo->sel0 = double(isamples1)/samplerate;
      viewInfo->sel1 = double(isamples2)/samplerate;
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         result.Printf(_("Cursor: %i+%i sec+samples   [Snap-To Samples]"),
                       isec1, isamp1);
      else
         result.Printf(_("Selection: %i+%i - %i+%i (%i+%i sec+samples)   [Snap-To Samples]"),
                       isec1, isamp1, isec2, isamp2, isectot, isamptot);
      break;

   case SELECTION_FORMAT_CDDA_SECTORS_BYTES:
      // use cdda sectors + bytes (2352 byte blocks)
      isamples1 = (long int)rint(samplerate*start); 
      isamples2 = (long int)rint(samplerate*end);
      isamplestot = isamples2 - isamples1;
      //
      // based on samples get cdda sectors bytes
      //
      // Audio CDs use 2352 byte blocks as sector size.  At 16 bit stereo each sample = 4 bytes
      // and each sector = 2352 / 4 = 588 samples.  As a sanity check, a 74 minute audio cd
      // contains 333000 sectors:
      // 
      // 588 samples   333000 sectors       1 sec        1 min
      // ----------- * -------------- * ------------- * ------ = 74.0 minutes per cd (Checks out)
      //   1 sector         1 cd        44100 samples   60 sec
      //
      isector1 = int(double(isamples1)/588.0);
      isector2 = int(double(isamples2)/588.0);
      isectortot = int(double(isamplestot)/588.0);
      ibyte1 = int(isamples1-(long int)(isector1*588))*4;
      ibyte2 = int(isamples2-(long int)(isector2*588))*4;
      ibytetot = int(isamplestot-(long int)(isectortot*588))*4;
      // snap to samples irregardless of snap-to mode
      viewInfo->sel0 = double(isamples1)/samplerate;
      viewInfo->sel1 = double(isamples2)/samplerate;
      // display a message about the selection in the status message window
      if(viewInfo->sel0 == viewInfo->sel1)
         if((int)rint(samplerate) == 44100)
           result.Printf(_("Cursor: %i+%04i cdda sectors+bytes (2352 bytes per sector)   [Snap-To Samples]"),
                       isector1, ibyte1);
         else
           result.Printf(_("Cursor: %i+%04i cdda sectors+bytes (2352 bytes per sector)   [Snap-To Samples]   Change Project Rate to 44100 for cdda"),
                       isector1, ibyte1);
      else
         if((int)rint(samplerate) == 44100)
           result.Printf(_("Selection: %i+%04i - %i+%04i (%i+%04i cdda sectors+bytes)   [Snap-To Samples]"),
                       isector1, ibyte1, isector2, ibyte2, isectortot, ibytetot);
         else
           result.Printf(_("Selection: %i+%04i - %i+%04i (%i+%04i cdda sectors+bytes)   [Snap-To Samples]   Change Project Rate to 44100 for cdda"),
                       isector1, ibyte1, isector2, ibyte2, isectortot, ibytetot);
      break;

   default:
      // This is an unusual error - doesn't need to be translated.
      result.Printf("Selection: invalid iformat value (%i) in FormatSelection.cpp",iformat);
      break;

   }

   return result;
}
