/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.h

  Mark Phillips
  
  This class inverts the selected audio.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_INVERT__
#define __AUDACITY_EFFECT_INVERT__

#include <wx/intl.h>
#include <wx/string.h>

#include "SimpleMono.h"

class WaveTrack;

class EffectInvert:public EffectSimpleMono {

 public:
   virtual wxString GetEffectName() {
      return wxString(_("Invert"));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Inverting"));
   }
   
 protected:
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);
};

#endif
