/**********************************************************************

  Audacity: A Digital Audio Editor

  PopClick.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_POPCLICK__
#define __AUDACITY_EFFECT_POPCLICK__

#include <wx/intl.h>
#include <wx/string.h>

#include "SimpleMono.h"

class WaveTrack;

class EffectPopClickRemoval: public Effect {
   
public:
   
   EffectPopClickRemoval();
   virtual ~EffectPopClickRemoval();

   virtual wxString GetEffectName() {
      return wxString(_("Pop and Click Removal..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Removing unwanted noise"));
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();
   
private:
   bool ProcessOne(int count, WaveTrack * track,
                   longSampleCount start,
                   sampleCount preLen, sampleCount postLen);
};

#endif
