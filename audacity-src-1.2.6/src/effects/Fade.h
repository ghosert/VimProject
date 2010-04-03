/**********************************************************************

  Audacity: A Digital Audio Editor

  Fade.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_FADE__
#define __AUDACITY_EFFECT_FADE__

class wxString;

#include <wx/intl.h>
#include "SimpleMono.h"

class EffectFadeIn: public EffectSimpleMono {

 public:
   virtual wxString GetEffectName() {
      return wxString(_("Fade In"));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Fading In"));
   }

 protected:
   sampleCount mSample;
   sampleCount mLen;

   virtual bool NewTrackSimpleMono();
   
   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);
};

class EffectFadeOut:public EffectSimpleMono {

 public:
   virtual wxString GetEffectName() {
      return wxString(_("Fade Out"));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Fading Out"));
   }

 protected:
   sampleCount mSample;
   sampleCount mLen;

   virtual bool NewTrackSimpleMono();

   virtual bool ProcessSimpleMono(float *buffer, sampleCount len);
};

#endif
