/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseRemoval.h

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NOISE_REMOVAL__
#define __AUDACITY_EFFECT_NOISE_REMOVAL__

#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/intl.h>

class wxString;

#include "Effect.h"

class Envelope;
class WaveTrack;

class EffectNoiseRemoval: public Effect {
   
public:
   
   EffectNoiseRemoval();
   virtual ~EffectNoiseRemoval();

   virtual wxString GetEffectName() {
      return wxString(_("Noise Removal..."));
   }
   
   virtual wxString GetEffectAction() {
      if (doProfile)
         return wxString(_("Creating Noise Profile"));
      else
         return wxString(_("Removing Noise"));
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();
   
private:
   bool ProcessOne(int count, WaveTrack * track,
                   longSampleCount start, sampleCount len);

   void GetProfile(sampleCount len,
                   float *buffer);
   void RemoveNoise(sampleCount len,
                    float *buffer);
   
   Envelope *mEnvelope;

   int       windowSize;
   float    *noiseGate;
   float    *sum;
   float    *sumsq;
   float    *smoothing;
   int      *profileCount;
   bool      doProfile;
   bool      hasProfile;
   
   int       level;

friend class NoiseRemovalDialog;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// NoiseRemovalDialog
//----------------------------------------------------------------------------

// Declare window functions

class NoiseRemovalDialog: public wxDialog
{
public:
   // constructors and destructors
   NoiseRemovalDialog(EffectNoiseRemoval * effect,
								wxWindow *parent, wxWindowID id, 
								const wxString &title,
								const wxPoint& pos = wxDefaultPosition,
								const wxSize& size = wxDefaultSize,
								long style = wxDEFAULT_DIALOG_STYLE );

   wxSizer *MakeNoiseRemovalDialog(bool call_fit = true, bool set_sizer = true);
   
private:
   // handlers
   void OnGetProfile( wxCommandEvent &event );
   void OnPreview(wxCommandEvent &event);
   void OnRemoveNoise( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   
private:
	EffectNoiseRemoval * m_pEffect;

public:
   wxButton * m_pButton_GetProfile;
   wxSlider * m_pSlider;
   wxButton * m_pButton_Preview;
   wxButton * m_pButton_RemoveNoise;
   
private:
   DECLARE_EVENT_TABLE()
};

#endif
