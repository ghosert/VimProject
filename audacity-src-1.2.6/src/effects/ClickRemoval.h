/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemoval.h

  Craig DeForest

  (Structure largely stolen from NoiseRemoval.h by Dominic Mazzoni)

  This file is intended to become part of Audacity.  You may modify and/or
  distribute it under the same terms as Audacity itself.

**********************************************************************/

#ifndef __AUDACITY_EFFECT_CLICK_REMOVAL__
#define __AUDACITY_EFFECT_CLICK_REMOVAL__

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

class EffectClickRemoval: public Effect {
   
public:
   
   EffectClickRemoval();
   virtual ~EffectClickRemoval();

   virtual wxString GetEffectName() {
      return wxString(_("Click Removal..."));
   }
   
   virtual wxString GetEffectAction() {
         return wxString(_("Removing clicks and pops..."));
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();
   
private:
   bool ProcessOne(int count, WaveTrack * track,
                   longSampleCount start, sampleCount len);

   void RemoveClicks(sampleCount len,
                    float *buffer);
   
   Envelope *mEnvelope;

   int       windowSize;
   int       level;
   int       width;
   int  sep;

friend class ClickRemovalDialog;
};

// WDR: class declarations

//----------------------------------------------------------------------------
// ClickRemovalDialog
//----------------------------------------------------------------------------

// Declare window functions

class ClickRemovalDialog: public wxDialog
{
public:
   // constructors and destructors
   ClickRemovalDialog(EffectClickRemoval * effect,
								wxWindow *parent, wxWindowID id, 
								const wxString &title,
								const wxPoint& pos = wxDefaultPosition,
								const wxSize& size = wxDefaultSize,
								long style = wxDEFAULT_DIALOG_STYLE );

   wxSizer *MakeClickRemovalDialog(bool call_fit = true, bool set_sizer = true);
   
private:
   // handlers
   void OnPreview(wxCommandEvent &event);
   void OnRemoveClicks( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   
private:
	EffectClickRemoval * m_pEffect;

public:
   wxButton * m_pButton_GetProfile;
   wxSlider * m_pSlider;
   wxSlider * m_pSlider_width;
   wxSlider * m_pSlider_sep;
   wxButton * m_pButton_Preview;
   wxButton * m_pButton_RemoveClicks;
   
private:
   DECLARE_EVENT_TABLE()
};

#endif
