/**********************************************************************

  Audacity: A Digital Audio Editor

  Echo.cpp

  Dominic Mazzoni
  Vaughan Johnson (dialog)

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/validate.h>
#include <wx/valtext.h>

#include <wx/generic/textdlgg.h>
#include <wx/intl.h>
#include <math.h>

#include "Echo.h"
#include "../WaveTrack.h"

EffectEcho::EffectEcho()
{
   delay = float(1.0);
   decay = float(0.5);
}

wxString EffectEcho::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   return wxString::Format(_("Applied effect: %s delay = %f seconds, decay factor = %f"), 
                           (const char *)(this->GetEffectName()), delay, decay); 
} 

bool EffectEcho::PromptUser()
{
   EchoDialog dlog(this, mParent, -1, _("Echo"));
   dlog.delay = delay;
   dlog.decay = decay;
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   delay = dlog.delay;
   decay = dlog.decay;

   return true;
}

bool EffectEcho::Process()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         longSampleCount start = track->TimeToLongSamples(t0);
         longSampleCount end = track->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);

         if (!ProcessOne(count, track, start, len))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
}

bool EffectEcho::ProcessOne(int count, WaveTrack * track,
                            longSampleCount start, sampleCount len)
{
   sampleCount s = 0;
   sampleCount blockSize = (sampleCount) (track->GetRate() * delay);
   
   //do nothing if the delay is less than 1 sample or greater than
   //the length of the selection
   if (blockSize < 1 || blockSize > len)
      return true;

   float *buffer0 = new float[blockSize];
   float *buffer1 = new float[blockSize];

   float *ptr0 = buffer0;
   float *ptr1 = buffer1;

   bool first = true;

   while (s < len) {
      sampleCount block = blockSize;
      if (s + block > len)
         block = len - s;

      track->Get((samplePtr)ptr0, floatSample, start + s, block);
      if (!first) {
         for (sampleCount i = 0; i < block; i++)
            ptr0[i] += ptr1[i] * decay;
         track->Set((samplePtr)ptr0, floatSample, start + s, block);
      }

      float *ptrtemp = ptr0;
      ptr0 = ptr1;
      ptr1 = ptrtemp;

      first = false;

      s += block;
      
      if (TrackProgress(count, s / (double) len))
         return false;
   }

   delete[]buffer0;
   delete[]buffer1;

   return true;
}

//----------------------------------------------------------------------------
// EchoDialog
//----------------------------------------------------------------------------

enum {
   ID_TEXT_DELAY = 10001,
   ID_TEXT_DECAY,
	ID_BUTTON_PREVIEW
};


// event table for EchoDialog

BEGIN_EVENT_TABLE(EchoDialog, wxDialog)
    EVT_BUTTON(wxID_OK, EchoDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, EchoDialog::OnCancel)
    EVT_BUTTON(ID_BUTTON_PREVIEW, EchoDialog::OnPreview)
END_EVENT_TABLE()

EchoDialog::EchoDialog(EffectEcho * effect,
								wxWindow * parent, wxWindowID id,
								const wxString & title, 
								const wxPoint & position, 
								const wxSize & size, 
								long style)
: wxDialog(parent, id, title, position, size, style)
{
   m_bLoopDetect = false;
	m_pEffect = effect;

	// NULL out these control members because there are some cases where the 
	// event table handlers get called during this method, and those handlers that 
	// can cause trouble check for NULL.
   m_pTextCtrl_Delay = NULL;
   m_pTextCtrl_Decay = NULL;
	
	// effect parameters
   delay = float(1.0);
   decay = float(0.5);

	// CREATE THE CONTROLS PROGRAMMATICALLY.
	wxStaticText * pStaticText;

   wxBoxSizer * pBoxSizer_Dialog = new wxBoxSizer(wxVERTICAL);

	// heading
   pStaticText = new wxStaticText(this, -1, 
												_("Echo"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxALL, 8);

   pStaticText = new wxStaticText(this, -1, 
												_("by Dominic Mazzoni && Vaughan Johnson"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxTOP | wxLEFT | wxRIGHT, 8);

   pBoxSizer_Dialog->Add(0, 8, 0); // spacer


	// delay
   wxBoxSizer * pBoxSizer_Delay = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText = new wxStaticText(this, -1, _("Delay time (seconds):"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Delay->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

	//v Override wxTextValidator to disallow negative values <= -100.0?
   m_pTextCtrl_Delay = 
		new wxTextCtrl(this, ID_TEXT_DELAY, "1.0", 
							wxDefaultPosition, wxSize(64, -1), 0,
							wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_Delay->Add(m_pTextCtrl_Delay, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_Delay, 0, wxALIGN_CENTER | wxALL, 4);


	// decay
   wxBoxSizer * pBoxSizer_Decay = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText = new wxStaticText(this, -1, _("Decay factor:"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Decay->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

	//v Override wxTextValidator to disallow negative values <= -100.0?
   m_pTextCtrl_Decay = 
		new wxTextCtrl(this, ID_TEXT_DECAY, "0.5", 
							wxDefaultPosition, wxSize(64, -1), 0,
							wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_Decay->Add(m_pTextCtrl_Decay, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_Decay, 0, wxALIGN_CENTER | wxALL, 4);


	// Preview, OK, & Cancel buttons
   pBoxSizer_Dialog->Add(0, 8, 0); // spacer

   wxBoxSizer * pBoxSizer_OK = new wxBoxSizer(wxHORIZONTAL);

   wxButton * pButton_Preview = 
		new wxButton(this, ID_BUTTON_PREVIEW, m_pEffect->GetPreviewName());
   pBoxSizer_OK->Add(pButton_Preview, 0, wxALIGN_CENTER | wxALL, 4);
   pBoxSizer_OK->Add(32, 8); // horizontal spacer

   wxButton * pButton_Cancel =
       new wxButton(this, wxID_CANCEL, _("Cancel"), wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_OK->Add(pButton_Cancel, 0, wxALIGN_CENTER | wxALL, 4);

   wxButton * pButton_OK =
       new wxButton(this, wxID_OK, _("OK"), wxDefaultPosition, wxDefaultSize, 0);
   pButton_OK->SetDefault();
   pButton_OK->SetFocus();
   pBoxSizer_OK->Add(pButton_OK, 0, wxALIGN_CENTER | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_OK, 0, wxALIGN_CENTER | wxALL, 8);


   this->SetAutoLayout(true);
   this->SetSizer(pBoxSizer_Dialog);
   pBoxSizer_Dialog->Fit(this);
   pBoxSizer_Dialog->SetSizeHints(this);
}

bool EchoDialog::Validate()
{
   return true; 
}

bool EchoDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

	wxString str;
	if (m_pTextCtrl_Delay) {
		str.Printf("%f", delay);
		m_pTextCtrl_Delay->SetValue(str);
	}
	if (m_pTextCtrl_Decay) {
		str.Printf("%f", decay);
		m_pTextCtrl_Decay->SetValue(str);
	}

	m_bLoopDetect = false;
	return true;
}

bool EchoDialog::TransferDataFromWindow()
{
   double newValue;
	wxString str;
   if (m_pTextCtrl_Delay) {
      str = m_pTextCtrl_Delay->GetValue();
      str.ToDouble(&newValue);
		delay = (float)(newValue);
	}
   if (m_pTextCtrl_Decay) {
      str = m_pTextCtrl_Decay->GetValue();
      str.ToDouble(&newValue);
		decay = (float)(newValue);
	}
   return true;
}

// handler implementations for EchoDialog

void EchoDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

	// Save & restore parameters around Preview, because we didn't do OK.
	float oldDelay = m_pEffect->delay;
	float oldDecay = m_pEffect->decay;

   m_pEffect->delay = delay;
   m_pEffect->decay = decay;

   m_pEffect->Preview();
   
   m_pEffect->delay = oldDelay;
   m_pEffect->decay = oldDecay;
}

void EchoDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();
   
   if (Validate()) 
      EndModal(true);
   else 
      event.Skip();
}

void EchoDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}


