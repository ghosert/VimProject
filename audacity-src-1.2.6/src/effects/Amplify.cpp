/**********************************************************************

  Audacity: A Digital Audio Editor

  Amplify.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

  This rewritten class supports a smart Amplify effect - it calculates
  the maximum amount of gain that can be applied to all tracks without
  causing clipping and selects this as the default parameter.

**********************************************************************/

#include <math.h>
#include <wx/intl.h>
#include <wx/valtext.h>

#include "Amplify.h"
#include "../WaveTrack.h"

//
// EffectAmplify
//

EffectAmplify::EffectAmplify()
{
   ratio = float(1.0);
}

wxString EffectAmplify::GetEffectDescription() { 
   // Note: This is useful only after ratio has been set. 
   return wxString::Format(_("Applied effect: %s %.1f dB"), 
                           (const char *)(this->GetEffectName()), 20*log10(ratio)); 
} 

bool EffectAmplify::Init()
{
   peak = float(0.0);

   TrackListIterator iter(mWaveTracks);
   Track *t = iter.First();
   int count = 0;
   while(t) {
      float min, max;
      ((WaveTrack *)t)->GetMinMax(&min, &max, mT0, mT1);
      float newpeak = (fabs(min) > fabs(max) ? fabs(min) : fabs(max));
      
      if (newpeak > peak)
         peak = newpeak;
   
      t = iter.Next();
      count++;
   }
   
   return true;

}

bool EffectAmplify::PromptUser()
{
   AmplifyDialog dlog(this, mParent, -1, _("Amplify"));
   dlog.peak = peak;
   dlog.ratio = 1.0 / peak;
   dlog.TransferDataToWindow();
   dlog.CenterOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   ratio = dlog.ratio;
   if (dlog.noclip && ratio*peak > 1.0)
      ratio = 1.0 / peak;

   return true;
}

bool EffectAmplify::ProcessSimpleMono(float *buffer, sampleCount len)
{
   sampleCount i;
   for (i = 0; i < len; i++)
      buffer[i] = (buffer[i] * ratio);
   return true;
}

//----------------------------------------------------------------------------
// AmplifyDialog
//----------------------------------------------------------------------------

#define AMP_MIN -240
#define AMP_MAX 240

// WDR: event table for AmplifyDialog

BEGIN_EVENT_TABLE(AmplifyDialog, wxDialog)
    EVT_BUTTON(wxID_OK, AmplifyDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, AmplifyDialog::OnCancel)
    EVT_SLIDER(ID_AMP_SLIDER, AmplifyDialog::OnAmpSlider)
    EVT_TEXT(ID_AMP_TEXT, AmplifyDialog::OnAmpText)
    EVT_TEXT(ID_PEAK_TEXT, AmplifyDialog::OnPeakText)
    EVT_CHECKBOX(ID_CLIP_CHECKBOX, AmplifyDialog::OnClipCheckBox)
    EVT_BUTTON(ID_BUTTON_PREVIEW, AmplifyDialog::OnPreview)
END_EVENT_TABLE()

AmplifyDialog::AmplifyDialog(EffectAmplify * effect, 
									  wxWindow * parent, wxWindowID id, 
									  const wxString & title, 
									  const wxPoint & position, const wxSize & size, 
									  long style):
wxDialog(parent, id, title, position, size, style)
{
   mLoopDetect = false;
	m_pEffect = effect;

   ratio = float(1.0);
   peak = float(0.0);

	wxStaticText * pStaticText;

   wxBoxSizer * pBoxSizer_Dialog = new wxBoxSizer(wxVERTICAL);

   pStaticText = new wxStaticText(this, -1,
												_("Amplify by Dominic Mazzoni"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxALL, 10);

	// Amplification text control
   wxBoxSizer *item2 = new wxBoxSizer(wxHORIZONTAL);

   pStaticText = new wxStaticText(this, -1, _("Amplification (dB):"),
												wxDefaultPosition, wxDefaultSize, 0);
   item2->Add(pStaticText, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl * item4 = new wxTextCtrl(this, ID_AMP_TEXT, "", 
													wxDefaultPosition, wxSize(60, -1), 
													0, wxTextValidator(wxFILTER_NUMERIC));
   item2->Add(item4, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   pBoxSizer_Dialog->Add(item2, 0, wxALIGN_CENTER | wxALL, 5);

	// slider
   wxSlider *item5 =
       new wxSlider(this, ID_AMP_SLIDER, 0, AMP_MIN, AMP_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   pBoxSizer_Dialog->Add(item5, 1, wxGROW | wxALIGN_CENTER | wxALL, 5);

	// New Peak Amplitude text control
   wxBoxSizer *item6 = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText = new wxStaticText(this, -1, _("New Peak Amplitude (dB):"),
												wxDefaultPosition, wxDefaultSize, 0);
   item6->Add(pStaticText, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl *item8 = new wxTextCtrl(this, ID_PEAK_TEXT, "", 
													wxDefaultPosition, wxSize(60, -1), 0);
   item6->Add(item8, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   pBoxSizer_Dialog->Add(item6, 0, wxALIGN_CENTER | wxALL, 5);

	// "Don't allow clipping" checkbox
   wxCheckBox *item8b = new wxCheckBox(this, ID_CLIP_CHECKBOX,
													_("Allow clipping"),
													wxDefaultPosition, wxDefaultSize, 0);
   item8b->SetValue(false);

   pBoxSizer_Dialog->Add(item8b, 0, wxALIGN_CENTER | wxALL, 5);


	// Preview, OK, & Cancel buttons
   pBoxSizer_Dialog->Add(0, 10, 0); // spacer

   wxBoxSizer * pBoxSizer_OK = new wxBoxSizer(wxHORIZONTAL);

   wxButton * pButton_Preview = 
		new wxButton(this, ID_BUTTON_PREVIEW, m_pEffect->GetPreviewName());
   pBoxSizer_OK->Add(pButton_Preview, 0, wxALIGN_CENTER | wxALL, 5);
   pBoxSizer_OK->Add(25, 8); // horizontal spacer

   wxButton *item11 =
       new wxButton(this, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   pBoxSizer_OK->Add(item11, 0, wxALIGN_CENTER | wxALL, 5);

   wxButton *item10 =
       new wxButton(this, wxID_OK, _("OK"), wxDefaultPosition, wxDefaultSize, 0);
   item10->SetDefault();
   item10->SetFocus();
   pBoxSizer_OK->Add(item10, 0, wxALIGN_CENTER | wxALL, 5);

   pBoxSizer_Dialog->Add(pBoxSizer_OK, 0, wxALIGN_CENTER | wxALL, 5);

	// These bools are from the old MakeAmplifyDialog, now merged into this constructor.
	bool call_fit = true;
	bool set_sizer = true;
   if (set_sizer) {
      this->SetAutoLayout(TRUE);
      this->SetSizer(pBoxSizer_Dialog);
      if (call_fit) {
         pBoxSizer_Dialog->Fit(this);
         pBoxSizer_Dialog->SetSizeHints(this);
      }
   }
}

bool AmplifyDialog::Validate()
{
   return TRUE;
}

bool AmplifyDialog::TransferDataToWindow()
{
   wxSlider *slider;

   slider = GetAmpSlider();
   if (slider)
      slider->SetValue((int)(200*log10(ratio)+0.5));

   mLoopDetect = true;

   wxTextCtrl *text = GetAmpText();
   if (text) {
      wxString str;
      str.Printf("%.1f", 20*log10(ratio));
      text->SetValue(str);
   }

   text = GetPeakText();
   if (text) {
      wxString str;
      str.Printf("%.1f", 20*log10(ratio*peak));
      text->SetValue(str);
   }

   mLoopDetect = false;

   return TRUE;
}

bool AmplifyDialog::TransferDataFromWindow()
{
   wxTextCtrl *c = GetAmpText();
   if (c) {
      double r = 0;

      wxString val = c->GetValue();
      val.ToDouble(&r);
      ratio = pow(10.0,TrapDouble(r*10, AMP_MIN, AMP_MAX)/200.0);
   }

   noclip = !GetClipCheckBox()->GetValue();

   return TRUE;
}

// WDR: handler implementations for AmplifyDialog

void AmplifyDialog::OnAmpText(wxCommandEvent & event)
{
   if (mLoopDetect)
      return;

   wxTextCtrl *c = GetAmpText();
   if (c) {
      double r;

      mLoopDetect = true;

      wxString val = c->GetValue();
      val.ToDouble(&r);
      ratio = pow(10.0,TrapDouble(r*10, AMP_MIN, AMP_MAX)/200.0);

      wxSlider *slider = GetAmpSlider();
      if (slider)
         slider->SetValue((int)(200*log10(ratio)+0.5));

      wxString str;
      str.Printf("%.1f", 20*log10(ratio*peak));
      GetPeakText()->SetValue(str);

      mLoopDetect = false;
   }
   
   CheckClip();
}

void AmplifyDialog::OnPeakText(wxCommandEvent & event)
{
   if (mLoopDetect)
      return;

   wxTextCtrl *c = GetPeakText();
   if (c) {
      double r;

      mLoopDetect = true;

      wxString val = c->GetValue();
      val.ToDouble(&r);

      ratio = pow(10.0, r/20.0) / peak;
      
      double dB = TrapDouble(200*log10(ratio), AMP_MIN, AMP_MAX)/10.0;
      ratio = pow(10.0, dB/20.0);

      wxSlider *slider = GetAmpSlider();
      if (slider)
         slider->SetValue((int)(10*dB+0.5));
      
      wxString str;
      str.Printf("%.1f", dB);
      GetAmpText()->SetValue(str);
      
      mLoopDetect = false;
   }
   
   CheckClip();
}

void AmplifyDialog::OnAmpSlider(wxCommandEvent & event)
{
   if (mLoopDetect)
      return;

   mLoopDetect = true;

   wxString str;
   double dB = GetAmpSlider()->GetValue() / 10.0;
   ratio = pow(10.0,TrapDouble(dB, AMP_MIN, AMP_MAX)/20.0);
   
   double dB2 = (GetAmpSlider()->GetValue()-1) / 10.0;
   double ratio2 = pow(10.0,TrapDouble(dB2, AMP_MIN, AMP_MAX)/20.0);

   if (!GetClipCheckBox()->GetValue() &&
       ratio * peak > 1.0 &&
       ratio2 * peak < 1.0)
      ratio = 1.0 / peak;
   
   str.Printf("%.1f", 20*log10(ratio));
   GetAmpText()->SetValue(str);
   str.Printf("%.1f", 20*log10(ratio*peak));
   GetPeakText()->SetValue(str);

   mLoopDetect = false;

   CheckClip();
}

void AmplifyDialog::OnClipCheckBox(wxCommandEvent & event)
{
   CheckClip();
}

void AmplifyDialog::CheckClip()
{
   if (!GetClipCheckBox()->GetValue() == true) {
      GetOK()->Enable(ratio * peak <= 1.0);
   }
   else {
      GetOK()->Enable(true);
   }
}

void AmplifyDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

	// Save & restore parameters around Preview, because we didn't do OK.
	float oldRatio = m_pEffect->ratio;
	float oldPeak = m_pEffect->peak;

   m_pEffect->ratio = ratio;
   if (noclip && ratio*peak > 1.0)
      m_pEffect->ratio = 1.0 / peak;
   m_pEffect->peak = peak;

   m_pEffect->Preview();

   m_pEffect->ratio = oldRatio;
   m_pEffect->peak = oldPeak;
}

void AmplifyDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();
   
   if (!GetClipCheckBox()->GetValue() == true) {
     if (ratio * peak > 1.0)
        ratio = 1.0 / peak;
   }

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void AmplifyDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

