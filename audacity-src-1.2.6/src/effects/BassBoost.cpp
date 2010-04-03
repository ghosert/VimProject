/**********************************************************************

  Audacity: A Digital Audio Editor

  BassBoost.cpp

  Effect programming:
  Nasca Octavian Paul

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

**********************************************************************/

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/intl.h>

#include "BassBoost.h"
#include "../WaveTrack.h"

//
// EffectBassBoost
//

EffectBassBoost::EffectBassBoost()
{
   frequency = 200;
   dB_boost = 12;
}

wxString EffectBassBoost::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   return wxString::Format(_("Applied effect: %s frequency = %.0f Hz, boost = %.0f dB"), 
                           (const char *)(this->GetEffectName()), frequency, dB_boost); 
} 

bool EffectBassBoost::NewTrackSimpleMono()
{
//(re)initialise filter parameters
   xn1=0;
   xn2=0;
   yn1=0;
   yn2=0;

   /* Compute coefficents of the biquand IIR filter */
   omega = 2 * 3.141592653589 * frequency / mCurRate;
   sn = sin(omega);
   cs = cos(omega);
   a = exp(log(10.0) * dB_boost / 40);
   shape = float(1.0);           /*Low Shelf filter's shape, if this is too large
                            or too small it will result an unstable filter */
   beta = sqrt((a * a + 1) / shape - (pow((a - 1), 2)));
   /*  Coefficients  */
   b0 = a * ((a + 1) - (a - 1) * cs + beta * sn);
   b1 = 2 * a * ((a - 1) - (a + 1) * cs);
   b2 = a * ((a + 1) - (a - 1) * cs - beta * sn);
   a0 = ((a + 1) + (a - 1) * cs + beta * sn);
   a1 = -2 * ((a - 1) + (a + 1) * cs);
   a2 = (a + 1) + (a - 1) * cs - beta * sn;

   return true;
}

bool EffectBassBoost::PromptUser()
{
   BassBoostDialog dlog(this, mParent, -1, _("Bass Boost"));
   dlog.freq = frequency;
   dlog.boost = dB_boost;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   frequency = dlog.freq;
   dB_boost = dlog.boost;

   return true;
}

bool EffectBassBoost::ProcessSimpleMono(float *buffer, sampleCount len)
{
   /* initialise the filter */

   float out, in = 0;

   for (int i = 0; i < len; i++) {
      in = buffer[i];
      out = (b0 * in + b1 * xn1 + b2 * xn2 - a1 * yn1 - a2 * yn2) / a0;
      xn2 = xn1;
      xn1 = in;
      yn2 = yn1;
      yn1 = out;

      if (out < -1.0)
         out = float(-1.0);
      else if (out > 1.0)
         out = float(1.0);        //Prevents clipping

      buffer[i] = out;
   }

   return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// BassBoostDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define FREQ_MAX 1000
#define BOOST_MIN 0
#define BOOST_MAX 36

BEGIN_EVENT_TABLE(BassBoostDialog, wxDialog)
    EVT_BUTTON(ID_BUTTON_PREVIEW, BassBoostDialog::OnPreview)
    EVT_BUTTON(wxID_OK, BassBoostDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, BassBoostDialog::OnCancel)
    EVT_SLIDER(ID_FREQ_SLIDER, BassBoostDialog::OnFreqSlider)
    EVT_SLIDER(ID_BOOST_SLIDER, BassBoostDialog::OnBoostSlider)
    EVT_TEXT(ID_FREQ_TEXT, BassBoostDialog::OnFreqText)
    EVT_TEXT(ID_BOOST_TEXT, BassBoostDialog::OnBoostText)
END_EVENT_TABLE()

BassBoostDialog::BassBoostDialog(EffectBassBoost *effect,
                                 wxWindow * parent, wxWindowID id,
                                 const wxString & title,
                                 const wxPoint & position,
                                 const wxSize & size,
                                 long style):
   wxDialog(parent, id, title, position, size, style),
   mEffect(effect)
{
   MakeBassBoostDialog(this, TRUE, TRUE);
}

bool BassBoostDialog::Validate()
{
   return TRUE;
}

bool BassBoostDialog::TransferDataToWindow()
{
   wxSlider *slider;

   slider = GetBoostSlider();
   if (slider)
      slider->SetValue((int)boost);

   slider = GetFreqSlider();
   if (slider)
      slider->SetValue((int)freq);

   wxTextCtrl *text = GetBoostText();
   if (text) {
      wxString str;
      str.Printf("%d", (int) boost);
      text->SetValue(str);
   }

   text = GetFreqText();
   if (text) {
      wxString str;
      str.Printf("%d", (int) freq);
      text->SetValue(str);
   }

   return TRUE;
}

bool BassBoostDialog::TransferDataFromWindow()
{
   wxTextCtrl *c = GetBoostText();
   if (c) {
      long b;

      wxString val = c->GetValue();
      val.ToLong(&b);
      boost = TrapLong(b, BOOST_MIN, BOOST_MAX);
   }

   c = GetFreqText();
   if (c) {
      long f;

      wxString val = c->GetValue();
      val.ToLong(&f);
      freq = TrapLong(f, FREQ_MIN, FREQ_MAX);
   }

   return TRUE;
}

// WDR: handler implementations for BassBoostDialog

void BassBoostDialog::OnBoostText(wxCommandEvent & event)
{
   wxTextCtrl *c = GetBoostText();
   if (c) {
      long boost;

      c->GetValue().ToLong(&boost);
      boost = TrapLong(boost, BOOST_MIN, BOOST_MAX);

      wxSlider *slider = GetBoostSlider();
      if (slider)
         slider->SetValue(boost);
   }
}

void BassBoostDialog::OnFreqText(wxCommandEvent & event)
{
   wxTextCtrl *c = GetFreqText();
   if (c) {
      long freq;

      c->GetValue().ToLong(&freq);
      freq = TrapLong(freq, FREQ_MIN, FREQ_MAX);

      wxSlider *slider = GetFreqSlider();
      if (slider)
         slider->SetValue(freq);
   }
}

void BassBoostDialog::OnBoostSlider(wxCommandEvent & event)
{
   wxString str;
   str.Printf("%d", GetBoostSlider()->GetValue());
   GetBoostText()->SetValue(str);
}

void BassBoostDialog::OnFreqSlider(wxCommandEvent & event)
{
   wxString str;
   long freq = GetFreqSlider()->GetValue();
   freq = ((freq + 5) / 10) * 10;       // round to nearest multiple of 10
   str.Printf("%ld", freq);
   GetFreqText()->SetValue(str);
}

void BassBoostDialog::OnPreview(wxCommandEvent & event)
{
   TransferDataFromWindow();
   mEffect->frequency = freq;
   mEffect->dB_boost = boost;
   mEffect->Preview();
}

void BassBoostDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void BassBoostDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

wxSizer *BassBoostDialog::MakeBassBoostDialog(wxWindow * parent, bool call_fit,
                                              bool set_sizer)
{
   wxBoxSizer *item0 = new wxBoxSizer(wxVERTICAL);

   wxStaticText *item1 =
       new wxStaticText(parent, -1,
                        _("Bass Boost by Nasca Octavian Paul"),
                        wxDefaultPosition, wxDefaultSize, 0);
   item0->Add(item1, 0, wxALIGN_CENTRE | wxALL, 5);

   wxFlexGridSizer *item2 = new wxFlexGridSizer(3, 0, 0);

   wxStaticText *item3 =
       new wxStaticText(parent, -1, _("Frequency (Hz):"),
                        wxDefaultPosition, wxDefaultSize, 0);
   item2->Add(item3, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item4 =
       new wxTextCtrl(parent, ID_FREQ_TEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   item2->Add(item4, 0, wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item5 =
       new wxSlider(parent, ID_FREQ_SLIDER, 0, FREQ_MIN, FREQ_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   //item5->SetValue(200);
   item2->Add(item5, 0, wxALIGN_CENTRE | wxALL, 5);

   wxStaticText *item6 =
       new wxStaticText(parent, -1, _("Boost (dB):"), wxDefaultPosition,
                        wxDefaultSize, 0);
   item2->Add(item6, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item7 =
       new wxTextCtrl(parent, ID_BOOST_TEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   item2->Add(item7, 0, wxALIGN_CENTRE | wxALL, 5);

   wxSlider *item8 =
       new wxSlider(parent, ID_BOOST_SLIDER, 0, BOOST_MIN, BOOST_MAX,
                    wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   //item8->SetValue(12);
   item2->Add(item8, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item2, 0, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *item9 = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item9b =
      new wxButton(parent, ID_BUTTON_PREVIEW, mEffect->GetPreviewName());
   item9->Add(item9b, 0, wxALIGN_CENTRE | wxALL, 5);
   item9->Add(20, 5); // horizontal spacer

   wxButton *item11 =
       new wxButton(parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item9->Add(item11, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item10 =
       new wxButton(parent, wxID_OK, _("OK"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item10->SetDefault();
   item10->SetFocus();
   item9->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item9, 0, wxALIGN_CENTRE | wxALL, 5);

   if (set_sizer) {
      parent->SetAutoLayout(TRUE);
      parent->SetSizer(item0);
      if (call_fit) {
         item0->Fit(parent);
         item0->SetSizeHints(parent);
      }
   }

   return item0;
}
