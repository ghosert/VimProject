/**********************************************************************

  Audacity: A Digital Audio Editor

  ToneGen.cpp

  Steve Jolly

  This class implements a tone generator effect.

**********************************************************************/

#include <wx/wxprec.h> 
#include "ToneGen.h"
#include "../WaveTrack.h"
#include "../Internat.h"

//
// EffectToneGen
//

EffectToneGen::EffectToneGen()
{
   frequency = float(440.0);          //Hz
   waveform = 0;                //sine
   amplitude = float(1.0);
   length = sDefaultGenerateLen;
}

wxString EffectToneGen::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   const char* waveformNames[] = {"sine", "square", "sawtooth"};
   return wxString::Format(_("Applied effect: Generate %s %s wave, frequency = %.2f Hz, amplitude = %.2f, %.6lf seconds"), 
                           (const char *)(this->GetEffectName()), 
                           waveformNames[waveform], frequency, amplitude, length); 
} 

bool EffectToneGen::PromptUser()
{
   if (mT1 > mT0)
      length = mT1 - mT0;

   ToneGenDialog dlog(mParent, -1, _("Tone Generator"));
   dlog.frequency = frequency;
   dlog.waveform = waveform;
   dlog.amplitude = amplitude;
   dlog.length = length;
   dlog.GetWaveformChoice()->Append(_("Sine"));
   dlog.GetWaveformChoice()->Append(_("Square"));
   dlog.GetWaveformChoice()->Append(_("Sawtooth"));
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == 0)
      return false;

   frequency = dlog.frequency;
   waveform = dlog.waveform;
   amplitude = dlog.amplitude;
   length = dlog.length;

   return true;
}

bool EffectToneGen::MakeTone(float *buffer, sampleCount len)
{
   double throwaway = 0;        //passed to modf but never used
   int i;

   switch (waveform) {
   case 0:                     //sine
      for (i = 0; i < len; i++)
         buffer[i] =
             amplitude * (float) sin(2 * M_PI * (i + mSample) * frequency / mCurRate);
      mSample += len;
      break;

   case 1:                     //square
      for (i = 0; i < len; i++) {
         if (modf(((i + mSample) * frequency / mCurRate), &throwaway) < 0.5)
            buffer[i] = amplitude;
         else
            buffer[i] = -amplitude;
      }
      mSample += len;
      break;

   case 2:                     //sawtooth
      for (i = 0; i < len; i++)
         buffer[i] = 2 * amplitude * ((((i + mSample) % (int) (mCurRate / frequency)) / (mCurRate / frequency)) - 0.5);
      mSample += len;
      break;

   default:
      break;
   }
   return true;
}

bool EffectToneGen::Process()
{
   if (length <= 0.0)
      length = sDefaultGenerateLen;

   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *)iter.First();
   while (track) {
      mSample = 0;
      WaveTrack *tmp = mFactory->NewWaveTrack(track->GetSampleFormat());
      mCurRate = track->GetRate();
      tmp->SetRate(mCurRate);
      longSampleCount numSamples =
         (longSampleCount)(length * mCurRate + 0.5);
      longSampleCount i = 0;
      float *data = new float[tmp->GetMaxBlockSize()];
      sampleCount block;

      while(i < numSamples) {
         block = tmp->GetBestBlockSize(i);
         if (block > (numSamples - i))
             block = numSamples - i;
         MakeTone(data, block);
         tmp->Append((samplePtr)data, floatSample, block);
         i += block;
      }
      delete[] data;

      tmp->Flush();
      track->Clear(mT0, mT1);
      track->Paste(mT0, tmp);
      delete tmp;
      
      //Iterate to the next track
      track = (WaveTrack *)iter.Next();
   }

	mT1 = mT0 + length; // Update selection.

   return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// ToneGenDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define FREQ_MAX 20000
#define AMP_MIN 0
#define AMP_MAX 1
#define WAVEFORM_MIN 0
#define WAVEFORM_MAX 2

BEGIN_EVENT_TABLE(ToneGenDialog, wxDialog)
   EVT_BUTTON(wxID_OK, ToneGenDialog::OnCreateTone)
   EVT_BUTTON(wxID_CANCEL, ToneGenDialog::OnCancel)
END_EVENT_TABLE()

ToneGenDialog::ToneGenDialog(wxWindow * parent, wxWindowID id, const wxString & title, const wxPoint & position, const wxSize & size, long style):
wxDialog(parent, id, title, position, size, style)
{
   CreateToneGenDialog(this, TRUE);
}

bool ToneGenDialog::Validate()
{
   return TRUE;
}

bool ToneGenDialog::TransferDataToWindow()
{
   wxChoice *choice;
   wxTextCtrl *text;

   choice = GetWaveformChoice();
   if (choice)
      choice->SetSelection(waveform);

   text = GetFreqText();
   if (text) {
      wxString str;
      str.Printf("%.2f", frequency);
      text->SetValue(str);
   }

   text = GetAmpText();
   if (text) {
      wxString str;
      str.Printf("%.2f", amplitude);
      text->SetValue(str);
   }

   text = GetLengthText();
   if (text) {
      wxString str;
      str.Printf("%.6lf", length);
      text->SetValue(str);
   }

   return TRUE;
}

bool ToneGenDialog::TransferDataFromWindow()
{
   wxTextCtrl *t;

   t = GetLengthText();
   if (t) {
      t->GetValue().ToDouble(&length);
   }

   t = GetAmpText();
   if (t) {
      double d;
      t->GetValue().ToDouble(&d);
      amplitude = TrapDouble(d, AMP_MIN, AMP_MAX);
   }

   t = GetFreqText();
   if (t) {
      double d;
      t->GetValue().ToDouble(&d);
      frequency = TrapDouble(d, FREQ_MIN, FREQ_MAX);
   }

   wxChoice *c = GetWaveformChoice();
   if (c)
      waveform = TrapLong(c->GetSelection(), WAVEFORM_MIN, WAVEFORM_MAX);

   return TRUE;
}

// WDR: handler implementations for ToneGenDialog

void ToneGenDialog::OnCreateTone(wxCommandEvent & event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void ToneGenDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

wxSizer *CreateToneGenDialog(wxWindow * parent, bool call_fit,
                             bool set_sizer)
{
   wxBoxSizer *item0 = new wxBoxSizer(wxVERTICAL);


   wxBoxSizer *item2 = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item3 =
       new wxStaticText(parent, ID_TEXT, _("Waveform:"), wxDefaultPosition,
                        wxDefaultSize, 0);
   item2->Add(item3, 0, wxALIGN_CENTRE | wxALL, 5);

   wxChoice *item4 = new wxChoice(parent, ID_WAVEFORM, wxDefaultPosition,
                                  wxSize(80, -1), 0, (const wxString *)NULL);
   	/* cast is needed on NetBSD/amd64. Seems to be fine elsewhere */
   item2->Add(item4, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item2, 1, wxALIGN_CENTRE | wxALL, 5);


   wxBoxSizer *item5 = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item6 =
       new wxStaticText(parent, ID_TEXT, _("Frequency / Hz"),
                        wxDefaultPosition,
                        wxDefaultSize, 0);
   item5->Add(item6, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item7 =
       new wxTextCtrl(parent, ID_FREQTEXT, "", wxDefaultPosition,
                      wxSize(60, -1), 0);
   item5->Add(item7, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item5, 1, wxALIGN_CENTRE | wxALL, 5);


   wxBoxSizer *item8 = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item9 =
       new wxStaticText(parent, ID_TEXT, _("Amplitude (0-1)"),
                        wxDefaultPosition,
                        wxDefaultSize, 0);
   item8->Add(item9, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item10 =
       new wxTextCtrl(parent, ID_AMPTEXT, "", wxDefaultPosition,
                      wxSize(60, -1), 0);
   item8->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item8, 1, wxALIGN_CENTRE | wxALL, 5);

   item8 = new wxBoxSizer(wxHORIZONTAL);
   item9 = new wxStaticText(parent, ID_TEXT, _("Length (seconds)"),
                        wxDefaultPosition,
                        wxDefaultSize, 0);
   item8->Add(item9, 0, wxALIGN_CENTRE | wxALL, 5);

   item10 = new wxTextCtrl(parent, ID_LENGTHTEXT, "", wxDefaultPosition,
                      wxSize(120, -1), 0);
   item8->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item8, 1, wxALIGN_CENTRE | wxALL, 5);

   wxBoxSizer *item11 = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item13 =
       new wxButton(parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item11->Add(item13, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item12 =
       new wxButton(parent, wxID_OK, _("Generate Tone"), wxDefaultPosition,
                    wxDefaultSize, 0);
   item12->SetDefault();
   item12->SetFocus();
   item11->Add(item12, 0, wxALIGN_CENTRE | wxALL, 5);

   item0->Add(item11, 0, wxALIGN_CENTRE | wxALL, 5);

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
