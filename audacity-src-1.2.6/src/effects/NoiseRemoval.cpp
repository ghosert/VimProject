/**********************************************************************

  Audacity: A Digital Audio Editor

  NoiseRemoval.cpp

  Dominic Mazzoni

  The noise is removed using noise gates on each frequency band in
  the FFT, and the signal is reconstructed using overlap/add of
  Hanning windows.

**********************************************************************/

#include "../Audacity.h"

#include <math.h>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

#ifdef __MACOSX__
#include <math.h>
#endif

#ifdef __MACOS9__
#include <fp.h>
#define finite(x) isfinite(x)
#endif

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/statbox.h>
#include <wx/intl.h>

#include "NoiseRemoval.h"
#include "../Envelope.h"
#include "../FFT.h"
#include "../WaveTrack.h"

EffectNoiseRemoval::EffectNoiseRemoval()
{
   windowSize = 2048;
   noiseGate = new float[windowSize];
   sum = new float[windowSize];
   sumsq = new float[windowSize];
   profileCount = new int[windowSize];
   smoothing = new float[windowSize];
   hasProfile = false;
   level = 8;
}

EffectNoiseRemoval::~EffectNoiseRemoval()
{
   delete [] noiseGate;
   delete [] sum;
   delete [] sumsq;
   delete [] profileCount;
   delete [] smoothing;
}

bool EffectNoiseRemoval::PromptUser()
{
   NoiseRemovalDialog dlog(this, mParent, -1, _("Noise Removal"));
   if (hasProfile) {
      dlog.m_pSlider->SetValue(level);
		dlog.m_pButton_RemoveNoise->SetDefault();
		dlog.m_pButton_RemoveNoise->SetFocus();
	} else {
      dlog.m_pSlider->Enable(false);
      dlog.m_pButton_Preview->Enable(false);
      dlog.m_pButton_RemoveNoise->Enable(false);
   }
   dlog.CentreOnParent();
   dlog.ShowModal();
   
   if (dlog.GetReturnCode() == 0)
      return false;

   level = dlog.m_pSlider->GetValue();


   if (dlog.GetReturnCode() == 1)
      doProfile = true;
   else
      doProfile = false;
   
   return true;
}

bool EffectNoiseRemoval::Process()
{
   if (doProfile) {
      for(int i=0; i<windowSize; i++) {
         sum[i] = float(0.0);
         sumsq[i] = float(0.0);
         profileCount[i] = 0;
      }
   }

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

   if (doProfile) {
      for(int i=0; i<=windowSize/2; i++) {
         //float stddev = sqrt(sumsq[i] - (sum[i]*sum[i])/profileCount[i])
         //                               / profileCount[i];
         noiseGate[i] = sum[i] / profileCount[i]; // average
      }
      
      hasProfile = true;
   }

   return true;
}

bool EffectNoiseRemoval::ProcessOne(int count, WaveTrack * track,
                                    longSampleCount start, sampleCount len)
{
   sampleCount s = 0;
   sampleCount idealBlockLen = track->GetMaxBlockSize() * 4;

   if (idealBlockLen % windowSize != 0)
      idealBlockLen += (windowSize - (idealBlockLen % windowSize));
   
   float *buffer = new float[idealBlockLen];
   
   float *window1 = new float[windowSize];
   float *window2 = new float[windowSize];
   float *thisWindow = window1;
   float *lastWindow = window2;
   
   int i;
   
   for(i=0; i<windowSize; i++) {
      lastWindow[i] = 0;
      smoothing[i] = float(0.0);
   }
   
   while((s < len)&&((len-s)>(windowSize/2))) {
      sampleCount block = idealBlockLen;
      if (s + block > len)
         block = len - s;
      
      track->Get((samplePtr) buffer, floatSample, start + s, block);
      
      for(i=0; i<(block-windowSize/2); i+=windowSize/2) {
         int wcopy = windowSize;
         if (i + wcopy > block)
            wcopy = block - i;
         
         int j;
         for(j=0; j<wcopy; j++)
            thisWindow[j] = buffer[i+j];
         for(j=wcopy; j<windowSize; j++)
            thisWindow[j] = 0;
         
         if (doProfile)
            GetProfile(windowSize, thisWindow);
         else {
            RemoveNoise(windowSize, thisWindow);
            for(j=0; j<windowSize/2; j++)
               buffer[i+j] = thisWindow[j] + lastWindow[windowSize/2 + j];
         }
         
         float *tempP = thisWindow;
         thisWindow = lastWindow;
         lastWindow = tempP;
      }
      
      // Shift by half-a-window less than the block size we loaded
      // (so that the blocks properly overlap)
      block -= windowSize/2;

      if (!doProfile)
         track->Set((samplePtr) buffer, floatSample, start + s, block);
      
      s += block;
      
      if (TrackProgress(count, s / (double) len))
         return false;
   }
   
   delete[] buffer;
   delete[] window1;
   delete[] window2;
   
   return true;
}

void EffectNoiseRemoval::GetProfile(sampleCount len,
                                    float *buffer)
{
   float *in = new float[len];
   float *out = new float[len];
   
   int i;
   
   for(i=0; i<len; i++)
      in[i] = buffer[i];

   // Apply window and FFT
   /* WindowFunc(3, len, in); // Hanning window */
   PowerSpectrum(len, in, out);
   
   for(i=0; i<=len/2; i++) {
      float value = log(out[i]);
      
      if (finite(value)) {
         sum[i] += value;
         sumsq[i] += value*value;
         profileCount[i]++;
      }
   }

   delete[] in;
   delete[] out;
}

void EffectNoiseRemoval::RemoveNoise(sampleCount len,
                                     float *buffer)
{
   float *inr = new float[len];
   float *ini = new float[len];
   float *outr = new float[len];
   float *outi = new float[len];
   float *power = new float[len];
   float *plog = new float[len];
   
   int i;
   
   for(i=0; i<len; i++)
      inr[i] = buffer[i];

   // Apply window and FFT
   /* WindowFunc(3, len, inr); // Hanning window */
   FFT(len, false, inr, NULL, outr, outi);

   for(i=0; i<len; i++)
      inr[i] = buffer[i];
   WindowFunc(3, len, inr); // Hanning window
   PowerSpectrum(len, inr, power);

   for(i=0; i<=len/2; i++)
      plog[i] = log(power[i]);
    
   int half = len/2;
   for(i=0; i<=half; i++) {
      float smooth;
      
      if (plog[i] < noiseGate[i] + (level/2.0))
         smooth = float(0.0);
      else
         smooth = float(1.0);
      
      smoothing[i] = smooth * 0.5 + smoothing[i] * 0.5;
   }

   /* try to eliminate tinkle bells */
   for(i=2; i<=half-2; i++) {
      if (smoothing[i]>=0.5 &&
          smoothing[i]<=0.55 &&
          smoothing[i-1]<0.1 &&
          smoothing[i-2]<0.1 &&
          smoothing[i+1]<0.1 &&
          smoothing[i+2]<0.1)
          smoothing[i] = float(0.0);
   }

   outr[0] *= smoothing[0];
   outi[0] *= smoothing[0];
   outr[half] *= smoothing[half];
   outi[half] *= smoothing[half];
   for(i=1; i<half; i++) {
      int j = len - i;
      float smooth = smoothing[i];

      outr[i] *= smooth;
      outi[i] *= smooth;
      outr[j] *= smooth;
      outi[j] *= smooth;
   }

   // Inverse FFT and normalization
   FFT(len, true, outr, outi, inr, ini);
   WindowFunc(3, len, inr); // Hanning window */
   
   for(i=0; i<len; i++)
      buffer[i] = inr[i];

   delete[] inr;
   delete[] ini;
   delete[] outr;
   delete[] outi;
   delete[] power;
   delete[] plog;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// NoiseRemovalDialog
//----------------------------------------------------------------------------

// WDR: event table for NoiseRemovalDialog

enum {
   ID_BUTTON_GETPROFILE = 10001,
	ID_BUTTON_PREVIEW
};

BEGIN_EVENT_TABLE(NoiseRemovalDialog,wxDialog)
	EVT_BUTTON(wxID_OK, NoiseRemovalDialog::OnRemoveNoise)
	EVT_BUTTON(wxID_CANCEL, NoiseRemovalDialog::OnCancel)
	EVT_BUTTON(ID_BUTTON_GETPROFILE, NoiseRemovalDialog::OnGetProfile)
	EVT_BUTTON(ID_BUTTON_PREVIEW, NoiseRemovalDialog::OnPreview)
END_EVENT_TABLE()

NoiseRemovalDialog::NoiseRemovalDialog(EffectNoiseRemoval * effect, 
													wxWindow *parent, wxWindowID id,
													const wxString &title,
													const wxPoint &position, 
													const wxSize& size,
													long style ) :
   wxDialog( parent, id, title, position, size, style )
{
	m_pEffect = effect;
   
	// NULL out the control members until the controls are created.
	m_pButton_GetProfile = NULL;
	m_pSlider = NULL;
	m_pButton_Preview = NULL;
	m_pButton_RemoveNoise = NULL;

   this->MakeNoiseRemovalDialog(true); 
}


// WDR: handler implementations for NoiseRemovalDialog

void NoiseRemovalDialog::OnGetProfile( wxCommandEvent &event )
{
   EndModal(1);
}

void NoiseRemovalDialog::OnPreview(wxCommandEvent &event)
{
	// Save & restore parameters around Preview, because we didn't do OK.
   bool oldDoProfile = m_pEffect->doProfile;
	int oldLevel = m_pEffect->level;

	m_pEffect->doProfile = false;
	m_pEffect->level = m_pSlider->GetValue();
   
	m_pEffect->Preview();
   
	m_pEffect->doProfile = oldDoProfile;
	m_pEffect->level = oldLevel; 
}

void NoiseRemovalDialog::OnRemoveNoise( wxCommandEvent &event )
{
   EndModal(2);
}

void NoiseRemovalDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(0);
}

wxSizer *NoiseRemovalDialog::MakeNoiseRemovalDialog(bool call_fit /* = true */, 
																	 bool set_sizer /* = true */)
{
   wxBoxSizer *mainSizer = new wxBoxSizer( wxVERTICAL );
   wxStaticBoxSizer *group;
   wxControl *item;
   
   item = new wxStaticText(this, -1,
                   _("Noise Removal by Dominic Mazzoni"), wxDefaultPosition,
                   wxDefaultSize, wxALIGN_CENTRE );
   mainSizer->Add(item, 0, wxALIGN_CENTRE|wxALL, 5);

   // Step 1
   
   group = new wxStaticBoxSizer(new wxStaticBox(this, -1,
                                                _("Step 1")), wxVERTICAL);

   item = new wxStaticText(this, -1,
                           _("Select a few seconds of just noise\n"
                             "so Audacity knows what to filter out, then\n"
                             "click Get Noise Profile:"),
                           wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
   group->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   m_pButton_GetProfile = new wxButton(this, ID_BUTTON_GETPROFILE, _("Get Noise Profile"), wxDefaultPosition, wxDefaultSize, 0 );
   group->Add(m_pButton_GetProfile, 0, wxALIGN_CENTRE|wxALL, 5 );

   mainSizer->Add( group, 0, wxALIGN_CENTRE|wxALL, 5 );
   
   // Step 2
   
   group = new wxStaticBoxSizer(new wxStaticBox(this, -1,
                                                _("Step 2")), wxVERTICAL);

   item = new wxStaticText(this, -1,
                           _("Select all of the audio you want filtered,\n"
                             "choose how much noise you want filtered out,\n"
                             "and then click Remove Noise.\n"),
                           wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
   group->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   m_pSlider = new wxSlider(this, -1, 8, 1, 15,
										wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL);
   group->Add(m_pSlider, 1, wxEXPAND|wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxTOP, 5 );

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);
   item = new wxStaticText(this, -1, _("Less"));
   hSizer->Add(item, 0, wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxBOTTOM, 5 );   
   hSizer->Add(10, 10, 1, wxALIGN_CENTRE | wxLEFT | wxRIGHT | wxBOTTOM, 5);
   item = new wxStaticText(this, -1, _("More"));
   hSizer->Add(item, 0, wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxBOTTOM, 5 );
   group->Add(hSizer, 1, wxEXPAND|wxALIGN_CENTRE|wxALL, 5 );
   
   hSizer = new wxBoxSizer(wxHORIZONTAL);

   m_pButton_Preview = new wxButton(this, ID_BUTTON_PREVIEW, m_pEffect->GetPreviewName());
   hSizer->Add(m_pButton_Preview, 0, wxALIGN_LEFT | wxALL, 5);
   
   hSizer->Add(25, 5); // horizontal spacer

	m_pButton_RemoveNoise = new wxButton(this, wxID_OK, _("Remove Noise"), wxDefaultPosition, wxDefaultSize, 0 );
   hSizer->Add(m_pButton_RemoveNoise, 0, wxALIGN_RIGHT | wxALL, 5 );
   
	group->Add(hSizer, 0, wxALIGN_CENTER | wxALL, 5 );

   mainSizer->Add( group, 0, wxALIGN_CENTRE|wxALL, 5 );
   

   item = new wxButton( this, wxID_CANCEL, _("Close"), wxDefaultPosition, wxDefaultSize, 0 );
   mainSizer->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   if (set_sizer) {
      this->SetAutoLayout( TRUE );
      this->SetSizer( mainSizer );
      if (call_fit) {
         mainSizer->Fit( this );
         mainSizer->SetSizeHints( this );
      }
   }
    
   return mainSizer;
}
