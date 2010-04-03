/**********************************************************************

  Audacity: A Digital Audio Editor

  Normalize.cpp

  Dominic Mazzoni
  Vaughan Johnson (Preview)

**********************************************************************/

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textdlg.h>

#include "Normalize.h"
#include "../Audacity.h" // for rint from configwin.h
#include "../WaveTrack.h"

EffectNormalize::EffectNormalize()
{
   mGain = true;
   mDC = true;
}

bool EffectNormalize::PromptUser()
{
   NormalizeDialog dlog(this, mParent, -1, _("Normalize"));
   dlog.mGain = mGain;
   dlog.mDC = mDC;
   dlog.TransferDataToWindow();

   dlog.CentreOnParent();
   dlog.ShowModal();
   
   if (!dlog.GetReturnCode())
      return false;

   mGain = dlog.mGain;
   mDC = dlog.mDC;

   return true;
}

bool EffectNormalize::Process()
{
   if (mGain == false &&
       mDC == false)
      return true;

   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   mCurTrackNum = 0;
   while (track) {
      //Get start and end times from track
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      mCurT0 = mT0 < trackStart? trackStart: mT0;
      mCurT1 = mT1 > trackEnd? trackEnd: mT1;

      // Process only if the right marker is to the right of the left marker
      if (mCurT1 > mCurT0) {

         //Transform the marker timepoints to samples
         longSampleCount start = track->TimeToLongSamples(mCurT0);
         longSampleCount end = track->TimeToLongSamples(mCurT1);
         
         //Get the track rate and samples
         mCurRate = track->GetRate();
         mCurChannel = track->GetChannel();

         //ProcessOne() (implemented below) processes a single track
         if (!ProcessOne(track, start, end))
            return false;
      }
      
      //Iterate to the next track
      track = (WaveTrack *) iter.Next();
      mCurTrackNum++;
   }

   return true;
}

//ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
//and executes AnalyzeData, then ProcessData, on it...
bool EffectNormalize::ProcessOne(WaveTrack * track,
                                  longSampleCount start, longSampleCount end)
{
   
   longSampleCount s;
   //Get the length of the buffer (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later 
   double len = (double)(end - start);

   //Initiate a processing buffer.  This buffer will (most likely)
   //be shorter than the length of the track being processed.
   float *buffer = new float[track->GetMaxBlockSize()];

   int pass;

   for(pass=0; pass<2; pass++) {

      if (pass==0)
         StartAnalysis();
      if (pass==1)
         StartProcessing();

      //Go through the track one buffer at a time. s counts which
      //sample the current buffer starts at.
      s = start;
      while (s < end) {
         //Get a block of samples (smaller than the size of the buffer)
         sampleCount block = track->GetBestBlockSize(s);
         
         //Adjust the block size if it is the final block in the track
         if (s + block > end)
            block = end - s;
         
         //Get the samples from the track and put them in the buffer
         track->Get((samplePtr) buffer, floatSample, s, block);
         
         //Process the buffer.

         if (pass==0)
            AnalyzeData(buffer, block);

         if (pass==1) {
            ProcessData(buffer, block);
         
            //Copy the newly-changed samples back onto the track.
            track->Set((samplePtr) buffer, floatSample, s, block);
         }
            
         //Increment s one blockfull of samples
         s += block;
         
         //Update the Progress meter
			if (TrackProgress(mCurTrackNum, 
									((double)(pass)*0.5) + // Approximate each pass as half.
										((double)(s - start) / (len*2))))
            return false;
      }
   }

   //Clean up the buffer
   delete[] buffer;

   //Return true because the effect processing succeeded.
   return true;
}

void EffectNormalize::StartAnalysis()
{
   mMin = 1.0;
   mMax = -1.0;
   mSum = 0.0;
   mCount = 0;
}

void EffectNormalize::AnalyzeData(float *buffer, sampleCount len)
{
   int i;

   for(i=0; i<len; i++) {
      if (buffer[i] < mMin)
         mMin = buffer[i];
      if (buffer[i] > mMax)
         mMax = buffer[i];
      mSum += (double)buffer[i];
   }

   mCount += len;
}

void EffectNormalize::StartProcessing()
{
   mMult = 1.0;
   mOffset = 0.0;

   if (mDC) {
      mOffset = (float)(-mSum / mCount);
   }

   if (mGain) {
      float extent = fabs(mMax + mOffset);
      if (fabs(mMin + mOffset) > extent)
         extent = fabs(mMin + mOffset);

      if (extent > 0) {
         mMult = ((sqrt(2.0)/2) / extent);
      }
   }
}

void EffectNormalize::ProcessData(float *buffer, sampleCount len)
{
   int i;

   for(i=0; i<len; i++)
      buffer[i] = (buffer[i] + mOffset) * mMult;
}

//----------------------------------------------------------------------------
// NormalizeDialog
//----------------------------------------------------------------------------

#define ID_BUTTON_PREVIEW 10001

BEGIN_EVENT_TABLE(NormalizeDialog,wxDialog)
   EVT_BUTTON( wxID_OK, NormalizeDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, NormalizeDialog::OnCancel )
	EVT_BUTTON(ID_BUTTON_PREVIEW, NormalizeDialog::OnPreview)
END_EVENT_TABLE()

NormalizeDialog::NormalizeDialog(EffectNormalize *effect,
                                 wxWindow *parent, wxWindowID id,
                                 const wxString &title,
                                 const wxPoint &position, const wxSize& size,
                                 long style ) :
   wxDialog( parent, id, title, position, size, style ),
   mEffect(effect)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   mainSizer->Add(new wxStaticText(this, -1,
                                   _("Normalize by Dominic Mazzoni"),
                                   wxDefaultPosition, wxDefaultSize,
                                   wxALIGN_CENTRE),
                  0, wxALIGN_CENTRE|wxALL, 5);
   
   mDCCheckBox = new wxCheckBox(this, -1,
                                  _("Remove any DC offset (center on 0 vertically)"));
   mDCCheckBox->SetValue(true);
   mainSizer->Add(mDCCheckBox, 0, wxALIGN_LEFT|wxALL, 5);

   mGainCheckBox = new wxCheckBox(this, -1,
                                  _("Normalize maximum amplitude to -3 dB"));
   mGainCheckBox->SetValue(true);
   mainSizer->Add(mGainCheckBox, 0, wxALIGN_LEFT|wxALL, 5);

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton * pButton_Preview = 
		new wxButton(this, ID_BUTTON_PREVIEW, mEffect->GetPreviewName());
   hSizer->Add(pButton_Preview, 0, wxALIGN_CENTER | wxALL, 5);
   hSizer->Add(20, 10); // horizontal spacer

   wxButton *cancel = new wxButton(this, wxID_CANCEL, _("Cancel"));
   hSizer->Add(cancel, 0, wxALIGN_CENTRE|wxALL, 5);

   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   ok->SetDefault();
   ok->SetFocus();
   hSizer->Add(ok, 0, wxALIGN_CENTRE|wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

bool NormalizeDialog::TransferDataToWindow()
{
   mGainCheckBox->SetValue(mGain);
   mDCCheckBox->SetValue(mDC);

   TransferDataFromWindow();

   return true;
}

bool NormalizeDialog::TransferDataFromWindow()
{
   mGain = mGainCheckBox->GetValue();
   mDC = mDCCheckBox->GetValue();

   return true;
}

void NormalizeDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

	// Save & restore parameters around Preview, because we didn't do OK.
   bool oldGain = mEffect->mGain;
   bool oldDC = mEffect->mDC;

   mEffect->mGain = mGain;
   mEffect->mDC = mDC;
   
	mEffect->Preview();
   
	mEffect->mGain = oldGain;
   mEffect->mDC = oldDC;
}

void NormalizeDialog::OnOk(wxCommandEvent &event)
{
   TransferDataFromWindow();

   EndModal(true);
}

void NormalizeDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(false);
}
