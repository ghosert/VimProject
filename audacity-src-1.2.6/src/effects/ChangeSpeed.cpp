/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeSpeed.cpp

  Vaughan Johnson, Dominic Mazzoni
  
  Change Speed effect, that affects both pitch & tempo.

**********************************************************************/

#include "../Audacity.h" // for USE_LIBSAMPLERATE

#include <math.h>

#include <wx/intl.h>
#include <wx/msgdlg.h> // for wxMessageBox
#include <wx/valtext.h>

#include "../Envelope.h"
#include "../Prefs.h"
#include "../Internat.h"
#include "ChangeSpeed.h"

//
// EffectChangeSpeed
//

EffectChangeSpeed::EffectChangeSpeed()
{
	// control values
	m_PercentChange = 0.0;
	m_FromVinyl = 0; 
	m_ToVinyl = 0; 
}

wxString EffectChangeSpeed::GetEffectDescription() { 
   // Note: This is useful only after change amount has been set. 
   return wxString::Format(_("Applied effect: %s %.1f%%"), 
                           (const char *)(this->GetEffectName()), 
									m_PercentChange); 
} 

bool EffectChangeSpeed::PromptUser()
{
   ChangeSpeedDialog dlog(this, mParent, -1, _("Change Speed"));
   dlog.m_PercentChange = m_PercentChange;
   dlog.m_FromVinyl = m_FromVinyl;
   dlog.m_ToVinyl = m_ToVinyl;
	// Don't need to call TransferDataToWindow, although other 
	//	Audacity dialogs (from which I derived this one) do it, because 
	//	ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog, 
	//	which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   m_PercentChange = dlog.m_PercentChange;
   m_FromVinyl = dlog.m_FromVinyl;
   m_ToVinyl = dlog.m_ToVinyl;

   return true;
}

bool EffectChangeSpeed::Process()
{
	// Similar to EffectSoundTouch::Process()

   //Iterate over each track
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   mCurTrackNum = 0;
	m_maxNewLength = 0.0;
	double curT0;
	double curT1;
   while (track) {
      //Get start and end times from track
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();

      //Set the current bounds to whichever left marker is
      //greater and whichever right marker is less:
      curT0 = mT0 < trackStart? trackStart: mT0;
      curT1 = mT1 > trackEnd? trackEnd: mT1;

      // Process only if the right marker is to the right of the left marker
      if (curT1 > curT0) {

         //Transform the marker timepoints to samples
         longSampleCount start = track->TimeToLongSamples(curT0);
         longSampleCount end = track->TimeToLongSamples(curT1);

         //ProcessOne() (implemented below) processes a single track
         if (!ProcessOne(track, start, end))
            return false;
      }
      
      //Iterate to the next track
      track = (WaveTrack *) iter.Next();
      mCurTrackNum++;
   }

	mT1 = mT0 + m_maxNewLength; // Update selection.
   return true;
}

// ProcessOne() takes a track, transforms it to bunch of buffer-blocks,
// and calls libsamplerate code on these blocks.
bool EffectChangeSpeed::ProcessOne(WaveTrack * track,
												longSampleCount start, longSampleCount end)
{
	if (track == NULL)
		return false;

	// initialization, per examples of Mixer::Mixer and
   // EffectSoundTouch::ProcessOne

   WaveTrack * outputTrack = mFactory->NewWaveTrack(track->GetSampleFormat());

   //Get the length of the selection (as double). len is
   //used simple to calculate a progress meter, so it is easier
   //to make it a double now than it is to do it later 
   double len = (double)(end - start);

   // Initiate processing buffers, most likely shorter than 
	//	the length of the selection being processed.
	sampleCount inBufferSize = track->GetMaxBlockSize();

   float * inBuffer = new float[inBufferSize];

   double factor = 100.0 / (100.0 + m_PercentChange);
	sampleCount outBufferSize = 
      (sampleCount)((factor * inBufferSize) + 10);
   float * outBuffer = new float[outBufferSize]; 

	// Set up the resampling stuff for this track.
   Resample resample(true, factor, factor);

   //Go through the track one buffer at a time. samplePos counts which
   //sample the current buffer starts at.
	bool bLoopSuccess = true;
   sampleCount blockSize;
  	longSampleCount samplePos = start;
   while (samplePos < end) {
      //Get a blockSize of samples (smaller than the size of the buffer)
      blockSize = track->GetBestBlockSize(samplePos);

      //Adjust the block size if it is the final block in the track
      if (samplePos + blockSize > end)
         blockSize = end - samplePos;

      //Get the samples from the track and put them in the buffer
      track->Get((samplePtr) inBuffer, floatSample, samplePos, blockSize);

      int inUsed;
      int outgen = resample.Process(factor,
                                    inBuffer,
                                    blockSize,
                                    ((samplePos + blockSize) >= end),
                                    &inUsed,
                                    outBuffer,
                                    outBufferSize);
      if (outgen < 0) {
			bLoopSuccess = false;
			break;
		}

      if (outgen > 0)
         outputTrack->Append((samplePtr)outBuffer, floatSample, 
                             outgen);

      // Increment samplePos
      samplePos += inUsed;

      // Update the Progress meter
      if (TrackProgress(mCurTrackNum, (samplePos - start) / len)) {
			bLoopSuccess = false;
			break;
		}
   }

	// Flush the output WaveTrack (since it's buffered, too)
	outputTrack->Flush();

   // Clean up the buffers
   delete [] inBuffer;
   delete [] outBuffer;

   // Take the output track and insert it in place of the original
   // sample data
	if (bLoopSuccess) {
		track->Clear(mT0, mT1);
		track->Paste(mT0, outputTrack);
	}

	double newLength = outputTrack->GetEndTime(); 
	if (newLength > m_maxNewLength) 
		m_maxNewLength = newLength; 

   // Delete the outputTrack now that its data is inserted in place
   delete outputTrack;

   return bLoopSuccess;
}


//----------------------------------------------------------------------------
// ChangeSpeedDialog
//----------------------------------------------------------------------------

// -99 for PERCENTCHANGE_MIN because -100% is nonsensical.
#define PERCENTCHANGE_MIN -99

#define PERCENTCHANGE_MAX 100 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

// the standard vinyl RPM choices
// If the percent change is not one of these ratios, the choice control gets "n/a".
#define CHOICE_33ANDATHIRD 0
#define CHOICE_45 1
#define CHOICE_78 2
#define CHOICE_NA 3 

enum {
   ID_TEXT_PERCENTCHANGE = 10001,
   ID_SLIDER_PERCENTCHANGE,
   ID_CHOICE_FROMVINYL,
   ID_CHOICE_TOVINYL,
	ID_BUTTON_PREVIEW
};


// event table for ChangeSpeedDialog

BEGIN_EVENT_TABLE(ChangeSpeedDialog, wxDialog)
    EVT_BUTTON(wxID_OK, ChangeSpeedDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, ChangeSpeedDialog::OnCancel)

    EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangeSpeedDialog::OnText_PercentChange)
    EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangeSpeedDialog::OnSlider_PercentChange)
    EVT_CHOICE(ID_CHOICE_FROMVINYL, ChangeSpeedDialog::OnChoice_FromVinyl)
    EVT_CHOICE(ID_CHOICE_TOVINYL, ChangeSpeedDialog::OnChoice_ToVinyl)

    EVT_BUTTON(ID_BUTTON_PREVIEW, ChangeSpeedDialog::OnPreview)
END_EVENT_TABLE()

ChangeSpeedDialog::ChangeSpeedDialog(EffectChangeSpeed * effect,
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
   m_pTextCtrl_PercentChange = NULL;
   m_pSlider_PercentChange = NULL;
   m_pChoice_FromVinyl = NULL;
   m_pChoice_ToVinyl = NULL;
	
	// effect parameters
	m_PercentChange = 0.0;
	m_FromVinyl = 0; 
	m_ToVinyl = 0; 

	
	// CREATE THE CONTROLS PROGRAMMATICALLY.
	wxStaticText * pStaticText;

   wxBoxSizer * pBoxSizer_Dialog = new wxBoxSizer(wxVERTICAL);

	// heading
   pStaticText = new wxStaticText(this, -1, 
												_("Change Speed, affecting both Tempo and Pitch"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxALL, 8);

   pStaticText = new wxStaticText(this, -1, 
												_("by Vaughan Johnson && Dominic Mazzoni"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxTOP | wxLEFT | wxRIGHT, 8);

   pStaticText = new wxStaticText(this, -1, 
												_("using SampleRate, by Erik de Castro Lopo"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxBOTTOM | wxLEFT | wxRIGHT, 8);


	// percent change controls

	// Group percent controls with spacers, 
	// rather than static box, so they don't look isolated.
   pBoxSizer_Dialog->Add(0, 8, 0); // spacer

   wxBoxSizer * pBoxSizer_PercentChange = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText = new wxStaticText(this, -1, _("Percent Change:"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_PercentChange->Add(pStaticText, 0, 
											wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

	//v Override wxTextValidator to disallow negative values <= -100.0?
   m_pTextCtrl_PercentChange = 
		new wxTextCtrl(this, ID_TEXT_PERCENTCHANGE, "0.0", 
							wxDefaultPosition, wxSize(40, -1), 0,
							wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_PercentChange->Add(m_pTextCtrl_PercentChange, 0, 
											wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_PercentChange, 0, wxALIGN_CENTER | wxALL, 4);

   m_pSlider_PercentChange = 
		new wxSlider(this, ID_SLIDER_PERCENTCHANGE, 0, 
							PERCENTCHANGE_MIN, PERCENTCHANGE_MAX,
							wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   pBoxSizer_Dialog->Add(m_pSlider_PercentChange, 1, 
									wxGROW | wxALIGN_CENTER | wxLEFT | wxRIGHT, 4);

   pBoxSizer_Dialog->Add(0, 8, 0); // spacer


	// from/to Vinyl controls
   wxBoxSizer * pBoxSizer_Vinyl = new wxBoxSizer(wxHORIZONTAL);

	const wxString strArray_VinylRPM[] = {"33 1/3", "45", "78",
                                         /* i18n-hint: n/a is an English
                                            abbreviation meaning "not
                                            applicable" (in other words,
                                            unimportant, not relevant). */
                                         _("n/a")};
	const int numChoices = 4;

   pStaticText = new wxStaticText(this, -1, _("Standard Vinyl RPM:   from"),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Vinyl->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pChoice_FromVinyl = 
		new wxChoice(this, ID_CHOICE_FROMVINYL, 
							wxDefaultPosition, wxSize(64, -1), numChoices, strArray_VinylRPM);
   pBoxSizer_Vinyl->Add(m_pChoice_FromVinyl, 0, wxALIGN_CENTER | wxALL, 4);

   pStaticText = new wxStaticText(this, -1, _("to"),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Vinyl->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pChoice_ToVinyl = 
		new wxChoice(this, ID_CHOICE_TOVINYL, 
							wxDefaultPosition, wxSize(64, -1), numChoices, strArray_VinylRPM);
   pBoxSizer_Vinyl->Add(m_pChoice_ToVinyl, 0, wxALIGN_CENTER | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_Vinyl, 0, wxALIGN_CENTER | wxALL, 4);


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

bool ChangeSpeedDialog::Validate()
{
   return true; 
}

bool ChangeSpeedDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

	// percent change controls
	this->Update_Text_PercentChange();
	this->Update_Slider_PercentChange();

	// from/to Vinyl controls
	if (m_pChoice_FromVinyl) 
		m_pChoice_FromVinyl->SetSelection(m_FromVinyl);

	if (m_pChoice_ToVinyl) 
		m_pChoice_ToVinyl->SetSelection(m_ToVinyl);

	m_bLoopDetect = false;

	return true;
}

bool ChangeSpeedDialog::TransferDataFromWindow()
{
	wxString str;

	// percent change controls
   if (m_pTextCtrl_PercentChange) {
      double newValue = 0;
      str = m_pTextCtrl_PercentChange->GetValue();
      str.ToDouble(&newValue);
		m_PercentChange = newValue;
	}

	// Ignore Slider_PercentChange because TextCtrl_PercentChange 
	// always tracks it & is more precise (decimal points).


	// from/to Vinyl controls
	if (m_pChoice_FromVinyl) 
		m_FromVinyl = m_pChoice_FromVinyl->GetSelection();

	if (m_pChoice_ToVinyl) 
		m_ToVinyl = m_pChoice_ToVinyl->GetSelection();

   return true;
}

// handler implementations for ChangeSpeedDialog

void ChangeSpeedDialog::OnText_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PercentChange) {
      double newValue = 0;
      wxString str = m_pTextCtrl_PercentChange->GetValue();
      str.ToDouble(&newValue);
		m_PercentChange = newValue;

      m_bLoopDetect = true;
		this->Update_Slider_PercentChange();
		this->Update_Vinyl();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangeSpeedDialog::OnSlider_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	if (m_pSlider_PercentChange) {
		m_PercentChange = (double)(m_pSlider_PercentChange->GetValue()); 
		// Warp positive values to actually go up faster & further than negatives.
		if (m_PercentChange > 0.0)
			m_PercentChange = pow(m_PercentChange, PERCENTCHANGE_SLIDER_WARP);

	   m_bLoopDetect = true;
		this->Update_Text_PercentChange();
		this->Update_Vinyl();
	   m_bLoopDetect = false;
	}
}

void ChangeSpeedDialog::OnChoice_FromVinyl(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	if (m_pChoice_FromVinyl) {
		m_FromVinyl = m_pChoice_FromVinyl->GetSelection();

      m_bLoopDetect = true;
		this->Update_PercentChange();
      m_bLoopDetect = false;
   }
}

void ChangeSpeedDialog::OnChoice_ToVinyl(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	if (m_pChoice_ToVinyl) {
		m_ToVinyl = m_pChoice_ToVinyl->GetSelection();

      m_bLoopDetect = true;
		this->Update_PercentChange();
      m_bLoopDetect = false;
   }
}


void ChangeSpeedDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

	// Save & restore parameters around Preview, because we didn't do OK.
	double oldPercentChange = m_pEffect->m_PercentChange;
   m_pEffect->m_PercentChange = m_PercentChange;
   m_pEffect->Preview();
   m_pEffect->m_PercentChange = oldPercentChange; 
}

void ChangeSpeedDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();
   
   if (Validate()) 
      EndModal(true);
   else 
      event.Skip();
}

void ChangeSpeedDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}


// helper fns

void ChangeSpeedDialog::Update_Text_PercentChange()
{
	if (m_pTextCtrl_PercentChange) {
		wxString str;
		str.Printf("%.3f", m_PercentChange);
		m_pTextCtrl_PercentChange->SetValue(str);
	}
}

void ChangeSpeedDialog::Update_Slider_PercentChange()
{
   if (m_pSlider_PercentChange) {
		double unwarped = m_PercentChange;
		if (unwarped > 0.0)
			// Un-warp values above zero to actually go up to PERCENTCHANGE_MAX.
			unwarped = pow(m_PercentChange, (1.0 / PERCENTCHANGE_SLIDER_WARP));
		
		// Add 0.5 to unwarped so trunc -> round.
		m_pSlider_PercentChange->SetValue((int)(unwarped + 0.5)); 
	}
}

void ChangeSpeedDialog::Update_Vinyl() 
// Update Vinyl controls for new percent change.
{
	if (m_pChoice_ToVinyl) 
		// Chances are so low that the slider will exactly match a 
		// standard ratio, just turn it "n/a" unless it's 0.0.
		if ((m_PercentChange == 0.0) && m_pChoice_FromVinyl)
			m_pChoice_ToVinyl->SetSelection(m_pChoice_FromVinyl->GetSelection());
		else
			m_pChoice_ToVinyl->SetSelection(CHOICE_NA);
}

void ChangeSpeedDialog::Update_PercentChange() 
// Update percent change controls for new Vinyl values.
{
	// If m_FromVinyl & m_ToVinyl are set, then there's a new percent change.
	if ((m_FromVinyl != CHOICE_NA) && (m_ToVinyl != CHOICE_NA)) {
		double fromRPM;
		double toRPM;
		switch (m_FromVinyl) {
      default:
		case CHOICE_33ANDATHIRD:	fromRPM = 33.0 + (1.0 / 3.0); break;
		case CHOICE_45:				fromRPM = 45.0; break;
		case CHOICE_78:				fromRPM = 78; break;
		}
		switch (m_ToVinyl) {
      default:
		case CHOICE_33ANDATHIRD:	toRPM = 33.0 + (1.0 / 3.0); break;
		case CHOICE_45:				toRPM = 45.0; break;
		case CHOICE_78:				toRPM = 78; break;
		}
		m_PercentChange = ((toRPM * 100.0) / fromRPM) - 100.0;

		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();
	}
}
