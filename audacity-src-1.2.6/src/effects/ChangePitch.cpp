/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangePitch.cpp

  Vaughan Johnson, Dominic Mazzoni
  
  Change Pitch effect provides raising or lowering 
  the pitch without changing the tempo.

**********************************************************************/

#include "../Audacity.h" // for USE_SOUNDTOUCH

#if USE_SOUNDTOUCH

#include <soundtouch/SoundTouch.h>
// this is the standard system path, then we use -I cflags to make
// it work if we have local soundtouch

#include "ChangePitch.h"

#include "../PitchName.h"
#include "../Spectrum.h"
#include "../WaveTrack.h"

#include <math.h>

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/radiobox.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>

//
// EffectChangePitch
//

EffectChangePitch::EffectChangePitch()
{
	m_FromPitchIndex = -1;		// -1 => uninitialized
	m_bWantPitchDown = false;
	m_ToPitchIndex = -1;			// -1 => uninitialized

	m_SemitonesChange = 0.0;

   m_FromFrequency = 0.0;		// 0.0 => uninitialized
   m_ToFrequency = 0.0;			// 0.0 => uninitialized

	m_PercentChange = 0.0;
}

wxString EffectChangePitch::GetEffectDescription() { 
   // Note: This is useful only after change amount has been set. 
   return wxString::Format(_("Applied effect: %s %.2f semitones"), 
                           (const char *)(this->GetEffectName()), 
									m_SemitonesChange); 
} 

bool EffectChangePitch::Init()
{
   mSoundTouch = NULL;
	return true;
}

// DeduceFrequencies is Dominic's extremely cool trick (Vaughan sez so!) 
// to set deduce m_FromFrequency from the samples at the beginning of 
// the selection. Then we set some other params accordingly.
void EffectChangePitch::DeduceFrequencies()
{
   // As a neat trick, attempt to get the frequency of the note at the
   // beginning of the selection.
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   if (track) {
      const int windowSize = 1024;
      const int analyzeSize = 8192;
      const int numWindows = analyzeSize / windowSize;
      double trackStart = track->GetStartTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      longSampleCount start = track->TimeToLongSamples(t0);
      double rate = track->GetRate();
      float buffer[analyzeSize];
      float freq[windowSize/2];
      float freqa[windowSize/2];
      int i, j, argmax;
      int lag;

      for(j=0; j<windowSize/2; j++)
         freqa[j] = 0;

      track->Get((samplePtr) buffer, floatSample, start, analyzeSize);
      for(i=0; i<numWindows; i++) {
         ComputeSpectrum(buffer+i*windowSize, windowSize, windowSize/2,
                         (int)rate, windowSize, rate, freq, true);
         for(j=0; j<windowSize/2; j++)
            freqa[j] += freq[j];
      }
      argmax=0;
      for(j=1; j<windowSize/2; j++)
         if (freqa[j] > freqa[argmax])
            argmax = j;
      lag = (windowSize/2 - 1) - argmax;
      m_FromFrequency = rate / lag;
		m_ToFrequency = (m_FromFrequency * (100.0 + m_PercentChange)) / 100.0;

		// Now we can set the pitch control values. 
		m_FromPitchIndex = PitchIndex(Freq2Pitch(m_FromFrequency));
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);
		m_ToPitchIndex = PitchIndex(Freq2Pitch(m_ToFrequency));
   }
}

bool EffectChangePitch::PromptUser()
{
	this->DeduceFrequencies(); // Set frequency-related control values based on sample.

   ChangePitchDialog dlog(this, mParent, -1, _("Change Pitch"));
   dlog.m_FromPitchIndex = m_FromPitchIndex;
   dlog.m_bWantPitchDown = m_bWantPitchDown;
   dlog.m_ToPitchIndex = m_ToPitchIndex;
	dlog.m_SemitonesChange = m_SemitonesChange;
   dlog.m_FromFrequency = m_FromFrequency;
   dlog.m_ToFrequency = m_ToFrequency;
   dlog.m_PercentChange = m_PercentChange;
	// Don't need to call TransferDataToWindow, although other 
	//	Audacity dialogs (from which I derived this one) do it, because 
	//	ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog, 
	//	which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   m_FromPitchIndex = dlog.m_FromPitchIndex;
   m_bWantPitchDown = dlog.m_bWantPitchDown;
   m_ToPitchIndex = dlog.m_ToPitchIndex;
	m_SemitonesChange = dlog.m_SemitonesChange;
   m_FromFrequency = dlog.m_FromFrequency;
   m_ToFrequency = dlog.m_ToFrequency;
   m_PercentChange = dlog.m_PercentChange;
   return true;
}

bool EffectChangePitch::Process()
{
   mSoundTouch = new SoundTouch();
   mSoundTouch->setPitchSemiTones((float)(m_SemitonesChange));
   return this->EffectSoundTouch::Process();
}

//----------------------------------------------------------------------------
// ChangePitchDialog
//----------------------------------------------------------------------------

#define PERCENTCHANGE_MIN -99
#define PERCENTCHANGE_MAX 100 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

enum {
   ID_TEXT_PERCENTCHANGE = 10001,
   ID_SLIDER_PERCENTCHANGE,
   ID_CHOICE_FROMPITCH,
   ID_RADIOBOX_PITCHUPDOWN,
   ID_CHOICE_TOPITCH,
   ID_TEXT_SEMITONESCHANGE,
   ID_TEXT_FROMFREQUENCY,
   ID_TEXT_TOFREQUENCY,
	ID_BUTTON_PREVIEW
};

// event table for ChangePitchDialog

BEGIN_EVENT_TABLE(ChangePitchDialog, wxDialog)
	EVT_BUTTON(wxID_OK, ChangePitchDialog::OnOk)
	EVT_BUTTON(wxID_CANCEL, ChangePitchDialog::OnCancel)

	EVT_CHOICE(ID_CHOICE_FROMPITCH, ChangePitchDialog::OnChoice_FromPitch)
	EVT_RADIOBOX(ID_RADIOBOX_PITCHUPDOWN, ChangePitchDialog::OnRadioBox_PitchUpDown)
	EVT_CHOICE(ID_CHOICE_TOPITCH, ChangePitchDialog::OnChoice_ToPitch)

	EVT_TEXT(ID_TEXT_SEMITONESCHANGE, ChangePitchDialog::OnText_SemitonesChange)

	EVT_TEXT(ID_TEXT_FROMFREQUENCY, ChangePitchDialog::OnText_FromFrequency)
	EVT_TEXT(ID_TEXT_TOFREQUENCY, ChangePitchDialog::OnText_ToFrequency)

	EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangePitchDialog::OnText_PercentChange)
	EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangePitchDialog::OnSlider_PercentChange)

	EVT_BUTTON(ID_BUTTON_PREVIEW, ChangePitchDialog::OnPreview)
END_EVENT_TABLE()


ChangePitchDialog::ChangePitchDialog(EffectChangePitch * effect, 
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
   m_pChoice_FromPitch = NULL;
	m_pRadioBox_PitchUpDown = NULL;
	m_pChoice_ToPitch = NULL;
   
	m_pTextCtrl_SemitonesChange = NULL;

	m_pTextCtrl_FromFrequency = NULL;
	m_pTextCtrl_ToFrequency = NULL;
   
	m_pTextCtrl_PercentChange = NULL;
	m_pSlider_PercentChange = NULL;

	// effect parameters
	m_FromPitchIndex = -1;		// -1 => uninitialized
	m_bWantPitchDown = false;
	m_ToPitchIndex = -1;			// -1 => uninitialized

	m_SemitonesChange = 0.0;

   m_FromFrequency = 0.0;		// 0.0 => uninitialized
   m_ToFrequency = 0.0;			// 0.0 => uninitialized

	m_PercentChange = 0.0;

	
	// CREATE THE CONTROLS PROGRAMMATICALLY.
	wxStaticText * pStaticText;

   wxBoxSizer * pBoxSizer_Dialog = new wxBoxSizer(wxVERTICAL);

	// heading
   pStaticText = new wxStaticText(this, -1, 
												_("Change Pitch without Changing Tempo"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxALL, 8);

   pStaticText = new wxStaticText(this, -1, 
												_("by Vaughan Johnson && Dominic Mazzoni"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxTOP | wxLEFT | wxRIGHT, 8);

   pStaticText = new wxStaticText(this, -1, 
												_("using SoundTouch, by Olli Parviainen"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Dialog->Add(pStaticText, 0, wxALIGN_CENTER | wxBOTTOM | wxLEFT | wxRIGHT, 8);

   pBoxSizer_Dialog->Add(0, 8, 0); // spacer


	// from/to pitch controls
   wxBoxSizer * pBoxSizer_Pitch = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText = new wxStaticText(this, -1, _("Pitch:   from"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Pitch->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

	const wxString strArray_PitchNames[] = 
		{"C", "C#/Db", "D", "D#/Eb", "E", "F", 
       "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"};
	const int numChoicesPitchNames = 12;
   m_pChoice_FromPitch = 
		new wxChoice(this, ID_CHOICE_FROMPITCH, wxDefaultPosition, wxSize(64, -1), 
							numChoicesPitchNames, strArray_PitchNames);
   pBoxSizer_Pitch->Add(m_pChoice_FromPitch, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

	const wxString strArray_RadioPitchUpDown[] = {_("up"), _("down")};
	m_pRadioBox_PitchUpDown = 
		new wxRadioBox(this, ID_RADIOBOX_PITCHUPDOWN, "", wxDefaultPosition, wxDefaultSize, 
								2, strArray_RadioPitchUpDown, 1);
   pBoxSizer_Pitch->Add(m_pRadioBox_PitchUpDown, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);
   
	pStaticText = new wxStaticText(this, -1, _("to"), wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Pitch->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pChoice_ToPitch = 
		new wxChoice(this, ID_CHOICE_TOPITCH, wxDefaultPosition, wxSize(64, -1), 
							numChoicesPitchNames, strArray_PitchNames);
   pBoxSizer_Pitch->Add(m_pChoice_ToPitch, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_Pitch, 0, wxALIGN_CENTER | wxALL, 4);


	// semitones change controls
   wxBoxSizer * pBoxSizer_SemitonesChange = new wxBoxSizer(wxHORIZONTAL);
   pStaticText = new wxStaticText(this, -1, _("Semitones (half-steps):"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_SemitonesChange->Add(pStaticText, 0, 
												wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pTextCtrl_SemitonesChange = 
		new wxTextCtrl(this, ID_TEXT_SEMITONESCHANGE, "0.0", 
							wxDefaultPosition, wxSize(40, -1), 0, 
							wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_SemitonesChange->Add(m_pTextCtrl_SemitonesChange, 0, 
												wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_SemitonesChange, 0, wxALIGN_CENTER | wxALL, 4);

	
	// from/to frequency controls
   pBoxSizer_Dialog->Add(0, 8, 0); // spacer
   wxBoxSizer * pBoxSizer_Frequency = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText = new wxStaticText(this, -1, _("Frequency (Hz):   from"),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Frequency->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pTextCtrl_FromFrequency = 
		new wxTextCtrl(this, ID_TEXT_FROMFREQUENCY, "", 
							wxDefaultPosition, wxSize(64, -1), 0,
							wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_Frequency->Add(m_pTextCtrl_FromFrequency, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pStaticText = new wxStaticText(this, -1, _("to"),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Frequency->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pTextCtrl_ToFrequency = 
		new wxTextCtrl(this, ID_TEXT_TOFREQUENCY, "", 
							wxDefaultPosition, wxSize(64, -1), 0,
							wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_Frequency->Add(m_pTextCtrl_ToFrequency, 0, 
										wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_Frequency, 0, wxALIGN_CENTER | wxALL, 4);


	// percent change control
	
	// Group percent controls with spacers, 
	// rather than static box, so they don't look isolated.
   pBoxSizer_Dialog->Add(0, 8, 0); // spacer

   wxBoxSizer * pBoxSizer_PercentChange = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText = new wxStaticText(this, -1, _("Percent Change:"),
												wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_PercentChange->Add(pStaticText, 0, 
											wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

	//v Override wxTextValidator to disallow negative values < -100.0?
   m_pTextCtrl_PercentChange = 
		new wxTextCtrl(this, ID_TEXT_PERCENTCHANGE, "0.0", 
							wxDefaultPosition, wxSize(40, -1), 0,
							wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_PercentChange->Add(m_pTextCtrl_PercentChange, 0, 
											wxALIGN_CENTER_VERTICAL | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_PercentChange, 0, wxALIGN_CENTER | wxALL, 4);

   m_pSlider_PercentChange = 
		new wxSlider(this, ID_SLIDER_PERCENTCHANGE, 
							0, PERCENTCHANGE_MIN, PERCENTCHANGE_MAX,
							wxDefaultPosition, wxSize(100, -1), wxSL_HORIZONTAL);
   pBoxSizer_Dialog->Add(m_pSlider_PercentChange, 1, 
									wxGROW | wxALIGN_CENTER | wxLEFT | wxRIGHT, 4);

   pBoxSizer_Dialog->Add(0, 8, 0); // spacer


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

bool ChangePitchDialog::Validate()
{
   return true; 
}

bool ChangePitchDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

	// from/to pitch controls
	if (m_pChoice_FromPitch) 
		m_pChoice_FromPitch->SetSelection(m_FromPitchIndex);

	this->Update_RadioBox_PitchUpDown();
	this->Update_Choice_ToPitch();


	// semitones change control
	this->Update_Text_SemitonesChange();


	// from/to frequency controls
	if (m_pTextCtrl_FromFrequency) {
		wxString str;
		if (m_FromFrequency > 0.0)
			str.Printf("%.3f", m_FromFrequency);
		else
			str = "";
		m_pTextCtrl_FromFrequency->SetValue(str);
	}

	this->Update_Text_ToFrequency();

	
	// percent change controls
	this->Update_Text_PercentChange();
	this->Update_Slider_PercentChange();


   m_bLoopDetect = false;

	return true;
}

bool ChangePitchDialog::TransferDataFromWindow()
{
   double newDouble;
	wxString str;


	// from/to pitch controls
	if (m_pChoice_FromPitch) 
		m_FromPitchIndex = m_pChoice_FromPitch->GetSelection(); 

	if (m_pRadioBox_PitchUpDown)
		m_bWantPitchDown = (m_pRadioBox_PitchUpDown->GetSelection() == 1);

	if (m_pChoice_ToPitch) 
		m_ToPitchIndex = m_pChoice_ToPitch->GetSelection();


	// semitones change control
   if (m_pTextCtrl_SemitonesChange) {
      str = m_pTextCtrl_SemitonesChange->GetValue();
      str.ToDouble(&newDouble);
		m_SemitonesChange = newDouble;
	}


	// from/to frequency controls
   if (m_pTextCtrl_FromFrequency) {
      str = m_pTextCtrl_FromFrequency->GetValue();
      str.ToDouble(&newDouble);
		m_FromFrequency = newDouble;
	}

   if (m_pTextCtrl_ToFrequency) {
      str = m_pTextCtrl_ToFrequency->GetValue();
      str.ToDouble(&newDouble);
		m_ToFrequency = newDouble;
	}


	// percent change controls
   if (m_pTextCtrl_PercentChange) {
      str = m_pTextCtrl_PercentChange->GetValue();
      str.ToDouble(&newDouble);
		m_PercentChange = newDouble;
	}

	// Ignore Slider_PercentChange because TextCtrl_PercentChange 
	// always tracks it & is more precise (decimal points).


   return true;
}


// calculations

void ChangePitchDialog::Calc_ToFrequency()
{
	m_ToFrequency = (m_FromFrequency * (100.0 + m_PercentChange)) / 100.0;
}

void ChangePitchDialog::Calc_ToPitchIndex()
{
	m_ToPitchIndex = (m_FromPitchIndex + 
							(int)(m_SemitonesChange + 
									// Round in the right direction.
									((m_bWantPitchDown ? -1.0 : 1.0) * 0.5))) 
							% 12;
}

void ChangePitchDialog::Calc_SemitonesChange_fromPitches()
{
	int sign = m_bWantPitchDown ? -1 : 1;
	m_SemitonesChange = sign * (((sign * (m_ToPitchIndex - m_FromPitchIndex)) + 12) % 12); 
}

void ChangePitchDialog::Calc_SemitonesChange_fromPercentChange()
{
	// Use m_PercentChange rather than m_FromFrequency & m_ToFrequency, because 
	// they start out uninitialized, but m_PercentChange is always valid.
	m_SemitonesChange = (12.0 * log((100.0 + m_PercentChange) / 100.0)) / log(2.0);
}

void ChangePitchDialog::Calc_PercentChange()
{
	m_PercentChange = 100.0 * (pow(2.0, (m_SemitonesChange / 12.0)) - 1.0);
}


// handlers

void ChangePitchDialog::OnChoice_FromPitch(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pChoice_FromPitch) {
		m_FromPitchIndex = m_pChoice_FromPitch->GetSelection();

		this->Calc_ToPitchIndex();

		m_bLoopDetect = true;
		this->Update_Choice_ToPitch();
		m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnRadioBox_PitchUpDown(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	if (m_pRadioBox_PitchUpDown) {
		m_bWantPitchDown = (m_pRadioBox_PitchUpDown->GetSelection() == 1);

		this->Calc_SemitonesChange_fromPitches();
		this->Calc_PercentChange(); // Call *after* m_SemitonesChange is updated.
		this->Calc_ToFrequency(); // Call *after* m_PercentChange is updated.

		m_bLoopDetect = true;
		this->Update_Text_SemitonesChange();
		this->Update_Text_ToFrequency();
		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();
		m_bLoopDetect = false;
	}
}

void ChangePitchDialog::OnChoice_ToPitch(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pChoice_ToPitch) {
		m_ToPitchIndex = m_pChoice_ToPitch->GetSelection();

		this->Calc_SemitonesChange_fromPitches();
		this->Calc_PercentChange(); // Call *after* m_SemitonesChange is updated.
		this->Calc_ToFrequency(); // Call *after* m_PercentChange is updated.

		m_bLoopDetect = true;
		this->Update_Text_SemitonesChange();
		this->Update_Text_ToFrequency();
		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();
		m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnText_SemitonesChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	if (m_pTextCtrl_SemitonesChange) {
		wxString str = m_pTextCtrl_SemitonesChange->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
		m_SemitonesChange = newValue;

		this->Calc_PercentChange();
		this->Calc_ToFrequency(); // Call *after* m_PercentChange is updated.
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);
		this->Calc_ToPitchIndex(); // Call *after* m_bWantPitchDown is updated.

		m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		if (m_pTextCtrl_SemitonesChange->IsModified())
			// See note at implementation of Update_RadioBox_PitchUpDown.
			m_pTextCtrl_SemitonesChange->SetFocus(); 
		this->Update_Choice_ToPitch();
		this->Update_Text_ToFrequency();
		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();
		m_bLoopDetect = false;
	}
}

void ChangePitchDialog::OnText_FromFrequency(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	if (m_pTextCtrl_FromFrequency) {
		wxString str = m_pTextCtrl_FromFrequency->GetValue();
		double newDouble;
      str.ToDouble(&newDouble);
		m_FromFrequency = newDouble;

		m_FromPitchIndex = PitchIndex(Freq2Pitch(m_FromFrequency));
		this->Calc_ToFrequency();
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);
		this->Calc_ToPitchIndex(); // Call *after* m_bWantPitchDown is updated.

		m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		if (m_pTextCtrl_FromFrequency->IsModified())
			// See note at implementation of Update_RadioBox_PitchUpDown.
			m_pTextCtrl_FromFrequency->SetFocus(); 
		this->Update_Choice_ToPitch();
		this->Update_Text_ToFrequency();
		m_bLoopDetect = false;
	}
}

void ChangePitchDialog::OnText_ToFrequency(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_ToFrequency) {
      wxString str = m_pTextCtrl_ToFrequency->GetValue();
		double newDouble;
      str.ToDouble(&newDouble);
		m_ToFrequency = newDouble;

		m_PercentChange = (((double)(m_ToFrequency) * 100.0) / 
									(double)(m_FromFrequency)) - 100.0;

		this->Calc_SemitonesChange_fromPercentChange();
		this->Calc_ToPitchIndex(); // Call *after* m_SemitonesChange is updated.
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);

		m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		if (m_pTextCtrl_ToFrequency->IsModified())
			// See note at implementation of Update_RadioBox_PitchUpDown.
			m_pTextCtrl_ToFrequency->SetFocus(); 
		this->Update_Choice_ToPitch();
		this->Update_Text_SemitonesChange();
		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();
		m_bLoopDetect = false;
   }
}

void ChangePitchDialog::OnText_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PercentChange) {
      wxString str = m_pTextCtrl_PercentChange->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
		m_PercentChange = newValue;

		this->Calc_SemitonesChange_fromPercentChange();
		this->Calc_ToPitchIndex(); // Call *after* m_SemitonesChange is updated.
		this->Calc_ToFrequency();
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);

      m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		if (m_pTextCtrl_PercentChange->IsModified())
			// See note at implementation of Update_RadioBox_PitchUpDown.
			m_pTextCtrl_PercentChange->SetFocus(); 
		this->Update_Choice_ToPitch();
		this->Update_Text_SemitonesChange();
		this->Update_Text_ToFrequency();
		this->Update_Slider_PercentChange();
      m_bLoopDetect = false;

      //v Probably better to override wxTextValidator to disallow negative values.
      // See comment in ChangePitchDialog::ChangePitchDialog.
      this->FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangePitchDialog::OnSlider_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

	if (m_pSlider_PercentChange) {
		m_PercentChange = (double)(m_pSlider_PercentChange->GetValue()); 
		// Warp positive values to actually go up faster & further than negatives.
		if (m_PercentChange > 0.0)
			m_PercentChange = pow(m_PercentChange, PERCENTCHANGE_SLIDER_WARP);

		this->Calc_SemitonesChange_fromPercentChange();
		this->Calc_ToPitchIndex(); // Call *after* m_SemitonesChange is updated.
		this->Calc_ToFrequency();
		m_bWantPitchDown = (m_ToFrequency < m_FromFrequency);

		m_bLoopDetect = true;
		this->Update_RadioBox_PitchUpDown();
		this->Update_Choice_ToPitch();
		this->Update_Text_SemitonesChange();
		this->Update_Text_ToFrequency();
		this->Update_Text_PercentChange();
	   m_bLoopDetect = false;
	}
}


void ChangePitchDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

	// Save & restore parameters around Preview, because we didn't do OK.
	double oldSemitonesChange = m_SemitonesChange;
	m_pEffect->m_SemitonesChange = m_SemitonesChange;
	m_pEffect->Preview();
	m_pEffect->m_SemitonesChange = oldSemitonesChange;
}

void ChangePitchDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();
   
   if (Validate()) 
      EndModal(true);
   else 
      event.Skip();
}

void ChangePitchDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}


// helper fns

// NOTE: wxWindows ref (C:\wxWindows_2.4.0\docs\htmlhelp) says 
// wxRadioBox::SetSelection "does not cause a 
// wxEVT_COMMAND_RADIOBOX_SELECTED event to get emitted", but it 
// calls SetFocus, which sure as heck DOES select the radio button.
//
// So, any wxTextCtrl handler that calls Update_RadioBox_PitchUpDown 
// needs to call wxTextCtrl::SetFocus afterward, to return the 
// focus to the wxTextCtrl so the user can keep typing.
//
// Also, it turns out the wxTextCtrl handlers are sometimes 
// called before the dialog is displayed, so those SetFocus calls 
// need to be conditionalized on wxTextCtrl::IsModified.
void ChangePitchDialog::Update_RadioBox_PitchUpDown() 
{
	if (m_pRadioBox_PitchUpDown)
		m_pRadioBox_PitchUpDown->SetSelection((int)(m_bWantPitchDown));
}

void ChangePitchDialog::Update_Choice_ToPitch() 
{
	if (m_pChoice_ToPitch) 
		m_pChoice_ToPitch->SetSelection(m_ToPitchIndex);
}


void ChangePitchDialog::Update_Text_SemitonesChange()
{
	if (m_pTextCtrl_SemitonesChange) {
		wxString str;
		str.Printf("%.2f", m_SemitonesChange);
		m_pTextCtrl_SemitonesChange->SetValue(str);
	}
}

void ChangePitchDialog::Update_Text_ToFrequency() 
{
	if (m_pTextCtrl_ToFrequency) {
		wxString str;
		if (m_ToFrequency > 0.0)
			str.Printf("%.3f", m_ToFrequency);
		else
			str = "";
		m_pTextCtrl_ToFrequency->SetValue(str);
	}
}


void ChangePitchDialog::Update_Text_PercentChange()
{
	if (m_pTextCtrl_PercentChange) {
		wxString str;
		str.Printf("%.3f", m_PercentChange);
		m_pTextCtrl_PercentChange->SetValue(str);
	}
}

void ChangePitchDialog::Update_Slider_PercentChange()
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


#endif // USE_SOUNDTOUCH

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0b070f91-579c-4b57-bc29-82ceb6775355

