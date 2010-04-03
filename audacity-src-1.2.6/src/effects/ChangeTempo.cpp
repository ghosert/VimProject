/**********************************************************************

  Audacity: A Digital Audio Editor

  ChangeTempo.cpp

  Vaughan Johnson, Dominic Mazzoni
  
  Change Tempo effect provides speeding up or 
  slowing down tempo without changing pitch.

**********************************************************************/

#include "../Audacity.h" // for USE_SOUNDTOUCH
#include "../Internat.h"

#if USE_SOUNDTOUCH

#include <math.h>

#include <wx/intl.h>
#include <wx/valtext.h>

#include <soundtouch/SoundTouch.h>
// this is the standard system path, then we use -I cflags to make
// it work if we have local soundtouch

#include "ChangeTempo.h"

//
// EffectChangeTempo
//

EffectChangeTempo::EffectChangeTempo()
{
	m_PercentChange = 0.0;
	m_FromBPM = 0; // indicates not yet set
	m_ToBPM = 0; // indicates not yet set
   m_FromLength = 0.0;	
   m_ToLength = 0.0;	
}

wxString EffectChangeTempo::GetEffectDescription() { 
   // Note: This is useful only after change amount has been set. 
   return wxString::Format(_("Applied effect: %s %.1f%%"), 
                           (const char *)(this->GetEffectName()), 
									m_PercentChange); 
} 

bool EffectChangeTempo::Init()
{
	// The selection might have changed since the last time EffectChangeTempo 
	// was invoked, so recalculate the Length parameters.
	m_FromLength = mT1 - mT0;
	m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);

   mSoundTouch = NULL;

	return true;
}

bool EffectChangeTempo::PromptUser()
{
   ChangeTempoDialog dlog(this, mParent, -1, _("Change Tempo"));
   dlog.m_PercentChange = m_PercentChange;
   dlog.m_FromBPM = m_FromBPM;
   dlog.m_ToBPM = m_ToBPM;
   dlog.m_FromLength = m_FromLength;
   dlog.m_ToLength = m_ToLength;
	// Don't need to call TransferDataToWindow, although other 
	//	Audacity dialogs (from which I derived this one) do it, because 
	//	ShowModal calls stuff that eventually calls wxWindowBase::OnInitDialog, 
	//	which calls dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   m_PercentChange = dlog.m_PercentChange;
   m_FromBPM = dlog.m_FromBPM;
   m_ToBPM = dlog.m_ToBPM;
   m_ToLength = dlog.m_ToLength;
   return true;
}


bool EffectChangeTempo::Process()
{
   mSoundTouch = new SoundTouch();
   mSoundTouch->setTempoChange(m_PercentChange);
   return this->EffectSoundTouch::Process();
}

//----------------------------------------------------------------------------
// ChangeTempoDialog
//----------------------------------------------------------------------------

#define PERCENTCHANGE_MIN -99
#define PERCENTCHANGE_MAX 100 // warped above zero to actually go up to 400%
#define PERCENTCHANGE_SLIDER_WARP 1.30105 // warp power takes max from 100 to 400.

#define ID_TEXT_PERCENTCHANGE 10001
#define ID_SLIDER_PERCENTCHANGE 10002
#define ID_TEXT_FROMBPM 10003
#define ID_TEXT_TOBPM 10004
#define ID_TEXT_FROMLENGTH 10005
#define ID_TEXT_TOLENGTH 10006
#define ID_BUTTON_PREVIEW 10009

// event table for ChangeTempoDialog

BEGIN_EVENT_TABLE(ChangeTempoDialog, wxDialog)
    EVT_BUTTON(wxID_OK, ChangeTempoDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, ChangeTempoDialog::OnCancel)

    EVT_TEXT(ID_TEXT_PERCENTCHANGE, ChangeTempoDialog::OnText_PercentChange)
    EVT_SLIDER(ID_SLIDER_PERCENTCHANGE, ChangeTempoDialog::OnSlider_PercentChange)
    EVT_TEXT(ID_TEXT_FROMBPM, ChangeTempoDialog::OnText_FromBPM)
    EVT_TEXT(ID_TEXT_TOBPM, ChangeTempoDialog::OnText_ToBPM)
    EVT_TEXT(ID_TEXT_TOLENGTH, ChangeTempoDialog::OnText_ToLength)

    EVT_BUTTON(ID_BUTTON_PREVIEW, ChangeTempoDialog::OnPreview)
END_EVENT_TABLE()

ChangeTempoDialog::ChangeTempoDialog(EffectChangeTempo * effect, 
													wxWindow * parent, wxWindowID id, 
													const wxString & title, 
													const wxPoint & position, const wxSize & size, 
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
   m_pTextCtrl_FromBPM = NULL;
   m_pTextCtrl_ToBPM = NULL;
   m_pTextCtrl_FromLength = NULL;
   m_pTextCtrl_ToLength = NULL;
	
	// effect parameters
	m_PercentChange = 0.0;
	m_FromBPM = 0; // indicates not yet set
	m_ToBPM = 0; // indicates not yet set
   m_FromLength = 0.0;	
   m_ToLength = 0.0;	

	
	// CREATE THE CONTROLS PROGRAMMATICALLY.
	wxStaticText * pStaticText;

   wxBoxSizer * pBoxSizer_Dialog = new wxBoxSizer(wxVERTICAL);

	// heading
   pStaticText = new wxStaticText(this, -1, 
												_("Change Tempo without Changing Pitch"),
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
							(int)PERCENTCHANGE_MIN, (int)PERCENTCHANGE_MAX,
							wxDefaultPosition, wxSize(100, -1), 
							wxSL_HORIZONTAL);
   pBoxSizer_Dialog->Add(m_pSlider_PercentChange, 1, 
									wxGROW | wxALIGN_CENTER | wxLEFT | wxRIGHT, 4);

   pBoxSizer_Dialog->Add(0, 8, 0); // spacer


	// from/to beats-per-minute controls
   wxBoxSizer * pBoxSizer_BPM = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText = new wxStaticText(this, -1, _("Beats per Minute (BPM):   from"),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_BPM->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pTextCtrl_FromBPM = 
		new wxTextCtrl(this, ID_TEXT_FROMBPM, "", 
							wxDefaultPosition, wxSize(40, -1), 0,
							wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_BPM->Add(m_pTextCtrl_FromBPM, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pStaticText = new wxStaticText(this, -1, _("to"),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_BPM->Add(pStaticText, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pTextCtrl_ToBPM = 
		new wxTextCtrl(this, ID_TEXT_TOBPM, "", 
							wxDefaultPosition, wxSize(40, -1), 0,
							wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_BPM->Add(m_pTextCtrl_ToBPM, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_BPM, 0, wxALIGN_CENTER | wxALL, 4);


	// from/to length controls
   wxBoxSizer * pBoxSizer_Length = new wxBoxSizer(wxHORIZONTAL);
   
   pStaticText = new wxStaticText(this, -1, _("Length (seconds):   from"),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Length->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pTextCtrl_FromLength = 
		new wxTextCtrl(this, ID_TEXT_FROMLENGTH, "", 
							wxDefaultPosition, wxSize(48, -1), 
							wxTE_READONLY); // Read only because it's from the selection.
							// No validator because it's read only.
   pBoxSizer_Length->Add(m_pTextCtrl_FromLength, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pStaticText = new wxStaticText(this, -1, _("to"),
									       wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_Length->Add(pStaticText, 0, 
									wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 4);

   m_pTextCtrl_ToLength =
       new wxTextCtrl(this, ID_TEXT_TOLENGTH, "", 
								wxDefaultPosition, wxSize(48, -1), 0,
								wxTextValidator(wxFILTER_NUMERIC));
   pBoxSizer_Length->Add(m_pTextCtrl_ToLength, 0, 
								wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_Dialog->Add(pBoxSizer_Length, 0, wxALIGN_CENTER | wxALL, 4);


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

bool ChangeTempoDialog::Validate()
{
   return true; 
}

bool ChangeTempoDialog::TransferDataToWindow()
{
   m_bLoopDetect = true;

	// percent change controls
	this->Update_Text_PercentChange();
	this->Update_Slider_PercentChange();

	// from/to BPM controls
	wxString str;
	if (m_pTextCtrl_FromBPM) {
		if (m_FromBPM != 0)
			str.Printf("%d", m_FromBPM);
		else
			str = "";
		m_pTextCtrl_FromBPM->SetValue(str);
	}
	if (m_pTextCtrl_ToBPM) {
		if (m_ToBPM != 0)
			str.Printf("%d", m_ToBPM);
		else
			str = "";
		m_pTextCtrl_ToBPM->SetValue(str);
	}

	// from/to Length controls
	if (m_pTextCtrl_FromLength) {
		str.Printf("%.2f", m_FromLength);
		m_pTextCtrl_FromLength->SetValue(str);
		m_pTextCtrl_FromLength->Enable(false); // Disable because the value comes from the user selection.
	}
	if (m_pTextCtrl_ToLength) {
		str.Printf("%.2f", m_ToLength);
		m_pTextCtrl_ToLength->SetValue(str);
	}

   m_bLoopDetect = false;

	return true;
}

bool ChangeTempoDialog::TransferDataFromWindow()
{
	wxString str;

	// percent change controls
   if (m_pTextCtrl_PercentChange) {
      str = m_pTextCtrl_PercentChange->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
		m_PercentChange = newValue;
	}

	// Ignore Slider_PercentChange because TextCtrl_PercentChange 
	// always tracks it & is more precise (decimal points).

	// from/to BPM controls
   long newLong;
   if (m_pTextCtrl_FromBPM) {
      str = m_pTextCtrl_FromBPM->GetValue();
      str.ToLong(&newLong);
		m_FromBPM = (unsigned int)(newLong);
	}
   if (m_pTextCtrl_ToBPM) {
      str = m_pTextCtrl_ToBPM->GetValue();
      str.ToLong(&newLong);
		m_ToBPM = (unsigned int)(newLong);
	}

	// from/to Length controls
   // Don't do m_pTextCtrl_ToLength. It's disabled.
   if (m_pTextCtrl_ToLength) {
      str = m_pTextCtrl_ToLength->GetValue();
      str.ToLong(&newLong);
		m_ToLength = (int)(newLong);
	}

   return true;
}

// handler implementations for ChangeTempoDialog

void ChangeTempoDialog::OnText_PercentChange(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_PercentChange) {
      wxString str = m_pTextCtrl_PercentChange->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
		m_PercentChange = newValue;

      m_bLoopDetect = true;
		this->Update_Slider_PercentChange();
		this->Update_Text_ToBPM();
		this->Update_Text_ToLength();
      m_bLoopDetect = false;

      FindWindow(wxID_OK)->Enable(m_PercentChange > -100.0);
   }
}

void ChangeTempoDialog::OnSlider_PercentChange(wxCommandEvent & event)
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
		this->Update_Text_ToBPM();
		this->Update_Text_ToLength();
	   m_bLoopDetect = false;
	}
}

void ChangeTempoDialog::OnText_FromBPM(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_FromBPM) {
      wxString str = m_pTextCtrl_FromBPM->GetValue();
      long newValue;
      str.ToLong(&newValue);
		m_FromBPM = (unsigned int)(newValue);

      m_bLoopDetect = true;

		this->Update_Text_ToBPM();

      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnText_ToBPM(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_ToBPM) {
      wxString str = m_pTextCtrl_ToBPM->GetValue();
      long newValue;
      str.ToLong(&newValue);
		m_ToBPM = (unsigned int)(newValue);

      m_bLoopDetect = true;

		// If FromBPM has already been set, then there's a new percent change.
		if (m_FromBPM != 0) {
			m_PercentChange = (((double)(m_ToBPM) * 100.0) / (double)(m_FromBPM)) - 100.0;

			this->Update_Text_PercentChange();
			this->Update_Slider_PercentChange();

			this->Update_Text_ToLength();
		}
      
      m_bLoopDetect = false;
   }
}

void ChangeTempoDialog::OnText_ToLength(wxCommandEvent & event)
{
   if (m_bLoopDetect)
      return;

   if (m_pTextCtrl_ToLength) {
      wxString str = m_pTextCtrl_ToLength->GetValue();
      double newValue = 0;
      str.ToDouble(&newValue);
		m_ToLength = newValue;

		m_PercentChange = ((m_FromLength * 100.0) / m_ToLength) - 100.0;

      m_bLoopDetect = true;

		this->Update_Text_PercentChange();
		this->Update_Slider_PercentChange();

		this->Update_Text_ToBPM();
      
      m_bLoopDetect = false;
   }
}


void ChangeTempoDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();

	// Save & restore parameters around Preview, because we didn't do OK.
	double oldPercentChange = m_pEffect->m_PercentChange;
	m_pEffect->m_PercentChange = m_PercentChange;
	m_pEffect->Preview();
	m_pEffect->m_PercentChange = oldPercentChange;
}

void ChangeTempoDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();
   
   if (Validate()) 
      EndModal(true);
   else 
      event.Skip();
}

void ChangeTempoDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}


// helper fns

void ChangeTempoDialog::Update_Text_PercentChange()
{
	if (m_pTextCtrl_PercentChange) {
		wxString str;
		str.Printf("%.3f", m_PercentChange);
		m_pTextCtrl_PercentChange->SetValue(str);
	}
}

void ChangeTempoDialog::Update_Slider_PercentChange()
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

void ChangeTempoDialog::Update_Text_ToBPM() 
// Use m_FromBPM & m_PercentChange to set new m_ToBPM & control.
{
   // Update ToBPM iff FromBPM has been set.
	if (m_FromBPM == 0) 
		return;

	m_ToBPM = (unsigned int)((((double)(m_FromBPM) * 
											(100.0 + m_PercentChange)) / 100.0) + 
										0.5); // Add 0.5 so trunc -> round.
	if (m_pTextCtrl_ToBPM) {
		wxString str;
		str.Printf("%d", m_ToBPM);
		m_pTextCtrl_ToBPM->SetValue(str);
	}
}

void ChangeTempoDialog::Update_Text_ToLength() 
// Use m_FromLength & m_PercentChange to set new m_ToLength & control.
{
	m_ToLength = (m_FromLength * 100.0) / (100.0 + m_PercentChange);
	if (m_pTextCtrl_ToLength) {
		wxString str;
		str.Printf("%.2f", m_ToLength);
		m_pTextCtrl_ToLength->SetValue(str);
	}
}

#endif // USE_SOUNDTOUCH
