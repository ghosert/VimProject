/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/colordlg.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "../Prefs.h"
#include "SpectrumPrefs.h"

int numFFTSizes = 7;

int FFTSizes[] = {
   64,
   128,
   256,
   512,
   1024,
   2048,
   4096
};

wxString stringFFTSizes[] = {
   wxTRANSLATE("64 - most wideband"),
               "128",
   wxTRANSLATE("256 - default"),
               "512",
               "1024",
               "2048",
   wxTRANSLATE("4096 - most narrowband")
};

SpectrumPrefs::SpectrumPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   int fftSize = gPrefs->Read("/Spectrum/FFTSize", 256L);
   bool isGrayscale = false;
   gPrefs->Read("/Spectrum/Grayscale", &isGrayscale, false);

   int i;
   int maxFreq = gPrefs->Read("/Spectrum/MaxFreq", 8000L);
   wxString maxFreqStr;
   maxFreqStr.Printf("%d", maxFreq);

   int pos = 3;                 // Fall back to 256 if it doesn't match anything else
   for (i = 0; i < numFFTSizes; i++)
      if (fftSize == FFTSizes[i]) {
         pos = i;
         break;
      }

   topSizer = new wxBoxSizer( wxVERTICAL );

   {
      wxStaticBoxSizer *fftSizeSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1, _("FFT Size")),
         wxVERTICAL);

      mFFTSize[0] = new wxRadioButton(
         this, -1, _(stringFFTSizes[0]), wxDefaultPosition,
         wxDefaultSize, wxRB_GROUP );
      mFFTSize[0]->SetValue(false);
      fftSizeSizer->Add(mFFTSize[0], 0, 
         wxGROW|wxLEFT|wxRIGHT, RADIO_BUTTON_BORDER );

      for(i = 1; i < numFFTSizes; i++) {
         mFFTSize[i] = new wxRadioButton(this, -1, _(stringFFTSizes[i]));
         mFFTSize[i]->SetValue(false);
         fftSizeSizer->Add(mFFTSize[i], 0,
            wxGROW|wxLEFT|wxRIGHT, RADIO_BUTTON_BORDER );
      }

      mFFTSize[pos]->SetValue(true);
            
      topSizer->Add( fftSizeSizer, 0, 
         wxGROW|wxALL, TOP_LEVEL_BORDER );
   }

   {
      mGrayscale  = new wxCheckBox(this, -1, _("Grayscale"));
      topSizer->Add(mGrayscale, 0,
         wxGROW|wxALL, RADIO_BUTTON_BORDER );
      
      if(isGrayscale)
         mGrayscale->SetValue(true);

      wxBoxSizer *freqSizer = new wxBoxSizer( wxHORIZONTAL );

      freqSizer->Add(
         new wxStaticText(this, -1, _("Maximum Frequency (Hz):")),
         0, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL|wxALL, GENERIC_CONTROL_BORDER );

      mMaxFreqCtrl = new wxTextCtrl( this, -1, maxFreqStr,
         wxDefaultPosition, wxSize(80,-1));
      freqSizer->Add(mMaxFreqCtrl, 0, wxALIGN_CENTER_VERTICAL|wxALL,
                     GENERIC_CONTROL_BORDER );

      topSizer->Add(freqSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER );
   }

   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
   SetSizer(outSizer);
} 

bool SpectrumPrefs::Apply()
{
   /*
      wxColourDialog dlog(this);
      dlog.ShowModal();
    */
  
   int pos = 0;
   
   for(int i = 0; i < numFFTSizes; i++)
      if(mFFTSize[i]->GetValue()) {
         pos = i;
         break;
      }

   long fftSize = FFTSizes[pos];
   gPrefs->Write("/Spectrum/FFTSize", fftSize);

   bool isGrayscale = mGrayscale->GetValue();
   gPrefs->Write("/Spectrum/Grayscale", isGrayscale);

   wxString maxFreqStr = mMaxFreqCtrl->GetValue();
   long maxFreq;
   if (!maxFreqStr.ToLong(&maxFreq)) {
      wxMessageBox(_("The maximum frequency must be an integer"));
      return false;
   }
   if (maxFreq < 100 || maxFreq > 100000) {
      wxMessageBox(_("Maximum frequency must be in the range "
                     "100 Hz - 100,000 Hz"));
      return false;
   }
   gPrefs->Write("/Spectrum/MaxFreq", maxFreq);

   // TODO: Force all projects to repaint themselves

   return true;

}


SpectrumPrefs::~SpectrumPrefs()
{
}
