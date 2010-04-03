/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.cpp

  Joshua Haberman

**********************************************************************/

#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/stattext.h>
#include <wx/sizer.h>
#include <wx/intl.h>
#include <wx/textctrl.h>

#include "../Audacity.h"
#include "../Prefs.h"
#include "../SampleFormat.h"
#include "../Dither.h"
#include "../AudioIO.h"
#include "QualityPrefs.h"
#include "../Resample.h"

int formats[] = {
   int16Sample,
   int24Sample,
   floatSample
};

wxString stringFormats[] = {
   "16-bit",
   "24-bit",
   "32-bit float"
};

#define NUM_FORMATS 3

#define ID_SAMPLE_RATE_CHOICE           7001

BEGIN_EVENT_TABLE(QualityPrefs, wxPanel)
   EVT_CHOICE(ID_SAMPLE_RATE_CHOICE,   QualityPrefs::OnSampleRateChoice)
END_EVENT_TABLE()

QualityPrefs::QualityPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   int i;
   
   // XXX: This should use a previously changed, but not yet saved
   //      sound card setting from the "I/O" preferences tab.
   wxArrayLong sampleRates = AudioIO::GetSupportedSampleRates();
   
   int rate =
       gPrefs->Read("/SamplingRate/DefaultProjectSampleRate",
                    AudioIO::GetOptimalSupportedSampleRate());

   int format =
      gPrefs->Read("/SamplingRate/DefaultProjectSampleFormat", floatSample);

   int pos = sampleRates.GetCount(); // Fall back to "Other..."
   for (i = 0; i < (int)sampleRates.GetCount(); i++)
      if (rate == sampleRates[i]) {
         pos = i;
         break;
      }

   int fmtpos = NUM_FORMATS-1;
   for (i = 0; i < NUM_FORMATS; i++)
      if (format == formats[i]) {
         fmtpos = i;
         break;
      }

    topSizer = new wxBoxSizer( wxHORIZONTAL );

   {
      topSizer->Add(
         new wxStaticText(this, -1, _("Default Sample Rate:")), 0, 
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);
         
      wxString *stringRates = new wxString[sampleRates.GetCount() + 1];
      
      for (i = 0; i < (int)sampleRates.GetCount(); i++)
      {
         int sampleRate = (int)sampleRates[i];
         stringRates[i] = wxString::Format("%i Hz", sampleRate);
      }
      
      stringRates[sampleRates.GetCount()] = _("Other...");

      mSampleRates = new wxChoice(this, ID_SAMPLE_RATE_CHOICE, wxDefaultPosition, wxDefaultSize,
                                 (int)sampleRates.GetCount() + 1, stringRates);
      mSampleRates->SetSelection(pos);

      topSizer->Add( mSampleRates, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );

      mOtherSampleRate = NULL;
      mOtherSampleRate = new wxTextCtrl(
         this, -1, wxString::Format("%i", rate),
         wxDefaultPosition, wxSize(50, -1), 0 );

      mOtherSampleRate->Enable(pos == (int)sampleRates.GetCount() + 1);

      topSizer->Add( mOtherSampleRate, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
      delete[] stringRates;
   }

    wxBoxSizer *top2Sizer = new wxBoxSizer( wxHORIZONTAL );

   {
      top2Sizer->Add(
         new wxStaticText(this, -1, _("Default Sample Format:")), 0, 
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

      mSampleFormats = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                                 NUM_FORMATS, stringFormats);
      mSampleFormats->SetSelection(fmtpos);

      top2Sizer->Add( mSampleFormats, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
   }

   int converterHQ = Resample::GetBestMethod();
   int converter = Resample::GetFastMethod();
   int numConverters = Resample::GetNumMethods();

   wxString *converterStrings;
   converterStrings = new wxString[numConverters];
   for(i=0; i<numConverters; i++)
      converterStrings[i] = Resample::GetMethodName(i);

   wxBoxSizer *top3Sizer = new wxBoxSizer( wxHORIZONTAL );

   top3Sizer->Add(
         new wxStaticText(this, -1, _("Real-time sample rate converter:")), 0, 
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   mConverters = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                              numConverters, converterStrings);
   mConverters->SetSelection(converter);
   top3Sizer->Add(mConverters, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );

   wxBoxSizer *top4Sizer = new wxBoxSizer( wxHORIZONTAL );

   top4Sizer->Add(
         new wxStaticText(this, -1, _("High-quality sample rate converter:")), 0, 
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);

   mHQConverters = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                                numConverters, converterStrings);
   mHQConverters->SetSelection(converterHQ);
   top4Sizer->Add(mHQConverters, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER );
   
   delete[] converterStrings;

   // These ditherers are currently defined
   int numDithers = 4;
   wxString ditherStrings[4];
   ditherStrings[Dither::none] = _("None");
   ditherStrings[Dither::rectangle] = _("Rectangle");
   ditherStrings[Dither::triangle] = _("Triangle");
   ditherStrings[Dither::shaped] = _("Shaped");
   
   // Low-quality dithering option
   int dither = gPrefs->Read("/Quality/DitherAlgorithm", (long)Dither::none);
   
   wxBoxSizer *top5Sizer = new wxBoxSizer(wxHORIZONTAL);
   
   top5Sizer->Add(
         new wxStaticText(this, -1, _("Real-time dither:")), 0,
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);
   
   mDithers = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                           numDithers, ditherStrings);
   mDithers->SetSelection(dither);
   
   top5Sizer->Add(mDithers, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER);
   
   // High quality dithering option
   int ditherHQ = gPrefs->Read("/Quality/HQDitherAlgorithm", (long)Dither::triangle);;
   
   wxBoxSizer *top6Sizer = new wxBoxSizer(wxHORIZONTAL);
   
   top6Sizer->Add(
         new wxStaticText(this, -1, _("High-quality dither:")), 0,
         wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, GENERIC_CONTROL_BORDER);
   
   mHQDithers = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                             numDithers, ditherStrings);
   mHQDithers->SetSelection(ditherHQ);
   
   top6Sizer->Add(mHQDithers, 0, wxALL|wxALIGN_CENTER_VERTICAL, TOP_LEVEL_BORDER);
   
   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);
   outSizer->Add(top2Sizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   outSizer->Add(top3Sizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);
   outSizer->Add(top4Sizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   outSizer->Add(top5Sizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);
   outSizer->Add(top6Sizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
   SetSizer(outSizer);

}

bool QualityPrefs::Apply()
{
   long rate = 44100;
   long format = floatSample;
   int sel = mSampleRates->GetSelection();
   int fmtsel = mSampleFormats->GetSelection();
   
   if (sel < mSampleRates->GetCount()-1)
      (mSampleRates->GetString(sel)).ToLong(&rate);
   else
      (mOtherSampleRate->GetValue()).ToLong(&rate);

   gPrefs->Write("/SamplingRate/DefaultProjectSampleRate", rate);

   format = formats[fmtsel];

   gPrefs->Write("/SamplingRate/DefaultProjectSampleFormat", format);

   /* Audacity will automatically re-read this value whenever a new project
    * is created, so don't bother making it do so now... */

   int converter = mConverters->GetSelection();
   int converterHQ = mHQConverters->GetSelection();
   Resample::SetFastMethod(mConverters->GetSelection());
   Resample::SetBestMethod(mHQConverters->GetSelection());

   // Save dither options
   int dither = mDithers->GetSelection();
   int ditherHQ = mHQDithers->GetSelection();
   gPrefs->Write("/Quality/HQDitherAlgorithm", (long)ditherHQ);
   gPrefs->Write("/Quality/DitherAlgorithm", (long)dither);
   
   // Tell CopySamples() to use these ditherers now
   InitDitherers();

   return true;
}

void QualityPrefs::OnSampleRateChoice(wxCommandEvent& evt)
{
   int sel = mSampleRates->GetSelection();

   mOtherSampleRate->Enable(sel == mSampleRates->GetCount() - 1);
}


QualityPrefs::~QualityPrefs()
{
}


