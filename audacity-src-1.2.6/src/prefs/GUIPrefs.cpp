/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/intl.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/radiobut.h>

#include "GUIPrefs.h"
#include "../Prefs.h"
#include "../Languages.h"
#include "../EditToolBar.h"
#include "../Envelope.h"


GUIPrefs::GUIPrefs(wxWindow * parent):
PrefsPanel(parent)
{
   // Scrolling
   bool autoscroll, spectrogram, editToolBar,mixerToolBar, alwaysEnablePause,
      quitOnClose, adjustSelectionEdges, ergonomicTransportButtons,
	   meterToolBar, alwaysEnablePlay;
   int envdBRange;

   gPrefs->Read("/GUI/AutoScroll", &autoscroll, true);
   gPrefs->Read("/GUI/UpdateSpectrogram", &spectrogram, true);

   gPrefs->Read("/GUI/EnableEditToolBar", &editToolBar, true);
   gPrefs->Read("/GUI/EnableMixerToolBar", &mixerToolBar, true);
   gPrefs->Read("/GUI/EnableMeterToolBar", &meterToolBar, true);
   gPrefs->Read("/GUI/AlwaysEnablePause", &alwaysEnablePause, false);
   gPrefs->Read("/GUI/AlwaysEnablePlay", &alwaysEnablePlay, false);

   // Code duplication warning: this default is repeated in Project.cpp
   // in the destructor.  -DMM
   #ifdef __WXMAC__
   bool defaultQuitOnClose = false;
   #else
   bool defaultQuitOnClose = true;
   #endif
   // End code duplication warning

   gPrefs->Read("/GUI/QuitOnClose", &quitOnClose, defaultQuitOnClose);
   gPrefs->Read("/GUI/AdjustSelectionEdges", &adjustSelectionEdges, true);
   gPrefs->Read("/GUI/ErgonomicTransportButtons", &ergonomicTransportButtons, false);
   gPrefs->Read("/GUI/EnvdBRange", &envdBRange, ENV_DB_RANGE);

   topSizer = new wxBoxSizer( wxVERTICAL );
   
   //Auto-scrolling preference
   mAutoscroll = new wxCheckBox(this, -1, _("Autoscroll while playing"));
   mAutoscroll->SetValue(autoscroll);
   topSizer->Add(mAutoscroll, 0, wxGROW|wxALL, 2);

   //Always enable pause preference
   mAlwaysEnablePause = new wxCheckBox(this, -1, _("Always allow pausing"));
   mAlwaysEnablePause->SetValue(alwaysEnablePause);
   topSizer->Add(mAlwaysEnablePause, 0, wxGROW|wxALL, 2);

   // Always enable play
   mAlwaysEnablePlay = new wxCheckBox(this, -1, _("Always allow playing"));
   mAlwaysEnablePlay->SetValue(alwaysEnablePlay);
   topSizer->Add(mAlwaysEnablePlay, 0, wxGROW|wxALL, 2);

   //Spectrogram preference
   mSpectrogram = new wxCheckBox(this, -1, _("Update spectrogram while playing"));
   mSpectrogram->SetValue(spectrogram);
   topSizer->Add(mSpectrogram, 0, wxGROW|wxALL, 2);

   //Enable Edit toolbar preference

   mEditToolBar = new wxCheckBox(this, -1, _("Enable Edit Toolbar"));
   mEditToolBar->SetValue(editToolBar);
   topSizer->Add(mEditToolBar, 0, wxGROW|wxALL, 2);

   //Enable Mixer toolbar preference
   mMixerToolBar = new wxCheckBox(this, -1, _("Enable Mixer Toolbar"));
   mMixerToolBar->SetValue(mixerToolBar);
   topSizer->Add(mMixerToolBar, 0, wxGROW|wxALL, 2);

   //Enable Meter toolbar preference
   mMeterToolBar = new wxCheckBox(this, -1, _("Enable Meter Toolbar"));
   mMeterToolBar->SetValue(meterToolBar);
   topSizer->Add(mMeterToolBar, 0, wxGROW|wxALL, 2);


   // Quit Audacity when last window closes?
   mQuitOnClose = new wxCheckBox(this, -1,
                                 _("Quit Audacity upon closing last window"));
   mQuitOnClose->SetValue(quitOnClose);
   topSizer->Add(mQuitOnClose, 0, wxGROW|wxALL, 2);

   // Enable/disable adjust selection edges
   mAdjustSelectionEdges =
      new wxCheckBox(this, -1,
                     _("Enable dragging of left and right selection edges"));
   mAdjustSelectionEdges->SetValue(adjustSelectionEdges);
   topSizer->Add(mAdjustSelectionEdges, 0, wxGROW|wxALL, 2);

   // Enable/disable ergonomic order of transport buttons
   mErgonomicTransportButtons =
      new wxCheckBox(this, -1,
                     _("Enable ergonomic order of Audio I/O buttons"));
   mErgonomicTransportButtons->SetValue(ergonomicTransportButtons);
   topSizer->Add(mErgonomicTransportButtons, 0, wxGROW|wxALL, 2);

   // Locale
   GetLanguages(mLangCodes, mLangNames);
   int numLangs = mLangNames.GetCount();

   wxString currentLang = gPrefs->Read("/Locale/Language", "en");
   wxString *langArray = new wxString[numLangs];
   int i;
   for(i=0; i<numLangs; i++)
      langArray[i] = mLangNames[i];
   mLocale = new wxChoice(this, -1, wxDefaultPosition, wxDefaultSize,
                          numLangs, langArray);
   mLocale->SetSelection(0); // in case nothing else matches
   delete[] langArray;
   for(i=0; i<numLangs; i++)
      if (mLangCodes[i] == currentLang)
         mLocale->SetSelection(i);

   mLocaleLabel = new wxStaticText(this, -1, _("Language:"));

   wxFlexGridSizer *localeSizer = new wxFlexGridSizer( 0, 2, 0, 0 );
   localeSizer->Add(mLocaleLabel, 0, wxALIGN_LEFT|wxALL|wxALIGN_CENTER_VERTICAL, 2);
   localeSizer->Add(mLocale, 1, wxGROW|wxALL|wxALIGN_CENTER_VERTICAL, 2 );
   topSizer->Add(localeSizer, 0, wxGROW|wxALL, 2);
   
   // dB range display setup
   {
      wxStaticBoxSizer *dbRangeSizer = new wxStaticBoxSizer(
         new wxStaticBox(this, -1,
            _("Minimum of dB mode display range")),
         wxVERTICAL );

      
      mdBArray[0] = new wxRadioButton(
	 this, -1, _("-36 dB (shallow range for high-amplitude editing)"),
         wxDefaultPosition, wxDefaultSize, wxRB_GROUP );

      dbRangeSizer->Add( mdBArray[0], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER );

      mdBArray[1] = new wxRadioButton(
	 this, -1, _("-48 dB (PCM range of 8 bit samples)"),
         wxDefaultPosition, wxDefaultSize, 0 );

      dbRangeSizer->Add( mdBArray[1], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER );

      mdBArray[2] = new wxRadioButton(
	 this, -1, _("-96 dB (PCM range of 16 bit samples)"),
         wxDefaultPosition, wxDefaultSize, 0 );

      dbRangeSizer->Add( mdBArray[2], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER );

      mdBArray[3] = new wxRadioButton(
	 this, -1, _("-120 dB (approximate limit of human hearing)"),
         wxDefaultPosition, wxDefaultSize, 0 );
          
      dbRangeSizer->Add( mdBArray[3], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER );

      mdBArray[4] = new wxRadioButton(
	 this, -1, _("-145 dB (PCM range of 24 bit samples)"),
         wxDefaultPosition, wxDefaultSize, 0 );

      dbRangeSizer->Add( mdBArray[4], 0,
         wxGROW|wxLEFT | wxRIGHT, RADIO_BUTTON_BORDER );

      mdBArray[0]->SetValue(false);
      mdBArray[1]->SetValue(false);
      mdBArray[2]->SetValue(false);
      mdBArray[3]->SetValue(false);
      mdBArray[4]->SetValue(false);

      if(envdBRange<=36){
	mdBArray[0]->SetValue(true);
      }else if(envdBRange<=48){
	mdBArray[1]->SetValue(true);
      }else if(envdBRange<=96){
	mdBArray[2]->SetValue(true);
      }else if(envdBRange<=120){
	mdBArray[3]->SetValue(true);
      }else{
	mdBArray[4]->SetValue(true);
      }

      topSizer->Add( dbRangeSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER );
   }

   // Finish layout
   outSizer = new wxBoxSizer( wxVERTICAL );
   outSizer->Add(topSizer, 0, wxGROW|wxALL, TOP_LEVEL_BORDER);

   SetAutoLayout(true);
   SetSizer(outSizer);

   outSizer->Fit(this);
   outSizer->SetSizeHints(this);
}

bool GUIPrefs::Apply()
{
   gPrefs->Write("/GUI/AutoScroll", mAutoscroll->GetValue());
   gPrefs->Write("/GUI/UpdateSpectrogram", mSpectrogram->GetValue());
   gPrefs->Write("/GUI/AlwaysEnablePause", mAlwaysEnablePause->GetValue());
   gPrefs->Write("/GUI/AlwaysEnablePlay", mAlwaysEnablePlay->GetValue());

   gPrefs->Write("/GUI/QuitOnClose", mQuitOnClose->GetValue());
   gPrefs->Write("/GUI/AdjustSelectionEdges",
                 mAdjustSelectionEdges->GetValue());
   gPrefs->Write("/GUI/ErgonomicTransportButtons",
                 mErgonomicTransportButtons->GetValue());

   //-------------------------------------------------------------
   //---------------------- Edit toolbar loading/unloading
   gPrefs->Write("/GUI/EnableEditToolBar", mEditToolBar->GetValue());
   bool enable = mEditToolBar->GetValue();
 
   if(gEditToolBarStub && !enable) {
      gEditToolBarStub->UnloadAll();    //Unload all docked EDIT toolbars
      delete gEditToolBarStub;
      gEditToolBarStub = NULL;
   }
   else if(!gEditToolBarStub && enable)
      {  
         gEditToolBarStub =  new ToolBarStub(gParentWindow, EditToolBarID);
         gEditToolBarStub->LoadAll();
      }
   //----------------------End of Edit toolbar loading/unloading
   //---------------------------------------------------------------


   //-------------------------------------------------------------
   //---------------------- Mixer toolbar loading/unloading
   gPrefs->Write("/GUI/EnableMixerToolBar", mMixerToolBar->GetValue());
   enable = mMixerToolBar->GetValue();
   
   if(gMixerToolBarStub && !enable) {
      gMixerToolBarStub->UnloadAll();    //Unload all docked MIXER toolbars
      delete gMixerToolBarStub;
      gMixerToolBarStub = NULL;
   }
   else if(!gMixerToolBarStub && enable) {
      gMixerToolBarStub =  new ToolBarStub(gParentWindow, MixerToolBarID);
      gMixerToolBarStub->LoadAll();
   }
   //----------------------End of Mixer toolbar loading/unloading
   //---------------------------------------------------------------

   //-------------------------------------------------------------
   //---------------------- Meter toolbar loading/unloading
   gPrefs->Write("/GUI/EnableMeterToolBar", mMeterToolBar->GetValue());
   enable = mMeterToolBar->GetValue();
   
   if(gMeterToolBarStub && !enable) {
      gMeterToolBarStub->UnloadAll();    //Unload all docked METER toolbars
      delete gMeterToolBarStub;
      gMeterToolBarStub = NULL;
   }
   else if(!gMeterToolBarStub && enable) {
      gMeterToolBarStub =  new ToolBarStub(gParentWindow, MeterToolBarID);
      gMeterToolBarStub->LoadAll();
   }
   //----------------------End of Meter toolbar loading/unloading
   //---------------------------------------------------------------


   int localeIndex = mLocale->GetSelection();
   if (localeIndex >= 0 && localeIndex < mLangCodes.GetCount())
      gPrefs->Write("/Locale/Language", mLangCodes[localeIndex]);

   int envdBRange=36;
   if(mdBArray[1]->GetValue())envdBRange=48;
   if(mdBArray[2]->GetValue())envdBRange=96;
   if(mdBArray[3]->GetValue())envdBRange=120;
   if(mdBArray[4]->GetValue())envdBRange=145;
   gPrefs->Write("/GUI/EnvdBRange", envdBRange);

   return true;
}

GUIPrefs::~GUIPrefs()
{
}
