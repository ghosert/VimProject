/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolBar.cpp

  Dominic Mazzoni
 
  See MixerToolBar.h for details

**********************************************************************/

#include "MixerToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include <wx/defs.h>
#include <wx/choice.h>
#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/brush.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/settings.h>
#endif

#include <wx/image.h>
#include <wx/tooltip.h>

#include "Audacity.h"
#include "AudioIO.h"
#include "ImageManipulation.h"
#include "widgets/ASlider.h"
#include "Prefs.h"
#include "Project.h"
#include "AColor.h"

#if USE_PORTMIXER
#include "AudioIO.h"
#endif

#include "../images/MixerImages.h"

MixerToolBar *GetCurrentMixerToolBar()
{
   AudacityProject *project = GetActiveProject();

   if (project)
      return project->GetMixerToolBar();
   else
      return NULL;
}

////////////////////////////////////////////////////////////
/// Methods for MixerToolBar
////////////////////////////////////////////////////////////

enum {
   FirstID = 2000,
   OutputVolumeID,
   InputVolumeID,
   InputSourceID
};


BEGIN_EVENT_TABLE(MixerToolBar, wxWindow)
   EVT_PAINT(MixerToolBar::OnPaint)
   EVT_SLIDER(OutputVolumeID, MixerToolBar::SetMixer)
   EVT_SLIDER(InputVolumeID, MixerToolBar::SetMixer)
   EVT_CHOICE(InputSourceID, MixerToolBar::SetMixer)
END_EVENT_TABLE()

//Standard contructor
MixerToolBar::MixerToolBar(wxWindow * parent):
   ToolBar(parent, -1, wxPoint(1, 1), wxSize(500, 27))
{
   InitializeMixerToolBar();
}

//Another constructor
MixerToolBar::MixerToolBar(wxWindow * parent, wxWindowID id,
                           const wxPoint & pos,
                           const wxSize & size):ToolBar(parent, id,
                                                        pos, size)
{
   InitializeMixerToolBar();
}


// This sets up the MixerToolBar, initializing all the important values
// and creating the buttons.
void MixerToolBar::InitializeMixerToolBar()
{
   mIdealSize = wxSize(500, 27);
   mTitle = _("Audacity Mixer Toolbar");
   mType = MixerToolBarID;
   int offset;

   #ifdef __WXMAC__
   offset = 0;
   #else
   offset = 1;
   #endif

   wxColour backgroundColour =
       wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   wxColour origColour(204, 204, 204);

   wxImage *speaker = new wxImage(wxBitmap(Speaker).ConvertToImage());
   wxImage *speakerAlpha = new wxImage(wxBitmap(SpeakerAlpha).ConvertToImage());
   wxImage *bkgnd = CreateSysBackground(25, 25, 1,
                                        backgroundColour);
   wxImage *speakerFinal = OverlayImage(bkgnd, speaker, speakerAlpha, 0, 0);
   wxImage *mic = new wxImage(wxBitmap(Mic).ConvertToImage());
   wxImage *micAlpha = new wxImage(wxBitmap(MicAlpha).ConvertToImage());
   wxImage *micFinal = OverlayImage(bkgnd, mic, micAlpha, 0, 0);

   mPlayBitmap = new wxBitmap(speakerFinal);
   mRecordBitmap = new wxBitmap(micFinal);

   delete speaker;
   delete speakerAlpha;
   delete bkgnd;
   delete speakerFinal;
   delete mic;
   delete micAlpha;
   delete micFinal;

   mOutputSlider = new ASlider(this, OutputVolumeID, _("Output Volume"),
                               wxPoint(30, offset), wxSize(130, 25));

   mInputSlider = new ASlider(this, InputVolumeID, _("Input Volume"),
                              wxPoint(210, offset), wxSize(130, 25));

   mInputSourceChoice = NULL;

   #if USE_PORTMIXER
   unsigned int    j;
   int leftPosition = 355;

   wxArrayString inputSources = gAudioIO->GetInputSourceNames();

   mInputSourceChoice = new wxChoice(this, InputSourceID,
                                     wxPoint(leftPosition, 2),
                                     wxSize(-1, 23));
   for(j = 0; j < inputSources.GetCount(); j++)
      mInputSourceChoice->Append(inputSources[j]);
   if (inputSources.GetCount() == 0)
      mInputSourceChoice->Enable(false);
      


   // Set choice control to default value
   float inputVolume;
   float playbackVolume;
   int inputSource;
   gAudioIO->GetMixer(&inputSource, &inputVolume, &playbackVolume);
   if (inputSource >= 0)
     mInputSourceChoice->SetSelection(inputSource);
   else if (inputSources.GetCount() > 0)
     mInputSourceChoice->SetSelection(0);

   UpdateControls();
   #endif
}

MixerToolBar::~MixerToolBar()
{
   delete mPlayBitmap;
   delete mRecordBitmap;
   delete mInputSlider;
   delete mOutputSlider;
   if (mInputSourceChoice)
      delete mInputSourceChoice;
}

void MixerToolBar::RecreateTipWindows()
{
   // Hack to make sure they appear on top of other windows
   mInputSlider->RecreateTipWin();
   mOutputSlider->RecreateTipWin();
}

void MixerToolBar::UpdateControls()
{
#if USE_PORTMIXER
   float inputVolume;
   float playbackVolume;
   int inputSource;

   gAudioIO->GetMixer(&inputSource, &inputVolume, &playbackVolume);

   // This causes weird GUI behavior and isn't really essential.
   // We could enable it again later.
   //if (inputSource != mInputSourceChoice->GetSelection())
   //    mInputSourceChoice->SetSelection(inputSource);
   mOutputSlider->Set(playbackVolume);
   mInputSlider->Set(inputVolume);
#endif // USE_PORTMIXER
}

void MixerToolBar::SetMixer(wxCommandEvent &event)
{
#if USE_PORTMIXER
   float inputVolume = mInputSlider->Get();
   float outputVolume = mOutputSlider->Get();
   int inputSource = mInputSourceChoice->GetSelection();

   gAudioIO->SetMixer(inputSource, inputVolume, outputVolume);
#endif // USE_PORTMIXER
}

void MixerToolBar::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);
   int width, height;
   GetSize(&width, &height);

#ifdef __WXMAC__
   // Mac has an Aqua background...
   DrawBackground(dc, width, height); 
#else
   // Reduce width by 3 to visually separate from next 
   // Grab bar
   wxRect BevelRect( 0,0,width-3,height-1);
   AColor::Bevel( dc, true, BevelRect );
#endif

   dc.DrawBitmap(*mPlayBitmap, 1, 1);
   dc.DrawBitmap(*mRecordBitmap, 181, 1);
}

void MixerToolBar::OnKeyEvent(wxKeyEvent & event)
{
   event.Skip();
}

void MixerToolBar::EnableDisableButtons()
{
}
