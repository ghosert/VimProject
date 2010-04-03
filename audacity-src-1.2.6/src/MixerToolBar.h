/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolbar.h
 
  Dominic Mazzoni
 
**********************************************************************/

#ifndef __AUDACITY_MIXER_TOOLBAR__
#define __AUDACITY_MIXER_TOOLBAR__

#include "ToolBar.h"

class MixerToolBar;
class ToolBar;
class ToolBarFrame;
class ASlider;

class wxImage;
class wxSize;
class wxPoint;
class wxChoice;

class MixerToolBar:public ToolBar {
 public:
   MixerToolBar() {};
   MixerToolBar(wxWindow * parent, wxWindowID id,
                const wxPoint & pos, const wxSize & size);
   MixerToolBar(wxWindow * parent);
   virtual ~ MixerToolBar();

   void RecreateTipWindows();
   void UpdateControls();
   void SetMixer(wxCommandEvent &event);

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnKeyEvent(wxKeyEvent & event);
   virtual void EnableDisableButtons();

   void OnSlider(wxCommandEvent & event);

 private:

   void InitializeMixerToolBar();

   wxBitmap *mPlayBitmap;
   wxBitmap *mRecordBitmap;

   wxChoice *mInputSourceChoice;

   ASlider *mInputSlider;
   ASlider *mOutputSlider;

   DECLARE_EVENT_TABLE()
};

MixerToolBar *GetCurrentMixerToolBar();

#endif
