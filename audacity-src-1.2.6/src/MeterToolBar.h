/**********************************************************************

  Audacity: A Digital Audio Editor
  
  MeterToolbar.h
 
  Dominic Mazzoni
 
  ToolBar to hold the VU Meter

**********************************************************************/

#ifndef __AUDACITY_METER_TOOLBAR__
#define __AUDACITY_METER_TOOLBAR__

#include "ToolBar.h"

class Meter;

class MeterToolBar:public ToolBar {
 public:
   MeterToolBar() {};
   MeterToolBar(wxWindow * parent, wxWindowID id,
               const wxPoint & pos, const wxSize & size);
   MeterToolBar(wxWindow * parent);
   virtual ~ MeterToolBar();
   void InitializeMeterToolBar();

   virtual void OnSize(wxSizeEvent & event);
   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnKeyEvent(wxKeyEvent & event);
   virtual void EnableDisableButtons();

   void GetMeters(Meter **playMeter, Meter **recordMeter) {
      *playMeter = mPlayMeter;
      *recordMeter = mRecordMeter;
   }

 private:

   Meter *mPlayMeter;
   Meter *mRecordMeter;

   DECLARE_EVENT_TABLE()
};

#endif
