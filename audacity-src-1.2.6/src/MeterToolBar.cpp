/**********************************************************************

  Audacity: A Digital Audio Editor

  MeterToolBar.cpp

  Dominic Mazzoni
 
  See MeterToolBar.h for details

**********************************************************************/

#include <wx/defs.h>
#include <wx/intl.h>

#include <wx/object.h> // tooltip.h needs this but doesn't include it.
#include <wx/window.h> // tooltip.h needs this but doesn't include it.
#include <wx/tooltip.h>
#include <wx/dcclient.h>

#include "MeterToolBar.h"

#include "widgets/Meter.h"

////////////////////////////////////////////////////////////
/// Methods for MeterToolBar
////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(MeterToolBar, wxWindow)
   EVT_SIZE(MeterToolBar::OnSize)
   EVT_PAINT(MeterToolBar::OnPaint)
   EVT_CHAR(MeterToolBar::OnKeyEvent)
END_EVENT_TABLE()

//Standard contructor
MeterToolBar::MeterToolBar(wxWindow * parent)
   : ToolBar(parent, -1, wxPoint(1, 1), wxSize(200, 55))
{
   InitializeMeterToolBar();
}

//Another constructor
MeterToolBar::MeterToolBar(wxWindow * parent, wxWindowID id,
                           const wxPoint & pos, const wxSize & size)
   : ToolBar(parent, id, pos, size)
{
   InitializeMeterToolBar();
}

// This sets up the MeterToolBar, initializing all the important values
// and creating the buttons.
void MeterToolBar::InitializeMeterToolBar()
{
   mIdealSize = wxSize(200, 55);
   mTitle = _("Audacity Meter Toolbar");
   mType = MeterToolBarID;

   mPlayMeter = new Meter(this, -1, false,
                          wxPoint(0, 0),
                          wxSize(97, 55));
   mRecordMeter = new Meter(this, -1, true,
                            wxPoint(98, 0),
                            wxSize(97, 55));

   #if wxUSE_TOOLTIPS
   mPlayMeter->SetToolTip(_("Output level meter"));
   mRecordMeter->SetToolTip(_("Input level meter - click to monitor input"));
   #endif
}

MeterToolBar::~MeterToolBar()
{
}

void MeterToolBar::OnSize(wxSizeEvent & evt)
{
   int width, height;
   GetClientSize(&width, &height);

   if (width > height && height > 120) {
      // Two stacked horizontal meters
      mPlayMeter->SetSize(0, 0, width-2, height/2 - 1);
      mRecordMeter->SetSize(0, height/2, width-2, height/2 - 1);
      mPlayMeter->SetStyle(Meter::HorizontalStereo);
      mRecordMeter->SetStyle(Meter::HorizontalStereo);
   }
   else if (width > height) {
      // Two horizontal, side-by-side
      mPlayMeter->SetSize(0, 0, width/2 - 3, height);
      mRecordMeter->SetSize(width/2-2, 0, width/2 - 3, height);
      mPlayMeter->SetStyle(Meter::HorizontalStereo);
      mRecordMeter->SetStyle(Meter::HorizontalStereo);
   }
   else {
      // Two vertical, side-by-side
      mPlayMeter->SetSize(0, 0, width/2 - 2, height);
      mRecordMeter->SetSize(width/2 - 1, 0, width/2 - 2, height);
      mPlayMeter->SetStyle(Meter::VerticalStereo);
      mRecordMeter->SetStyle(Meter::VerticalStereo);
   }
}

void MeterToolBar::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);

   DrawBackground(dc, width, height);
}

void MeterToolBar::EnableDisableButtons()
{
}

void MeterToolBar::OnKeyEvent(wxKeyEvent & event)
{
   event.Skip();
}

