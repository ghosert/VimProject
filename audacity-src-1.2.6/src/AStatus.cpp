/**********************************************************************

  Audacity: A Digital Audio Editor

  AStatus.cpp

  Copyright 2004 Dominic Mazzoni, Matt Brubeck, James Crook
  
  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

**********************************************************************/

#include "AStatus.h"

#include "AudioIO.h"
#include "AColor.h"

#include <wx/dc.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/frame.h>
#include <wx/gdicmn.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>

int GetStatusHeight()
{
   return 55;
}

enum {
   AStatusFirstID = 2700,

   OnRate8ID,
   OnRate11ID,
   OnRate16ID,
   OnRate22ID,
   OnRate44ID,
   OnRate48ID,
   OnRate96ID,
   OnRateOtherID
};

BEGIN_EVENT_TABLE(AStatus, wxWindow)
    EVT_MOUSE_EVENTS(AStatus::OnMouseEvent)
    EVT_PAINT(AStatus::OnPaint)

    EVT_MENU(OnRate8ID, AStatus::OnRate8)
    EVT_MENU(OnRate11ID, AStatus::OnRate11)
    EVT_MENU(OnRate16ID, AStatus::OnRate16)
    EVT_MENU(OnRate22ID, AStatus::OnRate22)
    EVT_MENU(OnRate44ID, AStatus::OnRate44)
    EVT_MENU(OnRate48ID, AStatus::OnRate48)
    EVT_MENU(OnRate96ID, AStatus::OnRate96)
    EVT_MENU(OnRateOtherID, AStatus::OnRateOther)
    END_EVENT_TABLE()

AStatus::AStatus(wxWindow * parent, wxWindowID id,
                     const wxPoint & pos,
                     const wxSize & size,
                     double rate,
                     AStatusListener * listener):
   wxWindow(parent, id, pos,  size),
   mListener(listener), mBitmap(NULL), mRate(rate)
{
   GetSize(&mWidth, &mHeight);

   mRateMenu = new wxMenu();
   mRateMenu->AppendCheckItem(OnRate8ID, "8000 Hz");
   mRateMenu->AppendCheckItem(OnRate11ID, "11025 Hz");
   mRateMenu->AppendCheckItem(OnRate16ID, "16000 Hz");
   mRateMenu->AppendCheckItem(OnRate22ID, "22050 Hz");
   mRateMenu->AppendCheckItem(OnRate44ID, "44100 Hz");
   mRateMenu->AppendCheckItem(OnRate48ID, "48000 Hz");
   mRateMenu->AppendCheckItem(OnRate96ID, "96000 Hz");
   mRateMenu->AppendCheckItem(OnRateOtherID, _("Other..."));

   UpdateRates();

   mRateField.x = 0;
   mRateField.y = 0;
   mRateField.width = 0;
   mRateField.height = 0;
}

AStatus::~AStatus()
{
   if (mBitmap)
      delete mBitmap;

   delete mRateMenu;
}

void AStatus::SetField(const char *msg, int fieldNum)
{
   if (fieldNum < 0 || fieldNum >= 10)
      return;

   if (mField[fieldNum] != msg) {
      mField[fieldNum] = msg;
      Refresh(false);
   }
}

void AStatus::SetRate(double rate)
{
   if (rate != mRate) {
      mRate = rate;
      Refresh(false);
   }
}

/// Draw resizing drag handle in bottom left corner of status
/// bar.  This is to give a more MS Windows look on that platform.
/// An alternative would be to derive AStatus from wxStatusBar,
/// rather than from wxWindow and set style wxST_SIZEGRIP.
void AStatus::DrawDragHandle( wxDC * pDC, int x, int y )
{
   int i, d;
   const int nStripes = 3;
   const int r=5;

   AColor::Medium( pDC, false );
   // Background is medium dark.
   pDC->DrawRectangle( x-12-r, y-11, 13+r,12);
   AColor::Dark(pDC, false);
   // Dark diagonal line is needed here because our beveled
   // edge is higher up than on a MS Windows status bar.
   // This line makes it OK to have the drag handle lower 
   // down.
   pDC->DrawLine( x-13,y, x-13-r, y-r);

   // Draw the dark diagonal lines of the drag handle.
   for(i=0;i<nStripes;i++){
      d = i*4+2; pDC->DrawLine( x-d, y, x, y-d);
      d = i*4+3; pDC->DrawLine( x-d, y, x, y-d);
   }
  
   // Drwaw the light diagonal lines of the drag handle.
   AColor::Light(pDC, false);
   for(i=0;i<nStripes;i++){
      d = i*4+4;
      pDC->DrawLine( x-d, y, x, y-d);
   }
}

void AStatus::OnPaint(wxPaintEvent & event)
{
   wxPaintDC dc(this);

   // msmeyer: Update check status of rates
   mRateMenu->Check(OnRate8ID, mRate == 8000);
   mRateMenu->Check(OnRate11ID, mRate == 11025);
   mRateMenu->Check(OnRate16ID, mRate == 16000);
   mRateMenu->Check(OnRate22ID, mRate == 22050);
   mRateMenu->Check(OnRate44ID, mRate == 44100);
   mRateMenu->Check(OnRate48ID, mRate == 48000);
   mRateMenu->Check(OnRate96ID, mRate == 96000);
   mRateMenu->Check(OnRateOtherID,
      mRate != 8000 && mRate != 11025 && mRate != 16000 &&
      mRate != 22050 && mRate != 44100 && mRate != 48000 &&
      mRate != 96000);

   int width, height;
   GetSize(&width, &height);

   if (width != mWidth || height != mHeight) {
      mWidth = width;
      mHeight = height;

      delete mBitmap;
      mBitmap = NULL;
   }

   if (!mBitmap)
      mBitmap = new wxBitmap(mWidth, mHeight);

   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);

   AColor::Medium(&memDC, false);
   memDC.DrawRectangle(0, 0, mWidth, mHeight);

   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawLine(0, 0, mWidth, 0);

   wxRect outline;
   outline.x = 0;
   outline.y = 1;
   outline.width = mWidth - 1;
   outline.height = mHeight - 2;
   AColor::Bevel(memDC, true, outline);

   // Display status message.
   AColor::SetLabelFont(memDC);

   wxRect msgField;
   msgField.x = 4;
   msgField.y = 6;
   msgField.width = mWidth - 8;
   msgField.height = 17;

   AColor::Bevel(memDC, false, msgField);

   wxString msg = mField[0];
   wxCoord textWidth, textHeight;
   memDC.GetTextExtent(msg, &textWidth, &textHeight);
   while (msg != "" && textWidth > msgField.width) {
      msg = msg.Left(msg.Length() - 1);
      memDC.GetTextExtent(msg, &textWidth, &textHeight);
   }
   memDC.DrawText(msg, msgField.x + 3, msgField.y + 2);

   // Display "Project rate" menu.
   wxString label = _("Project rate:");
   memDC.GetTextExtent(label, &textWidth, &textHeight);

   mRateField.x = textWidth + 10;
   mRateField.y = 29;
   mRateField.width = 50;
   mRateField.height = 17;

   memDC.DrawText(label, 3, mRateField.y + 2);

   AColor::Bevel(memDC, true, mRateField);
   memDC.DrawText(wxString::Format("%d", int (mRate + 0.5)),
                  mRateField.x + 3, mRateField.y + 2);

   // Display cursor and selection position.
   wxRect cursorField;
   cursorField.x = mRateField.x + mRateField.width + 10;
   cursorField.y = mRateField.y;
   cursorField.width = mWidth - cursorField.x - 4;
   cursorField.height = mRateField.height;

#ifdef __WXMAC__
   cursorField.width -= 15;
#endif

   AColor::Bevel(memDC, false, cursorField);
#if defined __WXMSW__
   //Under MS Windows draw the drag handle if not maximized.
   if( GetParent() != NULL)
   {
      //TODO: Using the cast here assumes that the immediate parent is
      //a frame.  This is just fine for Audacity, but to be strictly
      //correct we should test this before casting, possibly 
      //using GetStyle() [JKC].
      if( !((wxFrame*)GetParent())->IsMaximized() )
         DrawDragHandle( &memDC, mWidth-4, mHeight-4 );
   }
#endif

   msg = mField[1];
   memDC.GetTextExtent(msg, &textWidth, &textHeight);
   while (msg != "" && textWidth > cursorField.width) {
      msg = msg.Left(msg.Length() - 1);
      memDC.GetTextExtent(msg, &textWidth, &textHeight);
   }
   memDC.DrawText(msg, cursorField.x + 3, cursorField.y + 2);

   // Finish drawing.
   dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
   memDC.SelectObject(wxNullBitmap);
}

void AStatus::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown() && mRateField.Inside(event.m_x, event.m_y)) {

      {
         wxClientDC dc(this);
         AColor::Bevel(dc, false, mRateField);
      }

      PopupMenu(mRateMenu, mRateField.x, mRateField.y + mRateField.height);

      {
         wxClientDC dc(this);
         AColor::Bevel(dc, true, mRateField);
      }
   }
}

void AStatus::OnRate8(wxCommandEvent & WXUNUSED(event))
{
   mRate = 8000.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate11(wxCommandEvent & WXUNUSED(event))
{
   mRate = 11025.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate16(wxCommandEvent & WXUNUSED(event))
{
   mRate = 16000.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate22(wxCommandEvent & WXUNUSED(event))
{
   mRate = 22050.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate44(wxCommandEvent & WXUNUSED(event))
{
   mRate = 44100.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate48(wxCommandEvent & WXUNUSED(event))
{
   mRate = 48000.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRate96(wxCommandEvent & WXUNUSED(event))
{
   mRate = 96000.0;
   mListener->AS_SetRate(mRate);
   Refresh(false);
}

void AStatus::OnRateOther(wxCommandEvent & WXUNUSED(event))
{
   wxString defaultStr;
   defaultStr.Printf("%d", (int) (mRate + 0.5));
   wxString rateStr =
       wxGetTextFromUser(_("Enter a rate in Hz (samples per second):"),
                         _("Set Rate"),
                         defaultStr);

   if (rateStr != "") {
      double theRate;
      if (rateStr.ToDouble(&theRate) && theRate >= 1 && theRate <= 100000) {
         mRate = theRate;
         mListener->AS_SetRate(mRate);
         Refresh(false);
      } else
         wxMessageBox(_("Invalid rate."));
   }
}

void AStatus::UpdateRates()
{
   wxArrayLong rates = AudioIO::GetSupportedSampleRates();

   mRateMenu->Enable(OnRate8ID, rates.Index(8000) != wxNOT_FOUND);
   mRateMenu->Enable(OnRate11ID, rates.Index(11025) != wxNOT_FOUND);
   mRateMenu->Enable(OnRate16ID, rates.Index(16000) != wxNOT_FOUND);
   mRateMenu->Enable(OnRate22ID, rates.Index(22050) != wxNOT_FOUND);
   mRateMenu->Enable(OnRate44ID, rates.Index(44100) != wxNOT_FOUND);
   mRateMenu->Enable(OnRate48ID, rates.Index(48000) != wxNOT_FOUND);
   mRateMenu->Enable(OnRate96ID, rates.Index(96000) != wxNOT_FOUND);
}

