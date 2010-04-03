/**********************************************************************

  Audacity: A Digital Audio Editor

  AButton.cpp

  Dominic Mazzoni

  This is a custom button class for Audacity.  The main feature it
  supports that a wxButton does not is mouseovers.  It uses an image
  for all of its states: up, over, down, and disabled, allowing any
  sort of customization you want.  Currently it does not support
  transparency effects, so the images must be rectangular and
  opaque.

**********************************************************************/

#include "AButton.h"

#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/image.h>

//This is needed for tooltips
#include "../Project.h"
#include <wx/tooltip.h>

BEGIN_EVENT_TABLE(AButton, wxWindow)
   EVT_MOUSE_EVENTS(AButton::OnMouseEvent)
   EVT_PAINT(AButton::OnPaint)
END_EVENT_TABLE()

AButton::AButton(wxWindow * parent, wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size,
                 char **upXPM,
                 char **overXPM,
                 char **downXPM,
                 char **disXPM,
                 bool processdownevents):
   wxWindow(parent, id, pos, size)
{
   mWasShiftDown = false;
   mButtonIsDown = false;
   mButtonState = AButtonUp;
   mIsClicking = false;
   mEnabled = true;
   mProcessDownEvents=processdownevents;


   mBitmap[0] = new wxBitmap((const char **) upXPM);
   mBitmap[1] = new wxBitmap((const char **) overXPM);
   mBitmap[2] = new wxBitmap((const char **) downXPM);
   mBitmap[3] = new wxBitmap((const char **) disXPM);

   mAltBitmap[0] = NULL;
   mAltBitmap[1] = NULL;
   mAltBitmap[2] = NULL;
   mAltBitmap[3] = NULL;

   mAlternate = false;

   GetSize(&mWidth, &mHeight);
}

AButton::AButton(wxWindow * parent, wxWindowID id,
                 const wxPoint & pos,
                 const wxSize & size,
                 wxImage *up,
                 wxImage *over,
                 wxImage *down,
                 wxImage *dis,
                 bool processdownevents):
   wxWindow(parent, id, pos, size)
{
   mWasShiftDown = false;
   mButtonIsDown = false;
   mButtonState = AButtonUp;
   mIsClicking = false;
   mEnabled = true;
   mProcessDownEvents = processdownevents;

   mBitmap[0] = new wxBitmap(up);
   mBitmap[1] = new wxBitmap(over);
   mBitmap[2] = new wxBitmap(down);
   mBitmap[3] = new wxBitmap(dis);

   mAltBitmap[0] = NULL;
   mAltBitmap[1] = NULL;
   mAltBitmap[2] = NULL;
   mAltBitmap[3] = NULL;

   mAlternate = false;

   GetSize(&mWidth, &mHeight);
}

AButton::~AButton()
{
   delete mBitmap[0];
   delete mBitmap[1];
   delete mBitmap[2];
   delete mBitmap[3];

   if (mAltBitmap[0]) {
      delete mAltBitmap[0];
      delete mAltBitmap[1];
      delete mAltBitmap[2];
      delete mAltBitmap[3];
   }
}

void AButton::SetAlternateImages(wxImage *up,
                                 wxImage *over,
                                 wxImage *down,
                                 wxImage *dis)
{
   mAltBitmap[0] = new wxBitmap(up);
   mAltBitmap[1] = new wxBitmap(over);
   mAltBitmap[2] = new wxBitmap(down);
   mAltBitmap[3] = new wxBitmap(dis);
}

void AButton::SetAlternate(bool useAlternateImages)
{
   mAlternate = useAlternateImages;
   Refresh(false);
}

void AButton::OnPaint(wxPaintEvent & event)
{
   wxPaintDC dc(this);
#ifdef __WXMAC__
   if (mAlternate)
      dc.DrawBitmap(*mAltBitmap[mButtonState], 0, 0);
   else
      dc.DrawBitmap(*mBitmap[mButtonState], 0, 0);
#else
   wxMemoryDC memDC;
   if (mAlternate)
      memDC.SelectObject(*mAltBitmap[mButtonState]);
   else {
      fflush(stdout);
      memDC.SelectObject(*mBitmap[mButtonState]);
   }
   dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
#endif
}


void AButton::OnMouseEvent(wxMouseEvent & event)
{
   if (mAltBitmap[0] && mButtonState != AButtonDown) {
      if (mAlternate != event.ShiftDown()) {
         mAlternate = event.ShiftDown();
         Refresh(false);
      }
   }

   if (event.Leaving()){
      GetActiveProject()->TP_DisplayStatusMessage("",0);
   }
   

// In windows, Leave/Enter events appear to clobber each other,
// so the new enter event doesn't get processed.  If we change to a newer
// version of WXWINDOWS, (Post version 2), this may be fixed

#if defined __WXMSW__
   else {
#else
   else if (event.Entering()) {
#endif

      #if wxUSE_TOOLTIPS // Not available in wxX11
      // Display the tooltip in the status bar
      wxToolTip * pTip = this->GetToolTip();
      if( pTip ) {
         wxString tipText = pTip->GetTip();
         if (!mEnabled)
            tipText += _(" (disabled)");
         GetActiveProject()->TP_DisplayStatusMessage(tipText, 0);
      }
      #endif
   }

   //If the graphical button is disabled, or the button is down
   // the button can't process down events, get out of here.
   if (!mEnabled || (mButtonIsDown && !mProcessDownEvents) ) {
      this->Refresh(false);
      return;
   }

   //If the mouse button is released, the following stuff happens
   if (event.ButtonUp() ) {
      mIsClicking = false;
      if (HasCapture())
         ReleaseMouse();


      //Only process the event if you are releasing on the button--if you moved
      //off the button, dump the event.
      if (event.m_x >= 0 && event.m_y >= 0 &&
          event.m_x < mWidth && event.m_y < mHeight) {
      
         // Although this may be a little redundant, I'm segregating
         // two types of buttons for clarity
         // mProcessDownEvents=false buttons can only by pushed down-their pushing up must
         // be handled immediately, by the action of another button, or by
         // some other event (e.g., the file ending stops playing and pops up
         // the play button.
         // mProcessDownEvents=true buttons can be pushed down and then pushed up again
         // by clicking on them again once in the down state.
         if(mProcessDownEvents)
            {
               if(mButtonIsDown)
                  {
                     
                     //If the button is down, set the button state to up 
                     // and 'over'--highlighted.
                     mButtonState = AButtonOver;
                     mButtonIsDown = false;          
                  }
               else
                  {
                     //If the button is up, set the button state to down
                     mButtonState=AButtonDown;
                     mButtonIsDown=true;
                  }
            }
         else
            {
               //If it is a one-state button,
               //Set the button state to down undconditionally
               mButtonState = AButtonDown;
               mButtonIsDown = true;
            }

         mWasShiftDown = event.ShiftDown();

         //Create an event for the parent window to process.
         wxCommandEvent *e =
             new wxCommandEvent(wxEVT_COMMAND_BUTTON_CLICKED, GetId());
         GetParent()->AddPendingEvent(*e);
         delete e;
      }

      this->Refresh(false);
      return;
   }
      //This handles the mouse down event.
   else if (event.ButtonDown()) {
      mIsClicking = true;
      CaptureMouse();
   }


   //This following logic handles button in situations other than the 
   //up-click
   if (mProcessDownEvents)
      {
         if (mIsClicking) {
            if (event.m_x >= 0 && event.m_y >= 0 &&
                event.m_x < mWidth && event.m_y < mHeight) {
               mButtonState =  AButtonDown;
            } else
               mButtonState = mButtonIsDown ? AButtonDown: AButtonUp;
         }
         else {
            if (event.Entering())
               mButtonState = mButtonIsDown ? AButtonDown: AButtonOver;
            
            //If mouse leaves the button, put it in its previous state.
            else if (event.Leaving())
               mButtonState = mButtonIsDown ? AButtonDown: AButtonUp;
            
         }

      }
   else  //This is a push-down-only traditional button
      {
         if (mIsClicking) {
            if (event.m_x >= 0 && event.m_y >= 0 &&
                event.m_x < mWidth && event.m_y < mHeight) {
               mButtonState = AButtonDown;
            } else
               mButtonState = AButtonUp;
         }
         else {
            if (event.Entering())
               mButtonState = AButtonOver;
            else if (event.Leaving())
               mButtonState = AButtonUp;
         }
         
         
      }

   // If the button is disabled, make sure it doesn't accidentally get
   // set to the "up" state by the above logic:
   if (mButtonState == AButtonUp && !mEnabled)
      mButtonState = AButtonDis;

   //Do a final Refresh() event
   this->Refresh(false);
}

bool AButton::WasShiftDown()
{
   return mWasShiftDown;
}

void AButton::Enable()
{
   mEnabled = true;
   if (mButtonIsDown)
      mButtonState = AButtonDown;
   else
      mButtonState = AButtonUp;
   this->Refresh(false);
}

void AButton::Disable()
{
   mEnabled = false;
   mButtonState = AButtonDis;
   if (GetCapture()==this)
      ReleaseMouse();
   this->Refresh(false);
}

void AButton::PushDown()
{
   mButtonIsDown = true;
   mButtonState = AButtonDown;
   this->Refresh(false);
}

void AButton::PopUp()
{

   mButtonIsDown = false;
   if (mEnabled)
      mButtonState = AButtonUp;
   else
      mButtonState = AButtonDis;

   if (GetCapture()==this)
      ReleaseMouse();

   this->Refresh(false);
}

