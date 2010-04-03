/**********************************************************************

  Audacity: A Digital Audio Editor

  AButton.h

  Dominic Mazzoni

  This is a custom button class for Audacity.  The main feature it
  supports that a wxButton does not is mouseovers.  It uses an image
  for all of its states: up, over, down, and disabled, allowing any
  sort of customization you want.  Currently it does not support
  transparency effects, so the image musts be rectangular and
  opaque.

**********************************************************************/

#ifndef __AUDACITY_BUTTON__
#define __AUDACITY_BUTTON__

#include <wx/window.h>

class wxBitmap;
class wxImage;

class AButton:public wxWindow {
 public:

   AButton(wxWindow * parent, wxWindowID id,
           const wxPoint & pos,
           const wxSize & size,
           wxImage *up,
           wxImage *over,
           wxImage *down,
           wxImage *dis,
           bool processdownevents);

   AButton(wxWindow * parent, wxWindowID id,
           const wxPoint & pos,
           const wxSize & size,
           char **upXPM, char **overXPM, char **downXPM, char **disXPM,
           bool processdownevents           
           );

   virtual ~ AButton();

   virtual void SetAlternateImages(wxImage *up,
                                   wxImage *over,
                                   wxImage *down,
                                   wxImage *dis);

   virtual void SetAlternate(bool useAlternateImages);

   virtual void Disable();
   virtual void Enable();
   void SetEnabled(bool state) {
      state ? Enable() : Disable();
   }

   virtual void PushDown();
   virtual void PopUp();

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnMouseEvent(wxMouseEvent & event);

   virtual bool WasShiftDown(); // returns true if shift was held down
                                // the last time the button was clicked
   bool IsDown(){ return mButtonIsDown;}
   void SetButtonToggles( bool toggler ){ mProcessDownEvents = toggler;}
   void Toggle(){ mButtonIsDown ? PopUp() : PushDown();}

 private:

   enum AButtonState {
      AButtonUp,
      AButtonOver,
      AButtonDown,
      AButtonDis
   };

   int mWidth;
   int mHeight;

   bool mAlternate;

   bool mWasShiftDown;

   bool mButtonIsDown;
   bool mIsClicking;
   bool mEnabled;
   bool mProcessDownEvents;    // This bool, if true, makes the button able to process 
                               // events when it is in the down state, and moving to
                               // the opposite state when it is clicked. It is used for the Pause
                               // button, and possibly others. If false, it (should)
                               // behave just like a standard button.
   

   AButtonState mButtonState;

   wxBitmap *mBitmap[4];
   wxBitmap *mAltBitmap[4];

 public:

    DECLARE_EVENT_TABLE()
};

#endif
