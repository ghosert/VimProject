/**********************************************************************

  Audacity: A Digital Audio Editor

  AStatus.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_STATUS__
#define __AUDACITY_STATUS__

#include <wx/window.h>

class wxBitmap;
class wxMenu;

int GetStatusHeight();

class AStatusListener {
 public:
   virtual void AS_SetRate(double rate) = 0;
};

class AStatus:public wxWindow {
 public:

   AStatus(wxWindow * parent, wxWindowID id,
           const wxPoint & pos,
           const wxSize & size, double rate, AStatusListener * listener);

   virtual ~ AStatus();

   void DrawDragHandle( wxDC * pDC, int x, int y );
   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnMouseEvent(wxMouseEvent & event);

   void SetField(const char *msg, int fieldNum);

   void SetRate(double rate);

   void OnRate8(wxCommandEvent & event);
   void OnRate11(wxCommandEvent & event);
   void OnRate16(wxCommandEvent & event);
   void OnRate22(wxCommandEvent & event);
   void OnRate44(wxCommandEvent & event);
   void OnRate48(wxCommandEvent & event);
   void OnRate96(wxCommandEvent & event);
   void OnRateOther(wxCommandEvent & event);

   // msmeyer: Call this to enable/disable menu items
   // in the "rate" menu, f.e. if sound card selection
   // has changed.
   void UpdateRates();

 private:

   AStatusListener * mListener;
   wxBitmap *mBitmap;
   int mWidth;
   int mHeight;
   wxRect mRateField;
   wxMenu *mRateMenu;
   double mRate;
   wxString mField[10];

 public:

   DECLARE_EVENT_TABLE()
};

#endif
