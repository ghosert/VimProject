/**********************************************************************

  Audacity: A Digital Audio Editor

  FreqWindow.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_FREQ_WINDOW__
#define __AUDACITY_FREQ_WINDOW__

#include <wx/brush.h>
#include <wx/frame.h>
#include <wx/gdicmn.h>
#include <wx/pen.h>

class wxButton;
class wxChoice;

class FreqWindow;

class TrackList;

extern FreqWindow *gFreqWindow;

void InitFreqWindow(wxWindow * parent);

class FreqWindow;

class FreqPlot:public wxWindow {
 public:
   FreqPlot(wxWindow * parent, wxWindowID id,
            const wxPoint & pos, const wxSize & size);

   void OnMouseEvent(wxMouseEvent & event);
   void OnPaint(wxPaintEvent & event);

 private:

    FreqWindow * freqWindow;

    DECLARE_EVENT_TABLE()
};

class FreqWindow:public wxFrame {
 public:
   FreqWindow(wxWindow * parent, wxWindowID id,
              const wxString & title, const wxPoint & pos);

   virtual ~ FreqWindow();

   void Plot(int len, float *data, double rate);

   void PlotMouseEvent(wxMouseEvent & event);
   void PlotPaint(wxPaintEvent & event);

   void OnPaint(wxPaintEvent & event);

   void OnCloseWindow(wxCloseEvent & event);
   void OnCloseButton(wxCommandEvent & event);
   void OnSize(wxSizeEvent & event);
   void OnAlgChoice(wxCommandEvent & event);
   void OnSizeChoice(wxCommandEvent & event);
   void OnFuncChoice(wxCommandEvent & event);
   void OnAxisChoice(wxCommandEvent & event);
   void OnExport(wxCommandEvent & event);

   void Recalc();

 private:

    FreqPlot * mFreqPlot;

   wxBrush mBackgroundBrush;
   wxPen mBackgroundPen;

   wxCursor *mArrowCursor;
   wxCursor *mCrossCursor;

   wxButton *mCloseButton;
   wxButton *mExportButton;
   wxChoice *mAlgChoice;
   wxChoice *mSizeChoice;
   wxChoice *mFuncChoice;
   wxChoice *mAxisChoice;

   wxRect mPlotRect;
   wxRect mInfoRect;
   wxRect mUpdateRect;

   int mLeftMargin;
   int mBottomMargin;

   double mRate;
   int mDataLen;
   float *mData;
   int mWindowSize;
   float *mProcessed;
   int mProcessedSize;

   bool mLogAxis;
   float mYMin;
   float mYMax;
   float mYStep;

   wxBitmap *mBitmap;

   int mMouseX;
   int mMouseY;

   float GetProcessedValue(float freq0, float freq1);

   DECLARE_EVENT_TABLE()
};

#endif
