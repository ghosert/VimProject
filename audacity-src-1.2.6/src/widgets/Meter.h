/**********************************************************************

  Audacity: A Digital Audio Editor

  Meter.h

  Dominic Mazzoni

  VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

**********************************************************************/

#ifndef __AUDACITY_METER__
#define __AUDACITY_METER__

#include <wx/defs.h>
#include <wx/panel.h>
#include <wx/timer.h>

#include "../SampleFormat.h"
#include "Ruler.h"

// Increase this when we add support for multichannel meters
// (most of the code is already there)
const int kMaxMeterBars = 2;

struct MeterBar {
   bool   vert;
   wxRect r;
   float  peak;
   float  rms;
   float  peakHold;
   double peakHoldTime;
   wxRect rClip;
   bool   clipping;
   int    tailPeakCount;
};

struct MeterUpdateMsg
{
   int numFrames;
   float peak[kMaxMeterBars];
   float rms[kMaxMeterBars];
   bool clipping[kMaxMeterBars];
   int headPeakCount[kMaxMeterBars];
   int tailPeakCount[kMaxMeterBars];
};

// Thread-safe queue of update messages
class MeterUpdateQueue
{
 public:
   MeterUpdateQueue(int maxLen);
   ~MeterUpdateQueue();

   bool Put(MeterUpdateMsg &msg);
   bool Get(MeterUpdateMsg &msg);

   void Clear();

 private:
   int              mStart;
   int              mEnd;
   int              mBufferSize;
   MeterUpdateMsg  *mBuffer;
};

class Meter : public wxPanel
{
   DECLARE_DYNAMIC_CLASS(Meter)

 public:
   Meter(wxWindow* parent, wxWindowID id,
         bool isInput,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize);

   ~Meter();

   // These should be kept in the same order as they appear
   // in the menu
   enum Style {
      HorizontalStereo = 0,
      VerticalStereo,
      VerticalMulti,
      Equalizer,
      Waveform
   };

   Style GetStyle() { return mStyle; }
   void SetStyle(Style newStyle);

   //
   // These methods are thread-safe!  Feel free to call from a
   // different thread (like from an audio I/O callback)
   void Reset(double sampleRate, bool resetClipping);
   void UpdateDisplay(int numChannels,
                      int numFrames, float *sampleData);
   // End thread-safe methods
   //

   //
   // Event handlers
   //

   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnMouse(wxMouseEvent &evt);

   void OnMeterUpdate(wxTimerEvent &evt);

   void HandlePaint(wxDC &dc);
   void HandleLayout();

   //
   // Pop-up menu handlers
   //

   void OnHorizontal(wxCommandEvent &evt);
   void OnVertical(wxCommandEvent &evt);
   void OnMulti(wxCommandEvent &evt);
   void OnEqualizer(wxCommandEvent &evt);
   void OnWaveform(wxCommandEvent &evt);
   void OnLinear(wxCommandEvent &evt);
   void OnDB(wxCommandEvent &evt);
   void OnClip(wxCommandEvent &evt);
   void OnMonitor(wxCommandEvent &evt);
   void OnFloat(wxCommandEvent &evt);
   void OnDisable(wxCommandEvent &evt);
   
 private:
   void DrawMeterBar(wxDC &dc, MeterBar *meterBar);
   void ResetBar(MeterBar *bar, bool resetClipping);
   void RepaintBarsNow();
   void StartMonitoring();
   void CreateIcon(int aquaOffset);
   wxFont GetFont();

   MeterUpdateQueue mQueue;
   wxTimer          mTimer;

   int       mWidth;
   int       mHeight;

   bool      mIsInput;

   Style     mStyle;
   bool      mDB;
   int       mDBRange;
   bool      mDecay;
   float     mDecayRate; // dB/sec
   bool      mClip;
   int       mNumPeakSamplesToClip;
   double    mPeakHoldDuration;
   double    mT;
   double    mRate;

   int       mNumBars;
   MeterBar  mBar[kMaxMeterBars];

   bool      mLayoutValid;

   wxBitmap *mBitmap;
   wxBitmap *mBackgroundBitmap;
   wxRect    mMenuRect;
   wxPoint   mIconPos;
   wxPoint   mLeftTextPos;
   wxPoint   mRightTextPos;
   wxString  mLeftText;
   wxString  mRightText;
   wxSize    mLeftSize;
   wxSize    mRightSize;
   wxBitmap *mIcon;
   wxPen     mPen;
   wxPen     mLightPen;
   wxPen     mDarkPen;
   wxBrush   mBrush;
   wxBrush   mRMSBrush;
   wxBrush   mClipBrush;
   wxBrush   mBkgndBrush;
   wxRect    mAllBarsRect;
   Ruler     mRuler;

   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_METER__
