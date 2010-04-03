/**********************************************************************

  Audacity: A Digital Audio Editor

  Meter.cpp

  Dominic Mazzoni

  VU Meter, for displaying recording/playback level

  This is a bunch of common code that can display many different
  forms of VU meters and other displays.

**********************************************************************/

#include "../Audacity.h"
#include "../Prefs.h" // for gPrefs

#include <wx/defs.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/menu.h>
#include <wx/settings.h>
#include <wx/tooltip.h>

#include <math.h>

#include "Meter.h"

#include "../AudioIO.h"
#include "../AColor.h"
#include "../ImageManipulation.h"
#include "../../images/MixerImages.h"
#include "../Project.h"
#include "../MeterToolBar.h"
#include "Warning.h"

//
// The Meter passes itself messages via this queue so that it can
// communicate between the audio thread and the GUI thread.
// This class is as simple as possible in order to be thread-safe
// without needing mutexes.
//

MeterUpdateQueue::MeterUpdateQueue(int maxLen):
   mBufferSize(maxLen)
{
   mBuffer = new MeterUpdateMsg[mBufferSize];
   Clear();
}

// destructor
MeterUpdateQueue::~MeterUpdateQueue()
{
   delete[] mBuffer;
}

void MeterUpdateQueue::Clear()
{
   mStart = 0;
   mEnd = 0;
}

// Add a message to the end of the queue.  Return false if the
// queue was full.
bool MeterUpdateQueue::Put(MeterUpdateMsg &msg)
{
   int len = (mEnd + mBufferSize - mStart) % mBufferSize;

   // Never completely fill the queue, because then the
   // state is ambiguous (mStart==mEnd)
   if (len >= mBufferSize-1)
      return false;

   mBuffer[mEnd] = msg;
   mEnd = (mEnd+1)%mBufferSize;

   return true;
}

// Get the next message from the start of the queue.
// Return false if the queue was empty.
bool MeterUpdateQueue::Get(MeterUpdateMsg &msg)
{
   int len = (mEnd + mBufferSize - mStart) % mBufferSize;

   if (len == 0)
      return false;

   msg = mBuffer[mStart];
   mStart = (mStart+1)%mBufferSize;

   return true;
}

//
// Meter class
//

enum {
   OnMeterUpdateID = 6000,
   OnHorizontalID,
   OnVerticalID,
   OnMultiID,
   OnEqualizerID,
   OnWaveformID,
   OnLinearID,
   OnDBID,
   OnClipID,
   OnMonitorID,
   OnFloatID,
   OnDisableID
};

BEGIN_EVENT_TABLE(Meter, wxPanel)
   EVT_TIMER(OnMeterUpdateID, Meter::OnMeterUpdate)
   EVT_MOUSE_EVENTS(Meter::OnMouse)
   EVT_PAINT(Meter::OnPaint)
   EVT_SIZE(Meter::OnSize)
   EVT_MENU(OnHorizontalID, Meter::OnHorizontal)
   EVT_MENU(OnVerticalID, Meter::OnVertical)
   EVT_MENU(OnMultiID, Meter::OnMulti)
   EVT_MENU(OnEqualizerID, Meter::OnEqualizer)
   EVT_MENU(OnWaveformID, Meter::OnWaveform)
   EVT_MENU(OnLinearID, Meter::OnLinear)
   EVT_MENU(OnDBID, Meter::OnDB)
   EVT_MENU(OnClipID, Meter::OnClip)
   EVT_MENU(OnMonitorID, Meter::OnMonitor)
   EVT_MENU(OnFloatID, Meter::OnFloat)
   EVT_MENU(OnDisableID, Meter::OnDisable)
END_EVENT_TABLE()

IMPLEMENT_CLASS(Meter, wxPanel)

Meter::Meter(wxWindow* parent, wxWindowID id,
             bool isInput,
             const wxPoint& pos /*= wxDefaultPosition*/,
             const wxSize& size /*= wxDefaultSize*/):
   wxPanel(parent, id, pos, size),
   mQueue(256),
   mWidth(size.x), mHeight(size.y),
   mIsInput(isInput),
   mStyle(HorizontalStereo),
   mDB(true),
   mDBRange(60),
   mDecay(true),
   mDecayRate(60),
   mClip(true),
   mNumPeakSamplesToClip(3),
   mPeakHoldDuration(3),
   mT(0),
   mRate(0),
   mNumBars(0),
   mLayoutValid(false),
   mBitmap(NULL),
   mBackgroundBitmap(NULL),
   mIcon(NULL)
{
   int i;
   
   wxColour origColour(204, 204, 204);

   /* i18n-hint: One-letter abbreviation for Left, in VU Meter */
   mLeftText = _("L");
   /* i18n-hint: One-letter abbreviation for Right, in VU Meter */
   mRightText = _("R");

   mLeftSize = wxSize(0, 0);
   mRightSize = wxSize(0, 0);

   if (mIsInput) {
      mPen = wxPen(wxColour(204, 70, 70), 1, wxSOLID);
      mBrush = wxBrush(wxColour(204, 70, 70), wxSOLID);
      mRMSBrush = wxBrush(wxColour(255, 102, 102), wxSOLID);
      mClipBrush = wxBrush(wxColour(255, 53, 53), wxSOLID);
      mLightPen = wxPen(wxColour(255, 153, 153), 1, wxSOLID);
      mDarkPen = wxPen(wxColour(153, 61, 61), 1, wxSOLID);
   }
   else {
      mPen = wxPen(wxColour(70, 204, 70), 1, wxSOLID);
      mBrush = wxBrush(wxColour(70, 204, 70), wxSOLID);
      mRMSBrush = wxBrush(wxColour(102, 255, 102), wxSOLID);
      mClipBrush = wxBrush(wxColour(255, 53, 53), wxSOLID);
      mLightPen = wxPen(wxColour(153, 255, 153), 1, wxSOLID);
      mDarkPen = wxPen(wxColour(61, 164, 61), 1, wxSOLID);
   }

   CreateIcon(2);

   // The new Ruler is much smarter at picking fonts than we are...
   //mRuler.SetFonts(GetFont(), GetFont());

   mTimer.SetOwner(this, OnMeterUpdateID);
   Reset(44100.0, true);
   for(i=0; i<kMaxMeterBars; i++)
      mBar[i].clipping = false;
}

void Meter::CreateIcon(int aquaOffset)
{
   wxColour backgroundColour =
      wxSystemSettings::GetSystemColour(wxSYS_COLOUR_3DFACE);
   mBkgndBrush = wxBrush(backgroundColour, wxSOLID);
   wxImage *image, *alpha;

   if (mIcon) {
      delete mIcon;
      mIcon = NULL;
   }

   if (mIsInput) {
      image = new wxImage(wxBitmap(Mic).ConvertToImage());
      alpha = new wxImage(wxBitmap(MicAlpha).ConvertToImage());
   }
   else {
      image = new wxImage(wxBitmap(Speaker).ConvertToImage());
      alpha = new wxImage(wxBitmap(SpeakerAlpha).ConvertToImage());
   }

   wxImage *bkgnd = CreateSysBackground(25, 25, aquaOffset,
					backgroundColour);
   wxImage *final = OverlayImage(bkgnd, image, alpha, 0, 0);
   mIcon = new wxBitmap(final);

   delete image;
   delete alpha;
   delete bkgnd;
   delete final;
}

Meter::~Meter()
{
   delete mIcon;
   if (mBitmap)
      delete mBitmap;
   if (mBackgroundBitmap)
      delete mBackgroundBitmap;
}

void Meter::OnPaint(wxPaintEvent &evt)
{
   wxPaintDC dc(this);
  #ifdef __WXMAC__
   // Mac OS X automatically double-buffers the screen for you,
   // so our bitmap is unneccessary
   HandlePaint(dc);
  #else
   if (!mBitmap)
      mBitmap = new wxBitmap(mWidth, mHeight);
   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);
   HandlePaint(memDC);
   dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
  #endif 
}

void Meter::OnSize(wxSizeEvent &evt)
{
   delete mBitmap;
   mBitmap = NULL;
   GetClientSize(&mWidth, &mHeight);
   mLayoutValid = false;
}

void Meter::OnMouse(wxMouseEvent &evt)
{
  #if wxUSE_TOOLTIPS // Not available in wxX11
   if (evt.Leaving()){
      GetActiveProject()->TP_DisplayStatusMessage("",0);
   }
   else if (evt.Entering()) {
      // Display the tooltip in the status bar
      wxToolTip * pTip = this->GetToolTip();
      if( pTip ) {
         wxString tipText = pTip->GetTip();
         GetActiveProject()->TP_DisplayStatusMessage(tipText, 0);
      }
   }
  #endif

   if (evt.RightDown() ||
       (evt.ButtonDown() && mMenuRect.Inside(evt.m_x, evt.m_y))) {
      wxMenu *menu = new wxMenu();
      // Note: these should be kept in the same order as the enum
      menu->Append(OnHorizontalID, _("Horizontal Stereo"));
      menu->Append(OnVerticalID, _("Vertical Stereo"));
      //menu->Append(OnMultiID, _("Vertical Multichannel"));
      //menu->Append(OnEqualizerID, _("Equalizer"));
      //menu->Append(OnWaveformID, _("Waveform"));
      menu->Enable(OnHorizontalID + mStyle, false);
      menu->AppendSeparator();
      menu->Append(OnLinearID, _("Linear"));
      menu->Append(OnDBID, _("dB"));
      menu->Enable(mDB? OnDBID: OnLinearID, false);
      //menu->AppendSeparator();
      //menu->Append(OnClipID, _("Turn on clipping"));
      if (mIsInput) {
         menu->AppendSeparator();
         menu->Append(OnMonitorID, _("Monitor input"));
      }
      //menu->AppendSeparator();
      //menu->Append(OnFloatID, _("Float Window"));

      menu->AppendSeparator();
      menu->Append(OnDisableID, _("Disable"));

      if (evt.RightDown())
         PopupMenu(menu, evt.m_x, evt.m_y);
      else
         PopupMenu(menu, mMenuRect.x + 1, mMenuRect.y + mMenuRect.height + 1);
      delete menu;
   }       
   else if (evt.ButtonDown()) {
      if (mIsInput)
         StartMonitoring();
   }
}

void Meter::SetStyle(Meter::Style newStyle)
{
   mStyle = newStyle;
   mLayoutValid = false;
   Refresh(true);
}

void Meter::Reset(double sampleRate, bool resetClipping)
{
   int j;

   mT = 0;
   mRate = sampleRate;
   for(j=0; j<mNumBars; j++)
      ResetBar(&mBar[j], resetClipping);

   // wxTimers seem to be a little unreliable - sometimes they stop for
   // no good reason, so this "primes" it every now and then...
   mTimer.Stop();

   // While it's stopped, empty the queue
   MeterUpdateMsg msg;
   while(mQueue.Get(msg)) {
   }

   mTimer.Start(25); // every 25 ms -> ~40 updates per second

   mLayoutValid = false;
   Refresh(false);
}

static float floatMax(float a, float b)
{
   return a>b? a: b;
}

static int intmin(int a, int b)
{
   return a<b? a: b;
}

static int intmax(int a, int b)
{
   return a>b? a: b;
}

static float ClipZeroToOne(float z)
{
   if (z > 1.0)
      return 1.0;
   else if (z < 0.0)
      return 0.0;
   else
      return z;
}

static float ToDB(float v, float range)
{
   double db;
   if (v > 0)
      db = 20 * log10(fabs(v));
   else
      db = -999;
   return ClipZeroToOne((db + range) / range);
}

void Meter::UpdateDisplay(int numChannels, int numFrames, float *sampleData)
{
   int i, j;
   float *sptr = sampleData;
   int num = intmin(numChannels, mNumBars);
   MeterUpdateMsg msg;

   msg.numFrames = numFrames;
   for(j=0; j<mNumBars; j++) {
      msg.peak[j] = 0;
      msg.rms[j] = 0;
      msg.clipping[j] = false;
      msg.headPeakCount[j] = 0;
      msg.tailPeakCount[j] = 0;
   }

   for(i=0; i<numFrames; i++) {
      for(j=0; j<num; j++) {
         msg.peak[j] = floatMax(msg.peak[j], sptr[j]);
         msg.rms[j] += sptr[j]*sptr[j];

         // In addition to looking for mNumPeakSamplesToClip peaked
         // samples in a row, also send the number of peaked samples
         // at the head and tail, in case there's a run of 
         // Send the number of peaked samples at the head and tail,
         // in case there's a run of peaked samples that crosses
         // block boundaries
         if (fabs(sptr[j])>=1.0) {
            if (msg.headPeakCount[j]==i)
               msg.headPeakCount[j]++;
            msg.tailPeakCount[j]++;
            if (msg.tailPeakCount[j] > mNumPeakSamplesToClip)
               msg.clipping[j] = true;
         }
         else
            msg.tailPeakCount[j] = 0;
      }
      sptr += numChannels;
   }
   for(j=0; j<mNumBars; j++)
      msg.rms[j] = sqrt(msg.rms[j]/numFrames);

   if (mDB) {
      for(j=0; j<mNumBars; j++) {
         msg.peak[j] = ToDB(msg.peak[j], mDBRange);
         msg.rms[j] = ToDB(msg.rms[j], mDBRange);
      }
   }

   mQueue.Put(msg);
}

void Meter::OnMeterUpdate(wxTimerEvent &evt)
{
   MeterUpdateMsg msg;
   int numChanges = 0;

   // There may have been several update messages since the last
   // time we got to this function.  Catch up to real-time by
   // popping them off until there are none left.  It is necessary
   // to process all of them, otherwise we won't handle peaks and
   // peak-hold bars correctly.
   while(mQueue.Get(msg)) {
      numChanges++;
      double deltaT = msg.numFrames / mRate;
      int j;
      
      mT += deltaT;
      for(j=0; j<mNumBars; j++) {
         if (mDecay) {
            if (mDB) {
               float decayAmount = mDecayRate * deltaT / mDBRange;
               mBar[j].peak = floatMax(msg.peak[j],
                                       mBar[j].peak - decayAmount);
            }
            else {
               double decayAmount = mDecayRate * deltaT;
               double decayFactor = pow(10.0, -decayAmount/20);
               mBar[j].peak = floatMax(msg.peak[j],
                                       mBar[j].peak * decayFactor);
            }
         }
         else
            mBar[j].peak = msg.peak[j];
         
         // This smooths out the RMS signal
         mBar[j].rms = mBar[j].rms * 0.9 + msg.rms[j] * 0.1;
         
         if (mT - mBar[j].peakHoldTime > mPeakHoldDuration ||
             mBar[j].peak > mBar[j].peakHold) {
            mBar[j].peakHold = mBar[j].peak;
            mBar[j].peakHoldTime = mT;
         }
         
         if (msg.clipping[j] ||
             mBar[j].tailPeakCount+msg.headPeakCount[j] >=
             mNumPeakSamplesToClip)
            mBar[j].clipping = true;
         mBar[j].tailPeakCount = msg.tailPeakCount[j];
      }      
   } // while

   if (numChanges > 0)      
      RepaintBarsNow();
}

wxFont Meter::GetFont()
{
   int fontSize = 10;
#if defined __WXMSW__
   fontSize = 8;
#endif

   return wxFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
}

void Meter::ResetBar(MeterBar *b, bool resetClipping)
{
   b->peak = 0.0;
   b->rms = 0.0;
   b->peakHold = 0.0;
   b->peakHoldTime = 0.0;
   if (resetClipping)
      b->clipping = false;
   b->tailPeakCount = 0;
}

void Meter::HandleLayout()
{
   int iconWidth = mIcon->GetWidth();
   int iconHeight = mIcon->GetHeight();
   int menuWidth = 17;
   int menuHeight = 11;
   int width = mWidth;
   int height = mHeight;
   int left = 0, top = 0;
   int right, bottom;
   int barw, barh;
   int i;

#ifdef __WXMAC__
   if (!mBackgroundBitmap ||
       mBackgroundBitmap->GetWidth() != mWidth ||
       mBackgroundBitmap->GetWidth() != mHeight) {
     if (mBackgroundBitmap)
       delete mBackgroundBitmap;

     wxImage *image = CreateAquaBackground(mWidth, mHeight, 0);
     mBackgroundBitmap = new wxBitmap(image);
     delete image;
   }
#endif

   mRuler.SetFlip(true);
   mRuler.SetLabelEdges(true);

   switch(mStyle) {
   default:
      printf("Style not handled yet!\n");
   case VerticalStereo:
      mMenuRect = wxRect(mWidth - menuWidth - 5, mHeight - menuHeight - 2,
                         menuWidth, menuHeight);
      if (mHeight < (menuHeight + iconHeight + 8))
         mIconPos = wxPoint(-999, -999); // Don't display
      else
         mIconPos = wxPoint(mWidth - iconWidth - 1, 1);
      width = intmin(mWidth-(iconWidth+2), mWidth-(menuWidth+3));
      if (width >= mLeftSize.x + mRightSize.x + 24) {
         mLeftTextPos = wxPoint(2, height-2-mLeftSize.y);
         mRightTextPos = wxPoint(width-mLeftSize.x, height-2-mLeftSize.y);
         left += mLeftSize.x+4;
         width -= mLeftSize.x + mRightSize.x + 8;
      }
      barw = (width-2)/2;
      barh = height - 4;
      mNumBars = 2;
      mBar[0].vert = true;
      ResetBar(&mBar[0], false);
      mBar[0].r = wxRect(left + width/2 - barw - 1, 2, barw, barh);
      if (mClip) {
         mBar[0].rClip = mBar[0].r;
         mBar[0].rClip.height = 3;
         mBar[0].r.y += 4;
         mBar[0].r.height -= 4;
      }
      mBar[1].vert = true;
      ResetBar(&mBar[1], false);
      mBar[1].r = wxRect(left + width/2 + 1, 2, barw, barh);
      if (mClip) {
         mBar[1].rClip = mBar[1].r;
         mBar[1].rClip.height = 3;
         mBar[1].r.y += 4;
         mBar[1].r.height -= 4;
      }
      mRuler.SetOrientation(wxVERTICAL);
      mRuler.SetBounds(mBar[1].r.x + mBar[1].r.width + 1,
                       mBar[1].r.y,
                       mBar[1].r.x + width,
                       mBar[1].r.height);
      if (mDB) {
         mRuler.SetRange(0, -mDBRange);
         mRuler.SetFormat(Ruler::LinearDBFormat);
      }
      else {
         mRuler.SetRange(1, 0);
         mRuler.SetFormat(Ruler::RealFormat);
      }
      mRuler.OfflimitsPixels(mMenuRect.y-mBar[1].r.y, mBar[1].r.height);
      break;
   case HorizontalStereo:
      if (mWidth < menuWidth + iconWidth + 8) {
         mIconPos = wxPoint(-999, -999); // Don't display icon
         mMenuRect = wxRect(2, mHeight - menuHeight - 2,
                            menuWidth, menuHeight);
      }         
      else {
         mIconPos = wxPoint(2, mHeight - iconHeight);
         mMenuRect = wxRect(iconWidth + 2, mHeight - menuHeight - 3,
                            menuWidth, menuHeight);
      }
      height = intmin(height-(menuHeight+3), height-iconHeight) - 2;
      left = 2 + intmax(mLeftSize.x, mRightSize.x);
      width -= left;
      mLeftTextPos = wxPoint(2, (height)/4 - mLeftSize.y/2);
      mRightTextPos = wxPoint(2, (height*3)/4 - mLeftSize.y/2);
      barw = width - 4;
      barh = (height-2)/2;
      mNumBars = 2;
      mBar[0].vert = false;
      ResetBar(&mBar[0], false);
      mBar[0].r = wxRect(left+2, height/2 - barh - 1, barw, barh);
      if (mClip) {
         mBar[0].rClip = mBar[0].r;
         mBar[0].rClip.x += mBar[0].rClip.width-3;
         mBar[0].rClip.width = 3;
         mBar[0].r.width -= 4;
      }
      mBar[1].vert = false;
      ResetBar(&mBar[1], false);
      mBar[1].r = wxRect(left+2, height/2 + 1, barw, barh);
      if (mClip) {
         mBar[1].rClip = mBar[1].r;
         mBar[1].rClip.x += mBar[1].rClip.width-3;
         mBar[1].rClip.width = 3;
         mBar[1].r.width -= 4;
      }
      mRuler.SetOrientation(wxHORIZONTAL);
      mRuler.SetBounds(mBar[1].r.x,
                       mBar[1].r.y + mBar[1].r.height + 1,
                       mBar[1].r.x + mBar[1].r.width,
                       mWidth);
      if (mDB) {
         mRuler.SetRange(-mDBRange, 0);
         mRuler.SetFormat(Ruler::LinearDBFormat);
      }
      else {
         mRuler.SetRange(0, 1);
         mRuler.SetFormat(Ruler::RealFormat);
      }
      mRuler.OfflimitsPixels(0, mMenuRect.x+mMenuRect.width-4);
      break;
   case Waveform:
      mNumBars = 0;
      break;
   }

   if (mNumBars > 0) {
      // Compute bounding rectangle of all bars (to save time when
      // blitting just the bars to the screen)
      left = mBar[0].r.x;
      top = mBar[0].r.y;
      right = mBar[0].r.x + mBar[0].r.width;
      bottom = mBar[0].r.y + mBar[0].r.height;
      for(i=1; i<mNumBars; i++) {
         left = intmin(left, mBar[i].r.x);
         top = intmin(top, mBar[i].r.y);
         right = intmax(right, mBar[i].r.x + mBar[i].r.width);
         bottom = intmax(bottom, mBar[i].r.y + mBar[i].r.height);
         left = intmin(left, mBar[i].rClip.x);
         top = intmin(top, mBar[i].rClip.y);
         right = intmax(right, mBar[i].rClip.x + mBar[i].rClip.width);
         bottom = intmax(bottom, mBar[i].rClip.y + mBar[i].rClip.height);

      }
      mAllBarsRect = wxRect(left, top, right-left+1, bottom-top+1);
   }

   CreateIcon(mIconPos.y % 4);

   mLayoutValid = true;
}

void Meter::HandlePaint(wxDC &dc)
{
   int i;

   dc.SetFont(GetFont());
   if (mLeftSize.x == 0) {
      dc.GetTextExtent(mLeftText, &mLeftSize.x, &mLeftSize.y);
      dc.GetTextExtent(mRightText, &mRightSize.x, &mRightSize.y);
   }

   if (!mLayoutValid)
      HandleLayout();

#ifdef __WXMAC__
   dc.DrawBitmap(*mBackgroundBitmap, 0, 0);
#else
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBkgndBrush);
   dc.DrawRectangle(0, 0, mWidth, mHeight);
#endif

   dc.DrawBitmap(*mIcon, mIconPos.x, mIconPos.y);

   dc.SetPen(*wxBLACK_PEN);
   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   wxRect r = mMenuRect;
   dc.DrawRectangle(r);
   for(i=2; i<r.height-2; i++)   // Menu triangle
      dc.DrawLine(r.x + i, r.y + i,
                  r.x + r.width - i, r.y + i);
   dc.DrawLine(r.x + r.width, r.y + 1,
               r.x + r.width, r.y + r.height);
   dc.DrawLine(r.x + 1, r.y + r.height,
               r.x + r.width, r.y + r.height);

   if (mNumBars>0)
      mRuler.Draw(dc);
   
   dc.SetFont(GetFont());
   dc.DrawText(mLeftText, mLeftTextPos.x, mLeftTextPos.y);
   dc.DrawText(mRightText, mRightTextPos.x, mRightTextPos.y);

   for(i=0; i<mNumBars; i++)
      DrawMeterBar(dc, &mBar[i]);
}

void Meter::RepaintBarsNow()
{ 
   if (!mLayoutValid)
      return;

   wxClientDC dc(this);
   int i;

  #ifdef __WXMAC__
   // Mac OS X automatically double-buffers the screen for you,
   // so our bitmap is unneccessary
   for(i=0; i<mNumBars; i++)
      DrawMeterBar(dc, &mBar[i]);
  #else
   if (!mBitmap)
      mBitmap = new wxBitmap(mWidth, mHeight);
   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);
   for(i=0; i<mNumBars; i++)
      DrawMeterBar(memDC, &mBar[i]);
   dc.Blit(mAllBarsRect.x, mAllBarsRect.y,
           mAllBarsRect.width, mAllBarsRect.height,
           &memDC, mAllBarsRect.x, mAllBarsRect.y,
           wxCOPY, false);
  #endif 
}

void Meter::DrawMeterBar(wxDC &dc, MeterBar *meterBar)
{
   wxRect r = meterBar->r;
   wxRect rRMS;
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBkgndBrush);
   dc.DrawRectangle(r);
   AColor::Bevel(dc, false, r);

   /*
   AColor::Dark(&dc, false);
   for(i=0; i<mNumTicks; i++)
      if (meterBar->vert)
         dc.DrawLine(r.x+r.width/2-1, mTick[i], r.x+r.width/2+1, mTick[i]);
      else
         dc.DrawLine(mTick[i], r.y+r.height/2-1, mTick[i], r.y+r.height/2+1);
   */

   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.SetPen(mPen);

   if (meterBar->vert) {
      int ht = (int)(meterBar->peakHold * r.height + 0.5);
      dc.DrawLine(r.x+1, r.y + r.height - ht,
                  r.x+r.width, r.y + r.height - ht);
      if (ht > 1)
         dc.DrawLine(r.x+1, r.y + r.height - ht + 1,
                     r.x+r.width, r.y + r.height - ht + 1);
      ht = (int)(meterBar->peak * r.height + 0.5);
      r = wxRect(r.x, r.y + r.height - ht,
                 r.width, ht);
      ht = (int)(meterBar->rms * r.height + 0.5);
      rRMS = wxRect(r.x, r.y + r.height - ht,
                    r.width, ht);
   }
   else {
      int wd = (int)(meterBar->peakHold * r.width + 0.5);
      dc.DrawLine(r.x + wd, r.y + 1, r.x + wd, r.y + r.height);
      if (wd > 1)
         dc.DrawLine(r.x + wd - 1, r.y + 1, r.x + wd - 1, r.y + r.height);
      wd = (int)(meterBar->peak * r.width + 0.5);
      r = wxRect(r.x, r.y,
                 wd, r.height);
      wd = (int)(meterBar->rms * r.width + 0.5);
      rRMS = wxRect(r.x, r.y,
                    wd, r.height);
   }
   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(mBrush);
   dc.DrawRectangle(r);
   dc.SetBrush(mRMSBrush);
   dc.DrawRectangle(rRMS);

   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.SetPen(mLightPen);
   dc.DrawLine(r.x, r.y, r.x + r.width, r.y);
   dc.DrawLine(r.x, r.y, r.x, r.y + r.height);
   dc.SetPen(mDarkPen);
   dc.DrawLine(r.x + r.width, r.y, r.x + r.width, r.y + r.height);
   dc.DrawLine(r.x, r.y + r.height, r.x + r.width + 1, r.y + r.height);

   if (mClip) {
      if (meterBar->clipping)
         dc.SetBrush(mClipBrush);
      else
         dc.SetBrush(mBkgndBrush);
      dc.SetPen(*wxTRANSPARENT_PEN);
      dc.DrawRectangle(meterBar->rClip);
      dc.SetBrush(*wxTRANSPARENT_BRUSH);
      AColor::Bevel(dc, false, meterBar->rClip);
   }
}

void Meter::StartMonitoring()
{
   if (gAudioIO->IsMonitoring())
      gAudioIO->StopStream();
   else {
      gAudioIO->StartMonitoring(44100.0);
      AudacityProject *p = GetActiveProject();
      if (p) {
         MeterToolBar *bar;
         bar = p->GetMeterToolBar();
         if (bar) {
            Meter *play, *record;
            bar->GetMeters(&play, &record);
            gAudioIO->SetMeters(record, play);
         }
      }
   }
}

//
// Pop-up menu handlers
//

void Meter::OnHorizontal(wxCommandEvent &evt)
{
   SetStyle(HorizontalStereo);
}

void Meter::OnVertical(wxCommandEvent &evt)
{
   SetStyle(VerticalStereo);
}

void Meter::OnMulti(wxCommandEvent &evt)
{
   SetStyle(VerticalMulti);
}

void Meter::OnEqualizer(wxCommandEvent &evt)
{
   SetStyle(Equalizer);
}

void Meter::OnWaveform(wxCommandEvent &evt)
{
   SetStyle(Waveform);
}

void Meter::OnLinear(wxCommandEvent &evt)
{
   mDB = false;
   mLayoutValid = false;
   Refresh(false);
}

void Meter::OnDB(wxCommandEvent &evt)
{
   mDB = true;
   mLayoutValid = false;
   Refresh(false);
}

void Meter::OnClip(wxCommandEvent &evt)
{
}

void Meter::OnMonitor(wxCommandEvent &evt)
{
   StartMonitoring();
}

void Meter::OnFloat(wxCommandEvent &evt)
{
}

void Meter::OnDisable(wxCommandEvent &evt)
{
   ShowWarningDialog(this, "MeterDisableWarning",
                     _("The VU meters will now be disabled.\n\n"
                       "To enable them again, open the Preferences dialog and click on the Interface tab."));

   gPrefs->Write("/GUI/EnableMeterToolBar", false);
   
   if(gMeterToolBarStub) {
      gMeterToolBarStub->UnloadAll();    //Unload all docked METER toolbars
      delete gMeterToolBarStub;
      gMeterToolBarStub = NULL;
   }

   gAudioIO->SetMeters(NULL, NULL);
}
