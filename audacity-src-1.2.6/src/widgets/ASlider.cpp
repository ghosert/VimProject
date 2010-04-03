/**********************************************************************

  Audacity: A Digital Audio Editor

  ASlider.cpp

  Dominic Mazzoni

  This class is a custom slider (currently used just for the volume
  control).  It is not very customizable in the sense that the
  background image must be exactly the size you want it, but it does
  allow for a slicker look and feel by allowing you to use images
  for the slider background and the thumb.

**********************************************************************/

#include <math.h>

#include <wx/defs.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/msgdlg.h>
#include <wx/panel.h>
#include <wx/tooltip.h>
#include <wx/debug.h>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#define USE_POPUPWIN 1
#endif

#if USE_POPUPWIN
#include <wx/popupwin.h>
#endif

#include "ASlider.h"


#include "../AColor.h"
#include "../ImageManipulation.h"
#include "../Project.h"

#include "../../images/SliderThumb.xpm"

#if defined __WXMSW__
const int sliderFontSize = 10;
#else
const int sliderFontSize = 12;
#endif

//
// TipPanel
//

#if USE_POPUPWIN
class TipPanel : public wxPopupWindow
#else
class TipPanel : public wxPanel
#endif
{
 public:
   TipPanel(wxWindow * parent, wxWindowID id,
            wxString label,
            const wxPoint &pos);
   
   void SetPos(const wxPoint &pos);

   void OnPaint(wxPaintEvent & event);

   wxString label;
   wxString origLabel;

   wxWindow *mParent;

   DECLARE_EVENT_TABLE()
};

#if USE_POPUPWIN

BEGIN_EVENT_TABLE(TipPanel, wxPopupWindow)
   EVT_PAINT(TipPanel::OnPaint)
END_EVENT_TABLE()

TipPanel::TipPanel(wxWindow *parent, wxWindowID id,
                   wxString label, const wxPoint &pos):
   wxPopupWindow(parent)
{
   this->label = label;
   this->origLabel = label;
   mParent = parent;
   SetPos(pos);
}

void TipPanel::SetPos(const wxPoint& pos)
{
   int x = pos.x;
   int y = pos.y;

   if (mParent)
      mParent->ClientToScreen(&x,&y);

   wxClientDC dc(this);
   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc.SetFont(labelFont);
   int width, height;
   dc.GetTextExtent(origLabel, &width, &height);
   height += 4;
   SetSize(x - width/2, y, width, height);
}

#else

BEGIN_EVENT_TABLE(TipPanel, wxPanel)
   EVT_PAINT(TipPanel::OnPaint)
END_EVENT_TABLE()

TipPanel::TipPanel(wxWindow *parent, wxWindowID id,
                   wxString label,
                   const wxPoint &pos):
   wxPanel(parent, id)
{
   this->label = label;
   this->origLabel = label;
   SetPos(pos);
}

void TipPanel::SetPos(const wxPoint& pos)
{
   wxClientDC dc(this);
   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc.SetFont(labelFont);
   int width, height;
   dc.GetTextExtent(origLabel, &width, &height);
   width += 4;
   height += 4;
   int left = pos.x - width/2;
   if (left < 0)
      left = 0;
   SetSize(left, pos.y, width, height);   
}

#endif

void TipPanel::OnPaint(wxPaintEvent& event)
{
   wxPaintDC dc(this);
   int width, height, textWidth, textHeight;

   wxFont labelFont(sliderFontSize, wxSWISS, wxNORMAL, wxNORMAL);
   dc.SetFont(labelFont);
   GetClientSize(&width, &height);
   dc.SetPen(*wxBLACK_PEN);
   dc.SetBrush(AColor::tooltipBrush);
   dc.DrawRectangle(0, 0, width, height);
   dc.GetTextExtent(label, &textWidth, &textHeight);
   dc.DrawText(label, (width-textWidth)/2, (height-textHeight)/2);
}

//
// LWSlider
//

// Construct customizable slider
LWSlider::LWSlider(wxWindow * parent,
         wxString name,
         const wxPoint &pos,
         const wxSize &size,
         float minValue,
         float maxValue,
         float stepValue,
         bool canUseShift,
         int style,
         bool heavyweight /* = false */
         )
{
   Init(parent, name, pos, size, minValue, maxValue,
      stepValue, canUseShift, style, heavyweight);
}

// Construct predefined slider
LWSlider::LWSlider(wxWindow *parent,
                   wxString name,
                   const wxPoint &pos,
                   const wxSize &size,
                   int style,
                   bool heavyweight /* = false */)
{
   wxString leftLabel, rightLabel;
   float minValue, maxValue, stepValue;

   switch(style)
   {
   case PAN_SLIDER:
      minValue = -1.0f;
      maxValue = +1.0f;
      stepValue = 0.1f;
      break;
   case DB_SLIDER:
      minValue = -36.0f;
      maxValue = 36.0f;
      stepValue = 3.0f;
      break;
   case FRAC_SLIDER:
      minValue = 0.0f;
      maxValue = 1.0f;
      stepValue = STEP_CONTINUOUS;
      break;
   default:
      minValue = 0.0f;
      maxValue = 1.0f;
      stepValue = 0.0f;
      wxASSERT(false); // undefined style
   }

   Init(parent, name, pos, size, minValue, maxValue, stepValue,
        true, style, heavyweight);
}

void LWSlider::Init(wxWindow * parent,
     wxString name,
     const wxPoint &pos,
     const wxSize &size,
     float minValue,
     float maxValue,
     float stepValue,
     bool canUseShift,
     int style,
     bool heavyweight /* = false */
     )
{
   mName = name;
   mStyle = style;
   mIsDragging = false;
   mWidth = size.x;
   mHeight = size.y;
   mParent = parent;
   mHW = heavyweight;
   mID = -1;
   mMinValue = minValue;
   mMaxValue = maxValue;
   mStepValue = stepValue;
   mCanUseShift = canUseShift;
   mCurrentValue = 0.0f;

   if (!(mWidth & 0))
      mWidth--;

   mCenterY = mHeight - 9;

   wxMemoryDC *dc = new wxMemoryDC();
   wxBitmap *bitmap = new wxBitmap(8, 8);
   dc->SelectObject(*bitmap);

   AColor::Medium(dc, false);
   wxColour bkgnd = dc->GetBrush().GetColour();
   AColor::Medium(dc, true);   
   wxColour selBkgnd = dc->GetBrush().GetColour();

   wxImage *backgroundImage =
      CreateSysBackground(mWidth, mHeight, 0, bkgnd);
#if wxCHECK_VERSION(2, 5, 0)
   wxBitmap backgroundBitmap(backgroundImage);
#else
   wxBitmap backgroundBitmap =
      backgroundImage->ConvertToBitmap();
#endif
   wxImage *thumbImage = new wxImage(wxBitmap(SliderThumb).ConvertToImage());
   wxImage *thumb1 = ChangeImageColour(thumbImage, bkgnd);
   wxImage *thumb2 = ChangeImageColour(thumbImage, selBkgnd);
   mThumbBitmap = new wxBitmap(thumb1);
   mSelThumbBitmap = new wxBitmap(thumb2);
   delete thumb1;
   delete thumb2;
   delete thumbImage;

   delete dc;
   delete bitmap;

   mThumbWidth = mThumbBitmap->GetWidth();
   mThumbHeight = mThumbBitmap->GetHeight();

   mLeftX = mThumbWidth/2;
   mRightX = mWidth - mThumbWidth/2 - 1;
   mWidthX = mRightX - mLeftX;

   int i;
   for(i=0; i<2; i++) {
      wxBitmap *bitmap;

      bitmap = new wxBitmap(mWidth, mHeight);
      wxMemoryDC *dc = new wxMemoryDC();
      dc->SelectObject(*bitmap);

      if (mHW) {
         dc->DrawBitmap(backgroundBitmap, 0, 0);
      }
      else {
         AColor::Medium(dc, i==1);
         dc->DrawRectangle(-1, -1, mWidth+2, mHeight+2);
      }

      AColor::Medium(dc, i==1);
      dc->DrawLine(mLeftX, mCenterY, mRightX+2, mCenterY);
      AColor::Dark(dc, false);
      dc->DrawLine(mLeftX, mCenterY+1, mRightX+2, mCenterY+1);

      int divs = 10;
      double upp = divs / (double)(mWidthX-1);
      double d = 0;
      int int_d = -1;
      for(int p=0; p<=mWidthX; p++) {
         if (((int)d) > int_d) {
            int_d = (int)d;
            int ht = (int_d==0 || int_d==divs? 5: 3);
            AColor::Light(dc, false);
            dc->DrawLine(mLeftX+p, mCenterY-ht, mLeftX+p, mCenterY);
            AColor::Dark(dc, false);
            dc->DrawLine(mLeftX+p+1, mCenterY-ht+1, mLeftX+p+1, mCenterY);
         }
         d += upp;
      }

      if (style == PAN_SLIDER)
      {
         AColor::SetLabelFont(*dc);

         /* i18n-hint: One-letter abbreviation for Left, in the Pan slider */
         dc->DrawText(_("L"), mLeftX, 0);

         int width, height;
         /* i18n-hint: One-letter abbreviation for Right, in the Pan slider */
         dc->GetTextExtent(_("R"), &width, &height);
         dc->DrawText(_("R"), mRightX-width+1, 0);
      } else
      {
         // draw the '-' and the '+'
         dc->SetPen(*wxBLACK_PEN);
         dc->DrawLine(mLeftX, mCenterY-10, mLeftX+5, mCenterY-10);
         dc->DrawLine(mRightX-5, mCenterY-10, mRightX+0, mCenterY-10);
         dc->DrawLine(mRightX-3, mCenterY-12, mRightX-3, mCenterY-7);
      }
      
      delete dc;

      if (i==0)
         mBitmap = bitmap;
      else
         mSelBitmap = bitmap;
   }

   delete backgroundImage;

   int x=mWidth/2, y=mHeight, wx, wy;
   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top->GetPosition(&wx, &wy);
      x += wx;
      y += wy;
      top = top->GetParent();
   }
   
   mPopWin = NULL;
   Move(pos);
   CreatePopWin();
}

LWSlider::~LWSlider()
{
   delete mBitmap;
   delete mSelBitmap;
   delete mThumbBitmap;
   delete mSelThumbBitmap;
   delete mPopWin;
}

void LWSlider::SetId(wxWindowID id)
{
   mID = id;
}

void LWSlider::CreatePopWin()
{
   if (mPopWin)
      return;

   wxString maxStr = mName + ": 000000";

   if (mStyle == PAN_SLIDER || mStyle == DB_SLIDER)
      maxStr += "000";

   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top = top->GetParent();
   }

   mPopWin = new TipPanel(top, -1, maxStr, wxDefaultPosition);
   mPopWin->Hide();
}

void LWSlider::SetPopWinPosition()
{
   int x, y, wx, wy;

   x=mWidth/2 + mLeft;
   y=mHeight + mTop + 1;
   wxWindow *top = mParent;
   while(top && !top->IsTopLevel()) {
      top->GetPosition(&wx, &wy);
      x += wx;
      y += wy;
      top = top->GetParent();
   }

   if (mPopWin)
      ((TipPanel *)mPopWin)->SetPos(wxPoint(x, y));
}

void LWSlider::Move(const wxPoint &newpos)
{
   mLeft = newpos.x;
   mTop = newpos.y;
}

void LWSlider::RecreateTipWin()
{
   delete mPopWin;
   mPopWin = NULL;
   CreatePopWin();
}

void LWSlider::OnPaint(wxDC &dc, bool selected)
{
   //thumbPos should be in pixels
   int thumbPos = ValueToPosition(mCurrentValue);
   int thumbY = mCenterY - (mThumbHeight/2);
   wxBitmap *bitmap;
   wxBitmap *thumbBitmap;

   if (selected) {
      bitmap = mSelBitmap;
      thumbBitmap = mSelThumbBitmap;
   }
   else {
      bitmap = mBitmap;
      thumbBitmap = mThumbBitmap;
   }

#if defined(__WXMAC__) || defined(__WXMSW__)
   dc.DrawBitmap(*bitmap, mLeft, mTop);
   dc.DrawBitmap(*thumbBitmap, mLeft+thumbPos, mTop+thumbY);
#else
   wxMemoryDC memDC;
   memDC.SelectObject(*bitmap);
   dc.Blit(mLeft, mTop, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);
   memDC.SelectObject(*thumbBitmap);
   dc.Blit(mLeft+thumbPos, mTop+thumbY, mThumbWidth, mThumbHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
#endif

   if (mPopWin)
      mPopWin->Refresh();
}

void LWSlider::FormatPopWin()
{
   wxString label;
   wxString valstr;

   switch(mStyle) {
   case FRAC_SLIDER:
      label.Printf("%s: %.1f", (const char *)mName, mCurrentValue);
      break;
   case DB_SLIDER:
      valstr.Printf("%.1f", mCurrentValue);
      if (valstr.Right(1) == "0")
         valstr = valstr.Left(valstr.Length() - 2);
      if (mCurrentValue > 0)
         valstr = "+" + valstr;
      
      label.Printf("%s: %s dB", (const char*)mName, (const char *)valstr);
      break;
   case PAN_SLIDER:
      if (mCurrentValue == 0.0)
         label.Printf("%s: %s", (const char *)mName,
                      _("Center"));
      else {
         if (mCurrentValue < 0.0)
            label.Printf("%s: %.0f%% %s", (const char *)mName,
                      -mCurrentValue * 100.0f, _("Left"));
         else /* if (val > 0.0) */
            label.Printf("%s: %.0f%% %s", (const char *)mName,
                      mCurrentValue * 100.0f, _("Right"));
      }
         
      break;
   }

   ((TipPanel *)mPopWin)->label = label;
}

void LWSlider::OnMouseEvent(wxMouseEvent & event)
{
   if (event.Entering()) {
      #if wxUSE_TOOLTIPS // Not available in wxX11
      // Display the tooltip in the status bar
      if (mParent->GetToolTip()) {
         wxString tip = mParent->GetToolTip()->GetTip();
         GetActiveProject()->TP_DisplayStatusMessage(tip, 0);
         Refresh();
      }
      #endif
   }
   else if (event.Leaving()) {
      GetActiveProject()->TP_DisplayStatusMessage("",0);
      Refresh();
   }
   
   float prevValue = mCurrentValue;

   if (event.ButtonDown()) {

      //This jumps the thumb to clicked position
      if (!mIsDragging) {
         mCurrentValue = PositionToValue(event.m_x, event.ShiftDown());

         mIsDragging = true;
         mParent->CaptureMouse();

         FormatPopWin();
         SetPopWinPosition();
         mPopWin->Show();
      }

      // Don't generate notification yet
      return;

   } else if (event.ButtonUp() && mIsDragging) {
      mIsDragging = false;
      mParent->ReleaseMouse();
      mPopWin->Hide();
      ((TipPanel *)mPopWin)->SetPos(wxPoint(-1000, -1000));
   } else if (event.Dragging() && mIsDragging) {
      mCurrentValue = PositionToValue(event.m_x, event.ShiftDown());
   }

   if (prevValue != mCurrentValue) {
      FormatPopWin();
      mPopWin->Refresh();
      Refresh();

      wxCommandEvent *e =
         new wxCommandEvent(wxEVT_COMMAND_SLIDER_UPDATED, mID);
      int intValue =
         (int)((mCurrentValue - mMinValue) * 1000.0f
         / (mMaxValue - mMinValue));
      e->SetInt( intValue );
      mParent->ProcessEvent(*e);
      delete e;
   }
}

int LWSlider::ValueToPosition(float val)
{
   return (int)rint((val - mMinValue) * mWidthX / (mMaxValue - mMinValue));
}

float LWSlider::PositionToValue(int xPos, bool shiftDown)
{
   int pos = (xPos - mLeft) - mLeftX;

   // MM: Special cases: If position is at the very left or the
   // very right, set minimum/maximum value without other checks
   if (pos <= 0)
      return mMinValue;
   if (pos >= mWidthX)
      return mMaxValue;
   
   float val = ((float)pos/(float)mWidthX) 
      * (mMaxValue - mMinValue) + mMinValue;

   if (!(mCanUseShift && shiftDown) && mStepValue != STEP_CONTINUOUS)
   {
      // MM: If shift is not down, or we don't allow usage
      // of shift key at all, trim value to steps of
      // provided size.
      val = (int)(val / mStepValue + 0.5 * (val>0?1.0f:-1.0f)) * mStepValue;
   }

   return val;
}

float LWSlider::Get()
{
   if (mStyle == DB_SLIDER)
      return pow(10.0f, mCurrentValue / 20.0f);
   else
      return mCurrentValue;
}

void LWSlider::Set(float value)
{
   if(mIsDragging)
      return;

   if (mStyle == DB_SLIDER)
      mCurrentValue = 20.0f*log10(value);
   else
      mCurrentValue = value;

   if (mCurrentValue < mMinValue)
      mCurrentValue = mMinValue;
   if (mCurrentValue > mMaxValue)
      mCurrentValue = mMaxValue;

   Refresh();
}

void LWSlider::Refresh()
{
   if (mHW)
      mParent->Refresh(false);
}

//
// ASlider
//

BEGIN_EVENT_TABLE(ASlider, wxWindow)
   EVT_MOUSE_EVENTS(ASlider::OnMouseEvent)
   EVT_PAINT(ASlider::OnPaint)
END_EVENT_TABLE()

ASlider::ASlider(wxWindow * parent, wxWindowID id,
                 wxString name,
                 const wxPoint & pos,
                 const wxSize & size):
   wxWindow(parent, id, pos, size)
{
   mLWSlider = new LWSlider(this, name, wxPoint(0, 0), size,
                            FRAC_SLIDER, true);
   mLWSlider->SetId(id);
}

ASlider::~ASlider()
{
   delete mLWSlider;
}

void ASlider::OnPaint(wxPaintEvent &event)
{
   wxPaintDC dc(this);

   mLWSlider->OnPaint(dc, false);
}

void ASlider::OnMouseEvent(wxMouseEvent &event)
{
   mLWSlider->OnMouseEvent(event);
}

void ASlider::RecreateTipWin()
{
   mLWSlider->RecreateTipWin();
}

float ASlider::Get()
{
   return mLWSlider->Get();
}

void ASlider::Set(float value)
{
   mLWSlider->Set(value);
}
