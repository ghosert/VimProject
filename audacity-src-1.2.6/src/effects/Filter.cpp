/**********************************************************************

  Audacity: A Digital Audio Editor

  Filter.cpp

  Dominic Mazzoni

  This effect performs an FFT filter - it lets the user draw an
  arbitrary envelope (using the same envelope editing code that
  is used to edit the track's amplitude envelope) specifying how much
  to boost or reduce each frequency.

  The filter is applied using overlap/add of Hanning windows.

**********************************************************************/

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/intl.h>

#include "Filter.h"
#include "../Envelope.h"
#include "../FFT.h"
#include "../WaveTrack.h"
#include "../widgets/Ruler.h"

EffectFilter::EffectFilter()
{
   mEnvelope = new Envelope();
   mEnvelope->SetInterpolateDB(false);
   mEnvelope->Flatten(0.5);
   mEnvelope->SetTrackLen(1.0);
   mEnvelope->Mirror(false);

   windowSize = 256;
   filterFunc = new float[windowSize];
}

EffectFilter::~EffectFilter()
{
   delete mEnvelope;
   delete [] filterFunc;
}

bool EffectFilter::PromptUser()
{
   FilterDialog dlog(this, mParent, -1, _("FFT Filter"));
   dlog.SetEnvelope(mEnvelope);

   dlog.CentreOnParent();
   dlog.ShowModal();
   
   if (!dlog.GetReturnCode())
      return false;

   for(int i=0; i<=windowSize/2; i++) {
      double envVal = mEnvelope->GetValue(((double)i)/(windowSize/2));
      filterFunc[i] = (float)(pow(4.0, envVal*2.0 - 1.0));
   }
   
   return true;
}

void EffectFilter::Preview()
{
   for(int i=0; i<=windowSize/2; i++) {
      double envVal = mEnvelope->GetValue(((double)i)/(windowSize/2));
      filterFunc[i] = (float)(pow(4.0, envVal*2.0 - 1.0));
   }

   Effect::Preview();
}

bool EffectFilter::Process()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         longSampleCount start = track->TimeToLongSamples(t0);
         longSampleCount end = track->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);

         if (!ProcessOne(count, track, start, len))
            return false;
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
}

bool EffectFilter::ProcessOne(int count, WaveTrack * track,
                              longSampleCount start, sampleCount len)
{
   sampleCount s = 0;
   sampleCount idealBlockLen = track->GetMaxBlockSize() * 4;
   
   if (idealBlockLen % windowSize != 0)
      idealBlockLen += (windowSize - (idealBlockLen % windowSize));
   
   float *buffer = new float[idealBlockLen];
   
   float *window1 = new float[windowSize];
   float *window2 = new float[windowSize];
   float *thisWindow = window1;
   float *lastWindow = window2;
   
   int i;
   for(i=0; i<windowSize; i++)
      lastWindow[i] = 0;
   
   while((s < len) && ((len-s)>(windowSize/2))) {
      sampleCount block = idealBlockLen;
      if (s + block > len)
         block = len - s;
      
      track->Get((samplePtr) buffer, floatSample, start + s, block);
      
      for(i=0; i<(block-windowSize/2); i+=windowSize/2) {
         int wcopy = windowSize;
         if (i + wcopy > block)
            wcopy = block - i;
         
         int j;
         for(j=0; j<wcopy; j++)
            thisWindow[j] = buffer[i+j];
         for(j=wcopy; j<windowSize; j++)
            thisWindow[j] = 0;
         
         Filter(windowSize, thisWindow);
         
         for(j=0; j<windowSize/2; j++)
            buffer[i+j] = thisWindow[j] + lastWindow[windowSize/2 + j];
         
         float *tempP = thisWindow;
         thisWindow = lastWindow;
         lastWindow = tempP;
      }
      
      // Shift by half-a-window less than the block size we loaded
      // (so that the blocks properly overlap)
      block -= windowSize/2;
      
      track->Set((samplePtr) buffer, floatSample, start + s, block);
      
      s += block;
      
      if (TrackProgress(count, s / (double) len))
         return false;
   }
   
   delete[] buffer;
   delete[] window1;
   delete[] window2;
   
   return true;
}

void EffectFilter::Filter(sampleCount len,
                          float *buffer)
{
   float *inr = new float[len];
   float *ini = new float[len];
   float *outr = new float[len];
   float *outi = new float[len];
   
   int i;
   for(i=0; i<len; i++)
      inr[i] = buffer[i];

   // Apply window and FFT
   /* WindowFunc(3, len, inr); // Hanning window */
   FFT(len, false, inr, NULL, outr, outi);
   
   // Apply filter
   int half = len/2;
   for(i=0; i<=half; i++) {
      int j = len - i;
      
      outr[i] = outr[i]*filterFunc[i];
      outi[i] = outi[i]*filterFunc[i];
      
      if (i!=0 && i!=len/2) {
         outr[j] = outr[j]*filterFunc[i];
         outi[j] = outi[j]*filterFunc[i];
      }
   }

   // Inverse FFT and normalization
   FFT(len, true, outr, outi, inr, ini);
   WindowFunc(3, len, inr); // Hanning window */
   
   for(i=0; i<len; i++)
      buffer[i] = float(inr[i]);

   delete[] inr;
   delete[] ini;
   delete[] outr;
   delete[] outi;
}

//----------------------------------------------------------------------------
// FilterPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(FilterPanel, wxPanel)
    EVT_PAINT(FilterPanel::OnPaint)
    EVT_MOUSE_EVENTS(FilterPanel::OnMouseEvent)
END_EVENT_TABLE()

FilterPanel::FilterPanel( wxWindow *parent, wxWindowID id,
                          const wxPoint& pos,
                          const wxSize& size):
   wxPanel(parent, id, pos, size)
{
   mBitmap = NULL;
   mWidth = 0;
   mHeight = 0;
}

void FilterPanel::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc(this);

   int width, height;
   GetSize(&width, &height);
 
   if (!mBitmap || mWidth!=width || mHeight!=height) {
      if (mBitmap)
         delete mBitmap;

      mWidth = width;
      mHeight = height;
      mBitmap = new wxBitmap(mWidth, mHeight);
   }

   wxColour bkgnd = GetBackgroundColour();
   wxBrush bkgndBrush(bkgnd, wxSOLID);
  
   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);

   wxRect bkgndRect;
   bkgndRect.x = 0;
   bkgndRect.y = 0;
   bkgndRect.width = 40;
   bkgndRect.height = mHeight;
   memDC.SetBrush(bkgndBrush);
   memDC.SetPen(*wxTRANSPARENT_PEN);
   memDC.DrawRectangle(bkgndRect);

   bkgndRect.y = mHeight - 20;
   bkgndRect.width = mWidth;
   bkgndRect.height = 20;
   memDC.DrawRectangle(bkgndRect);

   wxRect border;
   border.x = 40;
   border.y = 0;
   border.width = mWidth;
   border.height = mHeight - 20;

   memDC.SetBrush(*wxWHITE_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   mEnvRect.x = 44;
   mEnvRect.width = mWidth - 50;
   mEnvRect.y = 3;
   mEnvRect.height = mHeight - 26;

   // Pure blue x-axis line
   memDC.SetPen(wxPen(wxColour(0, 0, 255), 1, wxSOLID));
   int center = mEnvRect.height/2;
   memDC.DrawLine(mEnvRect.x, mEnvRect.y + center,
                  mEnvRect.x + mEnvRect.width, mEnvRect.y + center);

   // Med-blue envelope line
   memDC.SetPen(wxPen(wxColour(110, 110, 220), 3, wxSOLID));

   // Draw envelope
   double *values = new double[mEnvRect.width];
   mEnvelope->GetValues(values, mEnvRect.width, 0.0, 1.0/mEnvRect.width);
   int x, y, xlast = 0, ylast = 0;
   for(int i=0; i<mEnvRect.width; i++) {
      x = mEnvRect.x + i;
      y = (int)(mEnvRect.height-mEnvRect.height*values[i]);
      if (i != 0) {
         memDC.DrawLine(xlast, ylast,
                        x, mEnvRect.y + y);
      }
      xlast = x;
      ylast = y;      
   }
   delete[] values;

   memDC.SetPen(*wxBLACK_PEN);
   mEnvRect.y -= 5;
   mEnvelope->Draw(memDC, mEnvRect, 0.0, mEnvRect.width, false, 0.0, 1.0);
   mEnvRect.y += 5;

   // Paint border again
   memDC.SetBrush(*wxTRANSPARENT_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   // Ruler

   Ruler dbRuler;
   dbRuler.SetBounds(0, 0, 40, mHeight-21);
   dbRuler.SetOrientation(wxVERTICAL);
   dbRuler.SetRange(12, -12);
   dbRuler.SetFormat(Ruler::LinearDBFormat);
   dbRuler.SetUnits("dB");
   dbRuler.Draw(memDC);

   Ruler freqRuler;
   freqRuler.SetBounds(41, mHeight-20, mWidth, mHeight);
   freqRuler.SetOrientation(wxHORIZONTAL);
   freqRuler.SetRange(0.0, 22050.0);
   freqRuler.SetFormat(Ruler::IntFormat);
   freqRuler.SetUnits("Hz");
   freqRuler.SetFlip(true);
   freqRuler.Draw(memDC);

   dc.Blit(0, 0, mWidth, mHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
}

void FilterPanel::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown()) {
      CaptureMouse();
   }

   if (mEnvelope->MouseEvent(event, mEnvRect, 0.0, mEnvRect.width, false,
                             0.0, 1.0))
      Refresh(false);

   if (event.ButtonUp()) {
      if (HasCapture())
         ReleaseMouse();
   }
}

// WDR: class implementations

//----------------------------------------------------------------------------
// FilterDialog
//----------------------------------------------------------------------------

enum {
	ID_FILTERPANEL = 10001, 
	ID_CLEAR,
	ID_BUTTON_PREVIEW
};

BEGIN_EVENT_TABLE(FilterDialog,wxDialog)
   EVT_BUTTON( wxID_OK, FilterDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, FilterDialog::OnCancel )
   EVT_SIZE( FilterDialog::OnSize )
   EVT_BUTTON( ID_CLEAR, FilterDialog::OnClear )
   EVT_BUTTON( ID_BUTTON_PREVIEW, FilterDialog::OnPreview )
END_EVENT_TABLE()

FilterDialog::FilterDialog(EffectFilter *effect,
                           wxWindow *parent, wxWindowID id,
                           const wxString &title,
                           const wxPoint &position, const wxSize& size,
                           long style ) :
   wxDialog( parent, id, title, position, size, style | wxRESIZE_BORDER ),
   mEffect(effect)
{
   MakeFilterDialog( this, TRUE ); 
   
   SetSizeHints(300, 200, 20000, 20000);

   SetSize(400, 300);
}

void FilterDialog::SetEnvelope(Envelope *env)
{
   ((FilterPanel *) FindWindow(ID_FILTERPANEL))->mEnvelope = env;
}

bool FilterDialog::Validate()
{
   return TRUE;
}

bool FilterDialog::TransferDataToWindow()
{
   return TRUE;
}

bool FilterDialog::TransferDataFromWindow()
{
   return TRUE;
}

// WDR: handler implementations for FilterDialog

void FilterDialog::OnClear( wxCommandEvent &event )
{
   FilterPanel *panel = ((FilterPanel *) FindWindow(ID_FILTERPANEL));
   panel->mEnvelope->Flatten(0.5);
   panel->mEnvelope->SetTrackLen(1.0);
   panel->Refresh(false);
}

void FilterDialog::OnSize(wxSizeEvent &event)
{
   event.Skip();
}

void FilterDialog::OnPreview(wxCommandEvent &event)
{
   mEffect->Preview();
}

void FilterDialog::OnOk(wxCommandEvent &event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void FilterDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(false);
}

wxSizer *FilterDialog::MakeFilterDialog( wxWindow *parent,
                                         bool call_fit, bool set_sizer )
{
   wxBoxSizer *item0 = new wxBoxSizer( wxVERTICAL );

   wxStaticText *item1 = new wxStaticText( parent, -1,
                       _("FFT Filter by Dominic Mazzoni"), wxDefaultPosition,
                       wxDefaultSize, wxALIGN_CENTRE );
   item0->Add( item1, 0, wxALIGN_CENTRE|wxALL, 5 );

   wxWindow *item2 = new FilterPanel( parent, ID_FILTERPANEL, wxDefaultPosition, wxDefaultSize );
   wxASSERT( item2 );
   item0->Add( item2, 1, wxGROW|wxALIGN_CENTRE|wxALL, 5 );

   wxBoxSizer *item3 = new wxBoxSizer( wxHORIZONTAL );

   wxButton *item4 = new wxButton( parent, ID_CLEAR, _("Clear"), wxDefaultPosition, wxDefaultSize, 0 );
   item3->Add( item4, 0, wxALIGN_CENTRE|wxALL, 5 );

   item3->Add( 20, 20, 1, wxALIGN_CENTRE|wxALL, 5 );

   wxButton *item4b = new wxButton( parent, ID_BUTTON_PREVIEW,
                                    mEffect->GetPreviewName(),
                                    wxDefaultPosition, wxDefaultSize, 0 );
   item3->Add( item4b, 0, wxALIGN_CENTRE|wxALL, 5 );
   item3->Add(20, 8); // horizontal spacer

   wxButton *item6 = new wxButton( parent, wxID_CANCEL, _("Cancel"), wxDefaultPosition, wxDefaultSize, 0 );
   item3->Add( item6, 0, wxALIGN_CENTRE|wxALL, 5 );

   wxButton *item5 = new wxButton( parent, wxID_OK, _("OK"), wxDefaultPosition, wxDefaultSize, 0 );
   item5->SetDefault();
   item5->SetFocus();
   item3->Add( item5, 0, wxALIGN_CENTRE|wxALL, 5 );

   item0->Add( item3, 0, wxGROW|wxALIGN_CENTER_VERTICAL|wxALL, 5 );

   if (set_sizer)
      {
         parent->SetAutoLayout( TRUE );
         parent->SetSizer( item0 );
         if (call_fit)
            {
               item0->Fit( parent );
               item0->SetSizeHints( parent );
            }
      }
    
   return item0;
}

// Implement menu bar functions

// Implement bitmap functions


// End of generated file



