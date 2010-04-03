/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectEqualization.cpp

  Mitch Golden
  Vaughan Johnson (Preview)

  Applies an FFT of certain specific equalization curves, suitable
  for old recordings.

  Clone of the FFT Filter effect, see documentation there.

**********************************************************************/

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/intl.h>
#include <wx/string.h>
#include <wx/radiobox.h>

#include "Equalization.h"
#include "../Envelope.h"
#include "../FFT.h"
#include "../WaveTrack.h"
#include "../widgets/Ruler.h"

const float EffectEqualization::curvex[] =
  {
    30., 31., 50., 63., 70., 100., 125., 200., 250., 300.,
    400., 500., 600., 700., 800., 900., 1000., 2000., 3000., 4000.,
    5000., 6000., 7000., 8000., 9000., 10000., 15000., 16000.
  };

const float EffectEqualization::curvey[][nCurvePoints] =
   {
      {
	// acoustic (see p 52)
	-20.0, -20.0, -20.0,   5.0,   4.4,   3.3,   2.5,   1.7,   0.0,   0.0,
	0.0,     0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  -1.5,  -2.5,
	-3.3,   -4.0,  -4.5,  -5.0, -10.0, -15.0, -20.0, -20.0
      },
      {
	// NAB
	20.0,   20.0,  16.0,  15.6,  15.5,  13.8,  12.0,   8.0,   6.2,   5.0,
	3.0,     1.7,   1.0,   0.0,  -0.5,  -1.0,  -1.3,  -4.2,  -6.5,  -8.5,
	-10.2, -12.0, -13.0, -14.0, -15.0, -16.0, -20.0, -20.0
      },
      {
	// LP
	13.5,   13.2,  13.0,  12.8,  12.5,  11.0,  10.5,   8.0,   7.0,   6.0,
	3.5,     2.5,   1.5,   1.0,   0.5,  -0.5,  -1.0,  -3.5,  -6.0,  -8.0,
	-10.0, -11.5, -12.5, -13.5, -14.5, -16.0, -21.2, -22.0
      },
      {
	// AES
	22.5,   22.5,  18.0,  16.0,  15.0,  12.0,  10.0,   6.5,   5.2,   4.5,
	3.0,     2.0,   1.5,   1.0,   0.5,   0.0,   0.0,  -2.2,  -4.0,  -5.5,
	-6.7,   -8.0,  -9.0, -10.0, -11.0, -12.0, -15.5, -16.0
      },
      {
	// Decca FFRR Micro
	14.0,   14.0,  14.0,  13.8,  13.5,  12.5,  11.5,   8.5,   7.2,   6.0,
	4.0,     2.5,   1.5,   1.0,   0.5,   0.0,   0.0,  -1.5,  -3.0,  -4.5,
	-6.0,   -7.0,  -8.0,  -8.5,  -9.0, -10.0, -12.6, -13.0
      },
      {
	// Decca FFRR 78
	22.0,   21.5,  14.0,  11.2,   9.8,   6.0,   2,0,   1.5,   1.0,   0.5,
	0.0,     0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0   -0.5,
	-1.0,   -2.0,  -2.5,  -3.5,  -4.0,  -4.5,  -7.0,  -7.5
      },
      {
	// RIAA
	18.6,   18.5,  17.0,  16.0,  15.3,  13.1,  11.8,   8.2,   7.9,   5.5,
	3.8,     2.7,   2.0,   1.2,   1.0,   0.5,   0.0,  -2.6,  -4.8,  -6.6,
	-8.2,   -9.6, -10.9, -11.9, -12.9, -13.6, -17.2, -18.0
      },
      {
	// Col 78
	16.0,   16.0,  16.0,  14.0,  12.5,  10.0,   8.5,   5.0,   4.0,   3.0,
	2.0,     1.0,   0.5,   0.2,   0.0,  -0.5,  -1.0,  -3.5,  -6.0,  -8.0,
	-10.0, -11.5, -12.5, -13.5, -14.5, -16.0, -21.2, -22.0
      },
      {
	// Decca FFRR LP
	17.5,   17.2,  14.0,  12.0,  11.5,   9.0,   7.5,   5.0,   4.0,   3.0,
	2.0,     1.5,   1.0,   0.7,   0.2,   0.0,   0.0,  -4.0,  -6.7,  -8.5,
	-10.0, -11.0, -12.0, -13.0, -13.2, -14.0, -16.0, -16.0
      },
      {
	// EMI 78
	14.0,   14.0,  14.0,  12.0,  11.0,   8.0,   7.0,   4.0,   3.0,   2.0,
	1.0,     0.5,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,
	0.0,     0.0,   0.0,   0.0,   0.0,   0.0,  -5.0,  -5.0
      },
      {
	// RCA Victor 1938
	24.0,   24.0,  24.0,  21.8,  20.0,  16.0,  13.0,   9.0,   7.5,   6.0,
	4.0,     3.0,   2.0,   1.5,   1.0,   0.5,   0.0,  -2.5,  -5.0,  -6.5,
	-7.5,   -8.0,  -7.7,  -7.5,  -7.5,  -7.5,  -7.5,  -7.5
      },
      {
	// RCA Victor 1947
	24.0,   24.0,  24.0,  21.8,  20.0,  16.0,  13.0,   9.0,   7.5,   6.0,
	4.0,     3.0,   2.0,   1.5,   1.0,   0.5,   0.0,  -2.5,  -5.0,  -6.5,
	-8.0,  -10.0, -11.5, -12.0, -12.5, -12.5, -12.5, -12.5
      }
  };

const char * EffectEqualization::curveNames[] =
  {
    "acoustic",
    "NAB",
    "Columbia LP",
    "AES",
    "Decca FFRR Micro",
    "Decca FFRR 78",
    "RIAA",
    "Columbia 78",
    "Decca FFRR LP",
    "EMI 78",
    "RCA Victor 1938",
    "RCA Victor 1947"
  };



EffectEqualization::EffectEqualization()
{
   mFilterFunc = new float[windowSize];
}


EffectEqualization::~EffectEqualization()
{
   if(mFilterFunc)
      delete[] mFilterFunc;
   mFilterFunc = NULL;
}


bool EffectEqualization::PromptUser()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *t = (WaveTrack *) iter.First();
   float hiFreq = ((float)(t->GetRate())/2.);

   EqualizationDialog dlog(this, ((double)loFreqI), hiFreq, mFilterFunc, windowSize,
			   mParent, -1, _("Equalization"));

   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   return true;
}


bool EffectEqualization::Process()
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


bool EffectEqualization::ProcessOne(int count, WaveTrack * t,
                                 sampleCount start, sampleCount len)
{
   sampleCount s = start;
   sampleCount idealBlockLen = t->GetMaxBlockSize() * 4;
   if (idealBlockLen % windowSize != 0)
      idealBlockLen += (windowSize - (idealBlockLen % windowSize));

   float *buffer = new float[idealBlockLen];

   float *window1 = new float[windowSize];
   float *window2 = new float[windowSize];
   float *thisWindow = window1;
   float *lastWindow = window2;

   sampleCount originalLen = len;

   int i;
   for(i=0; i<windowSize; i++)
      lastWindow[i] = 0;

   TrackProgress(count, 0.);

   while(len) {
      sampleCount block = idealBlockLen;
      if (block > len)
         block = len;

      t->Get((samplePtr)buffer, floatSample, s, block);

      int j;

/* Martyn Shaw set this code back like Filter.cpp
	as the 'lastWindow' was not getting set correctly and one more
	window was being taken from the current block than it should have been */

      for(i=0; i<(block-windowSize/2); i+=windowSize/2) {
         int wcopy = windowSize;
         if (i + wcopy > block)
            wcopy = block - i;

         for(j=0; j<wcopy; j++)
            thisWindow[j] = buffer[i+j];
         for(j=wcopy; j<windowSize; j++)
            thisWindow[j] = 0.;

         Filter(windowSize, thisWindow);

         for(j=0; j<windowSize/2; j++)
            buffer[i+j] = thisWindow[j] + lastWindow[windowSize/2 + j];

         float *tempP = thisWindow;
         thisWindow = lastWindow;
         lastWindow = tempP;
      }

      if (len > block && len > windowSize/2)
	 block -= windowSize/2;

      t->Set((samplePtr)buffer, floatSample, s, block);

      len -= block;
      s += block;

      TrackProgress(count, (s-start)/(double)originalLen);
   }

   delete[] buffer;
   delete[] window1;
   delete[] window2;

   return true;
}

void EffectEqualization::Filter(sampleCount len,
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
   WindowFunc(3, len, inr); // Hanning window
   FFT(len, false, inr, NULL, outr, outi);

   // Apply filter
   int half = len/2;
   for(i=0; i<=half; i++) {
      int j = len - i;

      outr[i] = outr[i]*mFilterFunc[i];
      outi[i] = outi[i]*mFilterFunc[i];

      if (i!=0 && i!=len/2) {
         outr[j] = outr[j]*mFilterFunc[i];
         outi[j] = outi[j]*mFilterFunc[i];
      }
   }

   // Inverse FFT and normalization
   FFT(len, true, outr, outi, inr, ini);

   for(i=0; i<len; i++)
      buffer[i] = float(inr[i]);

   delete[] inr;
   delete[] ini;
   delete[] outr;
   delete[] outi;
}


//----------------------------------------------------------------------------
// EqualizationPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EqualizationPanel, wxPanel)
    EVT_PAINT(EqualizationPanel::OnPaint)
    EVT_MOUSE_EVENTS(EqualizationPanel::OnMouseEvent)
END_EVENT_TABLE()

EqualizationPanel::EqualizationPanel( double loFreq, double hiFreq,
				      Envelope *env,
				      wxWindow *parent, wxWindowID id,
				      const wxPoint& pos,
				      const wxSize& size):
   wxPanel(parent, id, pos, size)
{
   mBitmap = NULL;
   mWidth = 0;
   mHeight = 0;
   mLoFreq = loFreq;
   mHiFreq = hiFreq;

   mEnvelope = env;
   mEnvelope->Flatten(0.5);
   mEnvelope->Mirror(false);
   mEnvelope->SetTrackLen(1.0);

   SetSizeHints(100, 80);
}


EqualizationPanel::~EqualizationPanel()
{
   if(mBitmap)
      delete mBitmap;
   mBitmap = NULL;
}


void EqualizationPanel::OnPaint(wxPaintEvent & evt)
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
   dbRuler.SetRange(30, -30);
   dbRuler.SetFormat(Ruler::LinearDBFormat);
   dbRuler.SetUnits("dB");
   dbRuler.Draw(memDC);

   Ruler freqRuler;
   freqRuler.SetBounds(41, mHeight-20, mWidth, mHeight);
   freqRuler.SetOrientation(wxHORIZONTAL);
   freqRuler.SetLog(true);
   freqRuler.SetRange(mLoFreq, mHiFreq);
   freqRuler.SetFormat(Ruler::IntFormat);
   freqRuler.SetUnits("Hz");
   freqRuler.SetFlip(true);
   freqRuler.Draw(memDC);

   dc.Blit(0, 0, mWidth, mHeight,
           &memDC, 0, 0, wxCOPY, FALSE);
}

void EqualizationPanel::OnMouseEvent(wxMouseEvent & event)
{
   if (event.ButtonDown()) {
      CaptureMouse();
   }

   if (mEnvelope->MouseEvent(event, mEnvRect, 0.0, mEnvRect.width, false,
                             0.0, 1.0))
      Refresh(false);

   if (event.ButtonUp()) {
      ReleaseMouse();
   }
}

// WDR: class implementations

//----------------------------------------------------------------------------
// EqualizationDialog
//----------------------------------------------------------------------------

// WDR: event table for EqualizationDialog

BEGIN_EVENT_TABLE(EqualizationDialog,wxDialog)
   EVT_BUTTON( wxID_OK, EqualizationDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, EqualizationDialog::OnCancel )
   EVT_SIZE( EqualizationDialog::OnSize )
   EVT_BUTTON( ID_CLEAR, EqualizationDialog::OnClear )
   EVT_BUTTON( ID_LOADCURVE, EqualizationDialog::OnLoadCurve )
	EVT_BUTTON(ID_BUTTON_PREVIEW, EqualizationDialog::OnPreview)
END_EVENT_TABLE()

EqualizationDialog::EqualizationDialog(EffectEqualization * effect,
					double loFreq, double hiFreq,
					float *filterFunc,
					long windowSize,
					wxWindow *parent, wxWindowID id,
					const wxString &title,
					const wxPoint &position,
					const wxSize& size,
					long style):
   wxDialog( parent, id, title, position, size, style | wxRESIZE_BORDER )
{
	m_pEffect = effect;

   mEnvelope = new Envelope();
   mEnvelope->SetInterpolateDB(false);
   mEnvelope->Mirror(false);

   MakeEqualizationDialog(loFreq, hiFreq,
			  mEnvelope,
			  &mPanel,
			  this);

   mLoFreq = loFreq;
   mHiFreq = hiFreq;

   mFilterFunc = filterFunc;
   mWindowSize = windowSize;
}

bool EqualizationDialog::Validate()
{
   return TRUE;
}

bool EqualizationDialog::TransferDataToWindow()
{
   return TRUE;
}

bool EqualizationDialog::TransferDataFromWindow()
{
   double loLog = log10(mLoFreq);
   double hiLog = log10(mHiFreq);
   double denom = hiLog - loLog;

   double delta = mHiFreq / ((double)(mWindowSize/2.));
   double val0 = 30.*((mEnvelope->GetValue(0.0))-0.5);
   double val1 = 30.*((mEnvelope->GetValue(1.0))-0.5);

   mFilterFunc[0] = val0;
   double freq = delta;

   int i;
   for(i=1; i<=mWindowSize/2; i++) {
      double when = (log10(freq) - loLog)/denom;
      if(when < 0.) {
         mFilterFunc[i] = val0;
      }
      else if(when > 1.0) {
	 mFilterFunc[i] = val1;
      }
      else {
	 mFilterFunc[i] = 30.*((mEnvelope->GetValue(when))-0.5);
      }
      freq += delta;
   }

   for(i=0;i<mWindowSize/2;i++) {
      mFilterFunc[i] = (float)(pow(10., mFilterFunc[i]/20.));
   }

   return TRUE;
}

// WDR: handler implementations for EqualizationDialog

void EqualizationDialog::OnClear( wxCommandEvent &event )
{
   mEnvelope->Flatten(0.5);
   mEnvelope->SetTrackLen(1.0);
   mPanel->Refresh(false);
}

void EqualizationDialog::OnSize(wxSizeEvent &event)
{
   event.Skip();
}

void EqualizationDialog::OnPreview(wxCommandEvent &event)
{
   TransferDataFromWindow();
	m_pEffect->Preview();
	//v Restore previous values?
}

void EqualizationDialog::OnOk(wxCommandEvent &event)
{
   TransferDataFromWindow();

   if (Validate()) {
     if(mEnvelope)
        delete mEnvelope;
     mEnvelope = NULL;
     mPanel = NULL;

     EndModal(true);
   }
   else {
      event.Skip();
   }
}

void EqualizationDialog::OnCancel(wxCommandEvent &event)
{
   if(mEnvelope)
     delete mEnvelope;
   mEnvelope = NULL;
   mPanel = NULL;

   EndModal(false);
}


void EqualizationDialog::OnLoadCurve(wxCommandEvent &event)
{
   setCurve(mEnvelope, predefined->GetSelection());
   mPanel->Refresh(false);
}


void EqualizationDialog::setCurve(Envelope *env, int currentCurve)
{
   env->Flatten(0.5);
   env->SetTrackLen(1.0);

   double when = 0.;
   double value = (EffectEqualization::curvey[currentCurve][0]/60.) + 0.5;
   env->Move(when, value);
   double loLog = log10(20.);
   double hiLog = log10(mHiFreq);
   double denom = hiLog - loLog;
   int i;
   for(i=0;i<EffectEqualization::nCurvePoints;i++) {
      when = (log10(EffectEqualization::curvex[i]) - loLog)/denom;
      value = (EffectEqualization::curvey[currentCurve][i]/60.) + 0.5;
      if(when < 1)
	 env->Insert(when, value);
      else
	 break;
   }
   i--;
   when = 1.;
   value = (EffectEqualization::curvey[currentCurve][i]/60.) + 0.5;
   env->Move(when, value);
}


wxSizer * MakeEqualizationDialog(
				 double loFreq, double hiFreq,
				 Envelope *env,
				 EqualizationPanel **pan,
				 wxWindow *parent, bool call_fit,
				 bool set_sizer )
{
   wxBoxSizer *item0 = new wxBoxSizer( wxVERTICAL );

	wxStaticText *item1 =
		new wxStaticText(parent, -1,
							  _("Equalization, by Mitch Golden && Vaughan Johnson"),
							  wxDefaultPosition, wxDefaultSize, wxALIGN_CENTER);
	item0->Add( item1, 0, wxALIGN_CENTRE|wxALL, 4 );

   (*pan) = new EqualizationPanel( loFreq, hiFreq,
				   env,
				   parent, ID_FILTERPANEL,
				   wxDefaultPosition, wxSize(100,80) );
   wxWindow *item2 = (*pan);
   wxASSERT( item2 );
   item0->Add( item2, 1, wxGROW|wxALIGN_CENTRE|wxALL, 4);


   wxBoxSizer *item3 = new wxBoxSizer( wxHORIZONTAL );

   wxButton *item4a = new wxButton( parent, ID_LOADCURVE, _("Load Predefined Curve"),
                       wxDefaultPosition, wxDefaultSize, 0 );
   item3->Add( item4a, 0, wxALIGN_CENTRE|wxLEFT, 4);

   item3->Add(80, 4); // horizontal spacer

   wxButton *item4b = new wxButton( parent, ID_CLEAR, _("Clear"),
                       wxDefaultPosition, wxDefaultSize, 0 );
   item3->Add( item4b, 0, wxALIGN_CENTRE|wxRIGHT, 4);

   item0->Add( item3, 0, wxALIGN_CENTER | wxALL, 0);


	// predefined curves
	wxBoxSizer *item9 = new wxBoxSizer( wxVERTICAL );

   wxString formats[EffectEqualization::nCurveTypes];
   int i;

   for(i=0;i<EffectEqualization::nCurveTypes;i++) {
     formats[i] = EffectEqualization::curveNames[i];
   }

   wxRadioBox *predefined = new wxRadioBox(parent, -1, _("Predefined:"),
			       wxDefaultPosition, wxDefaultSize,
			       EffectEqualization::nCurveTypes,
			       formats,
			       3,
			       wxRA_SPECIFY_COLS);

   ((EqualizationDialog *)parent)->predefined = predefined;

   item9->Add(predefined, 0, wxALIGN_CENTER | wxALL, 4);

   item0->Add( item9, 0, wxGROW|wxALIGN_CENTER, 4);


	// Preview, OK, & Cancel buttons
   wxBoxSizer * pBoxSizer_OK = new wxBoxSizer(wxHORIZONTAL);

   wxButton * pButton_Preview =
		new wxButton(parent, ID_BUTTON_PREVIEW,
							_("Preview"), //v Should be m_pEffect->GetPreviewName());
							wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_OK->Add(pButton_Preview, 0, wxALIGN_LEFT | wxALL, 4);

   pBoxSizer_OK->Add(80, 4); // horizontal spacer

   wxButton * pButton_Cancel =
       new wxButton(parent, wxID_CANCEL, _("Cancel"),
							wxDefaultPosition, wxDefaultSize, 0);
   pBoxSizer_OK->Add(pButton_Cancel, 0, wxALIGN_RIGHT | wxALL, 4);

   wxButton * pButton_OK =
		new wxButton(parent, wxID_OK, _("OK"), wxDefaultPosition, wxDefaultSize, 0);
   pButton_OK->SetDefault();
   pButton_OK->SetFocus();
   pBoxSizer_OK->Add(pButton_OK, 0, wxALIGN_RIGHT | wxALL, 4);

   item0->Add(pBoxSizer_OK, 0, wxALIGN_CENTER | wxALL, 4);


   if (set_sizer)
      {
         parent->SetSizer( item0 );
         if (call_fit)
            {
               item0->Fit( parent );
               item0->SetSizeHints( parent );
            }
      }

   return item0;
}
