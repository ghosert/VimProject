/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectEqualization.h

  Mitch Golden
  Vaughan Johnson (Preview)

**********************************************************************/

#ifndef __AUDACITY_EFFECT_EQUALIZATION__
#define __AUDACITY_EFFECT_EQUALIZATION__

#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/intl.h>
#include <wx/radiobox.h>

class wxString;

#include "Effect.h"

class Envelope;
class WaveTrack;

class EffectEqualization: public Effect {
   
public:
   
   EffectEqualization();
   virtual ~EffectEqualization();
   
   virtual wxString GetEffectName() {
      return wxString(_("Equalization..."));
   }
   
   virtual wxString GetEffectAction() {
      return wxString(_("Performing Equalization"));
   }
   
   virtual bool PromptUser();
   
   virtual bool Process();

   // Number of samples in an FFT window
   enum {windowSize=16384};

   // Low frequency of the FFT.  20Hz is the
   // low range of human hearing
   enum {loFreqI=20};
   
private:
   bool ProcessOne(int count, WaveTrack * t,
                   sampleCount start, sampleCount len);

   void Filter(sampleCount len,
               float *buffer);
   
   float *mFilterFunc;

public:
   enum curveType {
     acoustic,
     nab, lp, aes, deccaffrrmicro, deccaffrr78, riaa,
     col78, deccaffrrlp, emi78, rcavictor1938, rcavictor1947,
     nCurveTypes
   };

   enum {nCurvePoints=28};
   static const float curvex[];
   static const float curvey[][nCurvePoints];
   static const char * curveNames[];

friend class EqualizationDialog;
};


class EqualizationPanel: public wxPanel
{
public:
   EqualizationPanel( double loFreq, double hiFreq,
		      Envelope *env,
		      wxWindow *parent, wxWindowID id, 
		      const wxPoint& pos = wxDefaultPosition,
		      const wxSize& size = wxDefaultSize);
   ~EqualizationPanel();

   void OnMouseEvent(wxMouseEvent & event);
   void OnPaint(wxPaintEvent & event);

private:

   wxBitmap *mBitmap;
   wxRect mEnvRect;
   int mWidth;
   int mHeight;

   double mLoFreq;
   double mHiFreq;

   Envelope *mEnvelope;

   DECLARE_EVENT_TABLE()
};


// WDR: class declarations

//----------------------------------------------------------------------------
// EqualizationDialog
//----------------------------------------------------------------------------

// Declare window functions

#define ID_TEXT 10000
#define ID_FILTERPANEL 10001
#define ID_CLEAR 10002
#define ID_LOADCURVE 10003
#define ID_BUTTON_PREVIEW 10004

wxSizer *MakeEqualizationDialog( double loFreq, double hiFreq,
				 Envelope *env,
				 EqualizationPanel **pan,
				 wxWindow *parent, bool call_fit = TRUE,
				 bool set_sizer = TRUE );

class EqualizationDialog: public wxDialog
{
public:
   // constructors and destructors
   EqualizationDialog(EffectEqualization * effect,
								double loFreq, double hiFreq,
								float *filterFunc, long windowSize,
								wxWindow *parent, wxWindowID id,
								const wxString &title,
								const wxPoint& pos = wxDefaultPosition,
								const wxSize& size = wxDefaultSize,
								long style = wxDEFAULT_DIALOG_STYLE );
   
   // WDR: method declarations for EqualizationDialog
   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();
   
   wxRadioBox *predefined;
   
private:
   // WDR: member variable declarations for EqualizationDialog
   
private:
   // WDR: handler declarations for EqualizationDialog
   void OnClear( wxCommandEvent &event );
   void OnPreview(wxCommandEvent &event);
   void OnOk( wxCommandEvent &event );
   void OnCancel( wxCommandEvent &event );
   void OnLoadCurve( wxCommandEvent &event );
   void OnSize( wxSizeEvent &event );
   
   void setCurve(Envelope *env, int currentCurve);

private:
	EffectEqualization * m_pEffect;

   double mLoFreq;
   double mHiFreq;
   float *mFilterFunc;
   long mWindowSize;

   EqualizationPanel *mPanel;
   Envelope *mEnvelope;

private:
   DECLARE_EVENT_TABLE()
};

#endif
