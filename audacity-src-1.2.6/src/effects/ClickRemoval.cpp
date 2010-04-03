/**********************************************************************

  Audacity: A Digital Audio Editor

  ClickRemoval.cpp

  Craig DeForest

  Clicks are identified as small regions of high amplitude compared
  to the surrounding chunk of sound.  Anything sufficiently tall compared
  to a large (2048 sample) window around it, and sufficiently narrow,
  is considered to be a click.

  The structure was largely stolen from Domonic Mazzoni's NoiseRemoval
  module, and reworked for the new effect.

  This file is intended to become part of Audacity.  You may modify
  and/or distribute it under the same terms as Audacity itself.

**********************************************************************/

#include "../Audacity.h"

#include <math.h>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

#ifdef __MACOSX__
#include <math.h>
#endif

#ifdef __MACOS9__
#include <fp.h>
#define finite(x) isfinite(x)
#endif

#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/brush.h>
#include <wx/image.h>
#include <wx/dcmemory.h>
#include <wx/statbox.h>
#include <wx/intl.h>

#include "ClickRemoval.h"
#include "../Envelope.h"
// #include "../FFT.h"
#include "../WaveTrack.h"

EffectClickRemoval::EffectClickRemoval()
{
   windowSize = 8192;
   level = 200;
   width = 20;
   sep=2049;
}

EffectClickRemoval::~EffectClickRemoval()
{
}

bool EffectClickRemoval::PromptUser()
{
   ClickRemovalDialog dlog(this, mParent, -1, _("Click and Pop Removal"));
   dlog.m_pSlider->SetValue(level);
   dlog.m_pSlider_width->SetValue(width);
   //   dlog.m_pSlider_sep->SetValue(sep);
   dlog.m_pButton_RemoveClicks->SetDefault();
   dlog.m_pButton_RemoveClicks->SetFocus();

   dlog.CentreOnParent();
   dlog.ShowModal();
   
   if (dlog.GetReturnCode() == 0)
      return false;

   level = dlog.m_pSlider->GetValue();
   width = dlog.m_pSlider_width->GetValue();
   //   sep = dlog.m_pSlider_sep->GetValue();

   return true;
}

bool EffectClickRemoval::Process()
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

bool EffectClickRemoval::ProcessOne(int count, WaveTrack * track,
                                    longSampleCount start, sampleCount len)
{
   sampleCount s = 0;
   sampleCount idealBlockLen = track->GetMaxBlockSize() * 4;

   if (idealBlockLen % windowSize != 0)
      idealBlockLen += (windowSize - (idealBlockLen % windowSize));
   
   float *buffer = new float[idealBlockLen];
   
   float *datawindow = new float[windowSize];
   
   int i;

   while((s < len)&&((len-s)>(windowSize/2))) {
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
            datawindow[j] = buffer[i+j];
         for(j=wcopy; j<windowSize; j++)
            datawindow[j] = 0;
         
	 RemoveClicks(windowSize, datawindow);

	 for(j=0; j<wcopy; j++)
	   buffer[i+j] = datawindow[j];
	 
      }
      
      track->Set((samplePtr) buffer, floatSample, start + s, block);
      
      s += block;
      
      if (TrackProgress(count, s / (double) len))
         return false;
   }
   
   delete[] buffer;
   delete[] datawindow;
   
   return true;
}

void EffectClickRemoval::RemoveClicks(sampleCount len,
                                     float *buffer)
{
   int i;
   int j;
   int left = 0;

   float msw;
   int ww;
   int s2 = sep/2; 
   float *ms_seq = new float[len];
   float *b2 = new float[len];

   for( i=0; i<len; i++)
     b2[i] = buffer[i]*buffer[i];

   /* Shortcut for rms - multiple passes through b2, accumulating
    * as we go.
    */
   for(i=0;i<len;i++)
     ms_seq[i]=b2[i];

   for(i=1; i < sep; i *= 2) {
     for(j=0;j<len-i; j++)
       ms_seq[j] += ms_seq[j+i];
   }
   /* Cheat by truncating sep to next-lower power of two... */
   sep = i;

   for( i=0; i<len-sep; i++ ) 
     ms_seq[i] /= sep;

   /* ww runs from about 4 to width.  wrc is the reciprocal; 
    * chosen so that integer roundoff doesn't clobber us.
    */
   int wrc;
   for(wrc=width/4; wrc>=1; wrc /= 2) {
     ww = width/wrc;

     for( i=0; i<len-sep; i++ ){
       
       msw = 0;
       for( j=0; j<ww; j++) {
	 msw += b2[i+s2+j];
       }
       msw /= ww;
       
       if(msw >= level * ms_seq[i]/10) {
	 if( left == 0 ) 
	   left = i+s2;
       } else {
	 if(left != 0 && i-left+s2 <= ww*2) {
	   float lv = buffer[left];
	   float rv = buffer[i+ww+s2];
	   for(j=left; j<i+ww+s2; j++) {
	     buffer[j]= (rv*(j-left) + lv*(i+ww+s2-j))/(float)(i+ww+s2-left);
	     b2[j] = buffer[j]*buffer[j];
	   }
	   left=0;
	 } else if(left != 0) {
	   left = 0;
	 }
       }
     }
   }
   
   delete[] ms_seq;
   delete[] b2;
}
       


// WDR: class implementations

//----------------------------------------------------------------------------
// ClickRemovalDialog
//----------------------------------------------------------------------------

// WDR: event table for ClickRemovalDialog

enum {
	ID_BUTTON_PREVIEW=10002
};

BEGIN_EVENT_TABLE(ClickRemovalDialog,wxDialog)
	EVT_BUTTON(wxID_OK, ClickRemovalDialog::OnRemoveClicks)
	EVT_BUTTON(wxID_CANCEL, ClickRemovalDialog::OnCancel)
	EVT_BUTTON(ID_BUTTON_PREVIEW, ClickRemovalDialog::OnPreview)
END_EVENT_TABLE()

ClickRemovalDialog::ClickRemovalDialog(EffectClickRemoval * effect, 
													wxWindow *parent, wxWindowID id,
													const wxString &title,
													const wxPoint &position, 
													const wxSize& size,
													long style ) :
   wxDialog( parent, id, title, position, size, style )
{
	m_pEffect = effect;
   
	// NULL out the control members until the controls are created.
	m_pButton_GetProfile = NULL;
	m_pSlider = NULL;
	m_pButton_Preview = NULL;
	m_pButton_RemoveClicks = NULL;

   this->MakeClickRemovalDialog(true); 
}

// WDR: handler implementations for NoiseRemovalDialog

void ClickRemovalDialog::OnPreview(wxCommandEvent &event)
{
	// Save & restore parameters around Preview, because we didn't do OK.
	int oldLevel = m_pEffect->level;
	int oldWidth = m_pEffect->width;
	int oldSep = m_pEffect->sep;

	m_pEffect->level = m_pSlider->GetValue();
	m_pEffect->width = m_pSlider_width->GetValue();
	//	m_pEffect->sep = m_pSlider_sep->GetValue();
	
   
	m_pEffect->Preview();
   
	m_pEffect->sep   = oldSep;
	m_pEffect->width = oldWidth;
	m_pEffect->level = oldLevel; 
}

void ClickRemovalDialog::OnRemoveClicks( wxCommandEvent &event )
{
   EndModal(2);
}

void ClickRemovalDialog::OnCancel(wxCommandEvent &event)
{
   EndModal(0);
}

wxSizer *ClickRemovalDialog::MakeClickRemovalDialog(bool call_fit /* = true */, 
																	 bool set_sizer /* = true */)
{
   wxBoxSizer *mainSizer = new wxBoxSizer( wxVERTICAL );
   wxStaticBoxSizer *group;
   wxControl *item;
   
   item = new wxStaticText(this, -1,
                   _("Click and Pop Removal by Craig DeForest"), wxDefaultPosition,
                   wxDefaultSize, wxALIGN_CENTRE );
   mainSizer->Add(item, 0, wxALIGN_CENTRE|wxALL, 5);

   
   group = new wxStaticBoxSizer(new wxStaticBox(this, -1,
                                                _("Settings")), wxVERTICAL);

   item = new wxStaticText(this, -1,
                           _("\n \nSelect threshold (lower is more sensitive)"),
                           wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
   group->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   m_pSlider = new wxSlider(this, -1, 8, 1, 900,	wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL);
   group->Add(m_pSlider, 1, wxEXPAND|wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxTOP, 5 );
   item = new wxStaticText(this,-1,
			   _("\nMax spike width (higher is more sensitive)"),
			   wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT);
   group->Add(item,0,wxALIGN_CENTER|wxALL,5);

   m_pSlider_width = new wxSlider(this, -1, 8, 1, 40,	wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL);
   group->Add(m_pSlider_width, 1, wxEXPAND|wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxTOP, 5 );


   //   item = new wxStaticText(this,-1,
   //			   _("\nNeighborhood"),
   //			   wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT);
   //   group->Add(item,0,wxALIGN_CENTER|wxALL,5);
   //   m_pSlider_sep = new wxSlider(this,-1,8,1,4100,wxDefaultPosition,wxDefaultSize,wxSL_HORIZONTAL);
   //   group->Add(m_pSlider_sep, 1, wxEXPAND|wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxTOP, 5 );

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);
   item = new wxStaticText(this, -1, _("Less"));
   hSizer->Add(item, 0, wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxBOTTOM, 5 );   
   hSizer->Add(10, 10, 1, wxALIGN_CENTRE | wxLEFT | wxRIGHT | wxBOTTOM, 5);
   item = new wxStaticText(this, -1, _("More"));
   hSizer->Add(item, 0, wxALIGN_CENTRE|wxLEFT | wxRIGHT | wxBOTTOM, 5 );
   group->Add(hSizer, 1, wxEXPAND|wxALIGN_CENTRE|wxALL, 5 );
   
   hSizer = new wxBoxSizer(wxHORIZONTAL);

   m_pButton_Preview = new wxButton(this, ID_BUTTON_PREVIEW, m_pEffect->GetPreviewName());
   hSizer->Add(m_pButton_Preview, 0, wxALIGN_LEFT | wxALL, 5);
   
   hSizer->Add(25, 5); // horizontal spacer

	m_pButton_RemoveClicks = new wxButton(this, wxID_OK, _("Remove clicks"), wxDefaultPosition, wxDefaultSize, 0 );
   hSizer->Add(m_pButton_RemoveClicks, 0, wxALIGN_RIGHT | wxALL, 5 );
   
	group->Add(hSizer, 0, wxALIGN_CENTER | wxALL, 5 );

   mainSizer->Add( group, 0, wxALIGN_CENTRE|wxALL, 5 );
   

   item = new wxButton( this, wxID_CANCEL, _("Close"), wxDefaultPosition, wxDefaultSize, 0 );
   mainSizer->Add(item, 0, wxALIGN_CENTRE|wxALL, 5 );

   if (set_sizer) {
      this->SetAutoLayout( TRUE );
      this->SetSizer( mainSizer );
      if (call_fit) {
         mainSizer->Fit( this );
         mainSizer->SetSizeHints( this );
      }
   }
    
   return mainSizer;
}
