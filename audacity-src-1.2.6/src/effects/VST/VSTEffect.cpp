/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.cpp

  Dominic Mazzoni
  
  This class implements a VST Plug-in effect.  The plug-in must be
  loaded in a platform-specific way and passed into the constructor,
  but from here this class handles the interfacing.  VST plug-ins
  are used in Cubase and other Steinberg products, and all of those
  files and the information within is copyrighted by Steinberg.

**********************************************************************/

#include "../../Audacity.h"

#if USE_VST

#include "AEffect.h"            // VST API
#include "AEffEditor.hpp"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/frame.h>
#include <wx/slider.h>
#include <wx/sizer.h>
#include <wx/msgdlg.h>

#include "../Effect.h"          // Audacity Effect base class
#include "VSTEffect.h"          // This class's header file

#ifdef __MACOSX__
#include <Carbon/Carbon.h>
#endif

const int VSTEFFECT_SLIDER_ID = 13100;
const int PREVIEW_ID          = 13101;

class VSTEffectGUIDialog : public wxDialog
{
public:
   VSTEffectGUIDialog(wxWindow * parent, wxWindowID id,
                      const wxString & title,
                      VSTEffect *effect, AEffect *aEffect);

   void OnOk(wxCommandEvent &event);
   void OnCancel(wxCommandEvent &event);
   void OnPreview(wxCommandEvent &event);

   void OnIdle(wxIdleEvent &event);

private:
   VSTEffect *mEffect;
   AEffect   *mAEffect;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(VSTEffectGUIDialog,wxDialog)
   EVT_BUTTON( wxID_OK, VSTEffectGUIDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, VSTEffectGUIDialog::OnCancel )
   EVT_BUTTON( PREVIEW_ID, VSTEffectGUIDialog::OnPreview )
   EVT_IDLE( VSTEffectGUIDialog::OnIdle )
END_EVENT_TABLE()

VSTEffectGUIDialog::VSTEffectGUIDialog(wxWindow *parent, wxWindowID id,
                                       const wxString & title,
                                       VSTEffect *effect,
                                       AEffect *aEffect):
   wxDialog(parent, id, title, wxDefaultPosition, wxDefaultSize,
            wxRESIZE_BORDER | wxSYSTEM_MENU | wxCAPTION),
   mEffect(effect),
   mAEffect(aEffect)
{
   ERect *rect;

   mEffect->callDispatcher(mAEffect, effEditGetRect, 0, 0, &rect, 0.0);
   mEffect->callDispatcher(mAEffect, effEditOpen, 0, 0, (void *)GetHWND(), 0.0);

   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   mainSizer->Add(rect->right-rect->left, rect->bottom-rect->top);

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *preview =
      new wxButton(this, PREVIEW_ID, mEffect->GetPreviewName());
   hSizer->Add(preview, 0, wxALIGN_CENTRE|wxALL, 5);

   hSizer->Add(20, 10); // horizontal spacer

   wxButton *cancel = new wxButton(this, wxID_CANCEL, _("Cancel"));
   hSizer->Add(cancel, 0, wxALIGN_CENTRE|wxALL, 5);

   wxButton *ok = new wxButton(this, wxID_OK, _("OK"));
   ok->SetDefault();
   ok->SetFocus();
   hSizer->Add(ok, 0, wxALIGN_CENTRE|wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

void VSTEffectGUIDialog::OnIdle(wxIdleEvent &event)
{
   mEffect->callDispatcher(mAEffect, effEditIdle, 0, 0, NULL, 0.0);
}

void VSTEffectGUIDialog::OnPreview(wxCommandEvent &event)
{
   mEffect->Preview();
}

void VSTEffectGUIDialog::OnOk(wxCommandEvent &event)
{
   mEffect->callDispatcher(mAEffect, effEditClose, 0, 0, NULL, 0.0);
   EndModal(true);
}

void VSTEffectGUIDialog::OnCancel(wxCommandEvent &event)
{
   mEffect->callDispatcher(mAEffect, effEditClose, 0, 0, NULL, 0.0);
   EndModal(false);
}


VSTEffect::VSTEffect(wxString pluginName, AEffect * aEffect)
{
   this->aEffect = aEffect;
   this->pluginName = pluginName;

   buffer = NULL;
   fInBuffer = NULL;
   fOutBuffer = NULL;

   isOpened = false;
}

VSTEffect::~VSTEffect()
{
}

wxString VSTEffect::GetEffectName()
{
   return pluginName + "...";
}

wxString VSTEffect::GetEffectAction()
{
   return "Performing VST Effect: \""+pluginName+"\"";
}

bool VSTEffect::Init()
{
   inputs = aEffect->numInputs;
   outputs = aEffect->numOutputs;

   mBlockSize = 0;

   if (inputs > 1) {
      TrackListIterator iter(mWaveTracks);
      Track *left = iter.First();
      while(left) {
         longSampleCount lstart, rstart;
         sampleCount llen, rlen;
         GetSamples((WaveTrack *)left, &lstart, &llen);
         
         if (left->GetLinked()) {
            Track *right = iter.Next();
            GetSamples((WaveTrack *)right, &rstart, &rlen);
            
            if (llen != rlen || ((WaveTrack *)left)->GetRate() != ((WaveTrack *)right)->GetRate()) {
               wxMessageBox(_("Sorry, VST Effects cannot be performed on stereo tracks where "
                            "the individual channels of the track do not match."));
               return false;
            }
         }
         
         left = iter.Next();
      }
   }

   return true;
}

bool VSTEffect::PromptUser()
{
   if (!(aEffect->flags & effFlagsCanReplacing)) {
      wxMessageBox(_("Can't process replacing"));
      return false;
   }

   if (aEffect->flags & effFlagsHasEditor) {
      // Show native GUI
      VSTEffectGUIDialog dlog(mParent, -1, pluginName, this, aEffect);
      dlog.CentreOnParent();
      dlog.ShowModal();

      return dlog.GetReturnCode();
   }

   // Try to figure out how many parameters it takes by seeing how
   // many parameters have names
   char temp[8][256];
   numParameters = 0;
   do {
      long result;

      temp[numParameters][0] = 0;
      result = callDispatcher(aEffect, effGetParamName, numParameters, 0,
                              (void *) temp[numParameters], 0.0);

      if (temp[numParameters][0]==0)
         break;
      if (strstr(temp[numParameters], "ABOUT"))
         break;
      if (numParameters > 0
          && !strcmp(temp[numParameters], temp[numParameters - 1]))
         break;

      numParameters++;
   } while (temp[0] != 0 && numParameters < 8);

   if (numParameters > 0) {
      VSTEffectDialog dlog(mParent, pluginName, numParameters, this, aEffect);
      dlog.CentreOnParent();
      dlog.ShowModal();

      if (!dlog.GetReturnCode())
         return false;
   }

   return true;
}

void VSTEffect::GetSamples(WaveTrack *track,
                           longSampleCount *start,
                           sampleCount *len)
{
   double trackStart = track->GetStartTime();
   double trackEnd = track->GetEndTime();
   double t0 = mT0 < trackStart? trackStart: mT0;
   double t1 = mT1 > trackEnd? trackEnd: mT1;
   
   if (t1 > t0) {
      *start = track->TimeToLongSamples(t0);
      longSampleCount end = track->TimeToLongSamples(t1);
      *len = (sampleCount)(end - *start);
   }
   else {
      *start = 0;
      *len  = 0;
   }
}

bool VSTEffect::Process()
{
   TrackListIterator iter(mWaveTracks);
   int count = 0;
   Track *left = iter.First();
   Track *right;
   while(left) {
      longSampleCount lstart, rstart;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);

      right = NULL;
      if (left->GetLinked() && inputs>1) {
         right = iter.Next();         
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      // Reset the effect
      callDispatcher(aEffect, effOpen, 0, 0, NULL, 0.0);
      
      bool success = ProcessStereo(count,
                                   (WaveTrack *)left, (WaveTrack *)right,
                                   lstart, rstart, len);

      if (!success)
         return false;
   
      left = iter.Next();
      count++;
   }
   
   return true;
}

bool VSTEffect::ProcessStereo(int count, WaveTrack *left, WaveTrack *right,
                              longSampleCount lstart,
                              longSampleCount rstart, sampleCount len)
{
   if (mBlockSize == 0) {
      mBlockSize = left->GetMaxBlockSize() * 2;

      buffer = new float[mBlockSize];
      fInBuffer = new float *[inputs];
      int i;
      for (i = 0; i < inputs; i++)
         fInBuffer[i] = new float[mBlockSize];
      fOutBuffer = new float *[outputs];
      for (i = 0; i < outputs; i++)
         fOutBuffer[i] = new float[mBlockSize];

   }

   callDispatcher(aEffect, effSetSampleRate, 0, 0, NULL,
                       (float) left->GetRate());
   callDispatcher(aEffect, effSetBlockSize, 0, mBlockSize * 2, NULL, 0.0);

   // Actually perform the effect here

   sampleCount originalLen = len;
   longSampleCount ls = lstart;
   longSampleCount rs = rstart;
   while (len) {
      int i;
      int block = mBlockSize;
      if (block > len)
         block = len;

      left->Get((samplePtr)buffer, floatSample, ls, block);
      for (i = 0; i < block; i++)
         fInBuffer[0][i] = buffer[i];
      if (right) {
         right->Get((samplePtr)buffer, floatSample, rs, block);
         for (i = 0; i < block; i++)
            fInBuffer[1][i] = buffer[i];
      }

      callProcessReplacing(aEffect, fInBuffer, fOutBuffer, block);

      for (i = 0; i < block; i++)
         buffer[i] = fOutBuffer[0][i];
      left->Set((samplePtr)buffer, floatSample, ls, block);
      
      if (right) {
         for (i = 0; i < block; i++)
            buffer[i] = fOutBuffer[1][i];
         right->Set((samplePtr)buffer, floatSample, rs, block);
      }      

      len -= block;
      ls += block;
      rs += block;
      
      if (inputs > 1) {      
         if (TrackGroupProgress(count, (ls-lstart)/(double)originalLen))
            break;
      }
      else {
         if (TrackProgress(count, (ls-lstart)/(double)originalLen))
            break;
      }
   }

   return true;
}

void VSTEffect::End()
{
   if (buffer) {
      int i;

      delete[]buffer;
      for (i = 0; i < inputs; i++)
         delete fInBuffer[i];
      for (i = 0; i < outputs; i++)
         delete fOutBuffer[i];
      delete[]fInBuffer;
      delete[]fOutBuffer;

   }
   buffer = NULL;
   fInBuffer = NULL;
   fOutBuffer = NULL;
}

BEGIN_EVENT_TABLE(VSTEffectDialog, wxDialog)
    EVT_BUTTON(wxID_OK, VSTEffectDialog::OnOK)
    EVT_BUTTON(wxID_CANCEL, VSTEffectDialog::OnCancel)
    EVT_BUTTON(PREVIEW_ID, VSTEffectDialog::OnPreview)
    EVT_COMMAND_SCROLL(VSTEFFECT_SLIDER_ID, VSTEffectDialog::OnSlider)
    EVT_SLIDER(VSTEFFECT_SLIDER_ID, VSTEffectDialog::OnSliderCmd)
END_EVENT_TABLE()

VSTEffectDialog::VSTEffectDialog(wxWindow * parent,
                                 wxString effectName,
                                 int numParams,
                                 VSTEffect * vst,
                                 AEffect * aEffect,
                                 const wxPoint & pos)
:wxDialog(parent, -1, effectName, pos, wxSize(320, 430),
          wxDEFAULT_DIALOG_STYLE)
{
   this->vst = vst;
   this->aEffect = aEffect;
   this->numParams = numParams;

   int y = 10;

   new wxStaticText(this, 0, _("VST Plug-in parameters:"), wxPoint(10, y),
                    wxSize(300, 15));
   y += 20;

   sliders = new wxSlider *[numParams];
   labels = new wxStaticText *[numParams];

   for (int p = 0; p < numParams; p++) {

      char paramName[256];
      vst->callDispatcher(aEffect, effGetParamName, p, 0,
                     (void *) paramName, 0.0);
      new wxStaticText(this, 0, wxString(paramName), wxPoint(10, y),
                       wxSize(85, 15));

      float val = vst->callGetParameter(aEffect, p);

      sliders[p] =
          new wxSlider(this, VSTEFFECT_SLIDER_ID,
                       1000 * val, 0, 1000,
                       wxPoint(100, y + 5), wxSize(200, 25));

      char label[256];
      vst->callDispatcher(aEffect, effGetParamDisplay, p, 0,
                          (void *) label, 0.0);
      char units[256];
      vst->callDispatcher(aEffect, effGetParamLabel, p, 0, (void *) units,
                          0.0);

      labels[p] =
          new wxStaticText(this, 0,
                           wxString::Format("%s %s", label, units),
                           wxPoint(10, y + 15), wxSize(85, 15));

      y += 35;
   }

   y += 20;
   wxButton *preview =
      new wxButton(this, PREVIEW_ID,
                   vst->GetPreviewName(),
                   wxPoint(10, y), wxSize(80, 30));
   wxButton *cancel =
       new wxButton(this, wxID_CANCEL, _("Cancel"), wxPoint(130, y), wxSize(80, 30));
   wxButton *ok =
       new wxButton(this, wxID_OK, _("OK"), wxPoint(220, y), wxSize(80, 30));

   y += 40;

   wxSize size;
   size.x = 320;
   size.y = y;

#ifdef __WXMSW__
   size.y += 20;
#endif

   Centre(wxBOTH | wxCENTER_FRAME);

   SetSize(size);
}

VSTEffectDialog::~VSTEffectDialog()
{
   // TODO: proper disposal here

   delete[]sliders;
   delete[]labels;
}

void VSTEffectDialog::OnSlider(wxScrollEvent & WXUNUSED(event))
{
   for (int p = 0; p < numParams; p++) {
      float val;

      val = sliders[p]->GetValue() / 1000.;
      vst->callSetParameter(aEffect, p, val);

      char label[256];
      vst->callDispatcher(aEffect, effGetParamDisplay, p, 0,
                          (void *) label, 0.0);
      char units[256];
      vst->callDispatcher(aEffect, effGetParamLabel, p, 0, (void *) units,
                          0.0);
      labels[p]->SetLabel(wxString::Format("%s %s", label, units));
   }
}

void VSTEffectDialog::OnSliderCmd(wxCommandEvent & WXUNUSED(event))
{
   for (int p = 0; p < numParams; p++) {
      float val;

      val = sliders[p]->GetValue() / 1000.;
      vst->callSetParameter(aEffect, p, val);

      char label[256];
      vst->callDispatcher(aEffect, effGetParamDisplay, p, 0,
                          (void *) label, 0.0);
      char units[256];
      vst->callDispatcher(aEffect, effGetParamLabel, p, 0, (void *) units,
                          0.0);
      labels[p]->SetLabel(wxString::Format("%s %s", label, units));
   }
}

void VSTEffectDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(TRUE);
}

void VSTEffectDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(FALSE);
}

void VSTEffectDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   vst->Preview();
}

#ifdef __MACOSX__

// Mac OS X methods
//
// Cross-platform VST plug-ins on the Mac are compiled as Carbon,
// CFM code.  Audacity is compiled as Carbon, Mach-O code.  Special care
// must be taken when calling funtions in the other mode - these
// functions make it easier.
//

// MachOFunctionPointerForCFMFunctionPointer(void *cfmfp)
//
// Borrowed from the Apple Sample Code file "CFM_MachO_CFM.c"
// This function allocates a block of CFM glue code which contains
// the instructions to call CFM routines

void *NewMachOFromCFM(void *cfmfp)
{
   UInt32 CFMTemplate[6] = {0x3D800000, 0x618C0000, 0x800C0000,
                            0x804C0004, 0x7C0903A6, 0x4E800420};
   UInt32	*mfp = (UInt32*)NewPtr(sizeof(CFMTemplate));
   
   mfp[0] = CFMTemplate[0] | ((UInt32)cfmfp >> 16);
   mfp[1] = CFMTemplate[1] | ((UInt32)cfmfp & 0xFFFF);
   mfp[2] = CFMTemplate[2];
   mfp[3] = CFMTemplate[3];
   mfp[4] = CFMTemplate[4];
   mfp[5] = CFMTemplate[5];
   MakeDataExecutable(mfp, sizeof(CFMTemplate));
   
   return(mfp);
}

void DisposeMachOFromCFM(void *ptr)
{
   DisposePtr((Ptr)ptr);
}

void *NewCFMFromMachO(void *machofp)
{
   void *result = NewPtr(8);
   ((void **)result)[0] = machofp;
   ((void **)result)[1] = result;

   return result;
}

void DisposeCFMFromMachO(void *ptr)
{
   DisposePtr((Ptr)ptr);
}

long VSTEffect::callDispatcher(AEffect * effect, long opCode,
                               long index, long value, void *ptr,
                               float opt)
{
   long rval;

   dispatcherFn fp = (dispatcherFn)NewMachOFromCFM(effect->dispatcher);
   rval = fp(effect, opCode, index, value, ptr, opt);
   DisposeMachOFromCFM(fp);

   return rval;
}

void VSTEffect::callProcess(AEffect * effect, float **inputs,
                            float **outputs, long sampleframes)
{
   processFn fp = (processFn)NewMachOFromCFM(effect->process);
   fp(effect, inputs, outputs, sampleframes);
   DisposeMachOFromCFM(fp);
}

void VSTEffect::callProcessReplacing(AEffect * effect, float **inputs,
                                     float **outputs, long sampleframes)
{
   processFn fp = (processFn)NewMachOFromCFM(effect->processReplacing);
   fp(effect, inputs, outputs, sampleframes);
   DisposeMachOFromCFM(fp);
}

void VSTEffect::callSetParameter(AEffect * effect, long index,
                                 float parameter)
{
   setParameterFn fp = (setParameterFn)NewMachOFromCFM(effect->setParameter);
   fp(effect, index, parameter);
   DisposeMachOFromCFM(fp);
}

float VSTEffect::callGetParameter(AEffect * effect, long index)
{
   float rval;

   getParameterFn fp = (getParameterFn)NewMachOFromCFM(effect->getParameter);
   rval = fp(effect, index);
   DisposeMachOFromCFM(fp);

   return rval;
}

#else // ifdef __MACOSX__

long VSTEffect::callDispatcher(AEffect * effect, long opCode,
                               long index, long value, void *ptr,
                               float opt)
{
   return effect->dispatcher(effect, opCode, index, value, ptr, opt);
}

void VSTEffect::callProcess(AEffect * effect, float **inputs,
                            float **outputs, long sampleframes)
{
   effect->process(effect, inputs, outputs, sampleframes);
}

void VSTEffect::callProcessReplacing(AEffect * effect, float **inputs,
                                     float **outputs, long sampleframes)
{
   effect->processReplacing(effect, inputs, outputs, sampleframes);
}

void VSTEffect::callSetParameter(AEffect * effect, long index,
                                 float parameter)
{
   effect->setParameter(effect, index, parameter);
}

float VSTEffect::callGetParameter(AEffect * effect, long index)
{
   return effect->getParameter(effect, index);
}

#endif // MACOSX

#endif // USE_VST
