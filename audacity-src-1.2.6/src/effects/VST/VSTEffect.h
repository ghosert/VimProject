/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/

#include "../../Audacity.h"

#if USE_VST

#include "AudioEffect.hpp"      // VST API

class wxSlider;
class labels;

#include <wx/dialog.h>
#include <wx/stattext.h>

#include "../Effect.h"

typedef long (*dispatcherFn)(AEffect * effect, long opCode,
                             long index, long value, void *ptr,
                             float opt);

typedef void (*processFn)(AEffect * effect, float **inputs,
                          float **outputs, long sampleframes);

typedef void (*setParameterFn)(AEffect * effect, long index,
                               float parameter);

typedef float (*getParameterFn)(AEffect * effect, long index);

class VSTEffect:public Effect {

 public:

   VSTEffect(wxString pluginName, AEffect * aEffect);

   virtual ~VSTEffect();

   virtual wxString GetEffectName();
   
   virtual wxString GetEffectAction();
   
   virtual int GetEffectFlags() {
      return PLUGIN_EFFECT | PROCESS_EFFECT;
   }

   virtual bool Init();

   virtual bool PromptUser();
   
   virtual bool Process();
   
   virtual void End();

   // VST methods

   long callDispatcher(AEffect * effect, long opCode,
                       long index, long value, void *ptr,
                       float opt);
   void callProcess(AEffect * effect, float **inputs,
                    float **outputs, long sampleframes);
   void callProcessReplacing(AEffect * effect, float **inputs,
                             float **outputs, long sampleframes);
   void callSetParameter(AEffect * effect, long index,
                         float parameter);
   float callGetParameter(AEffect * effect, long index);

 private:
   bool ProcessStereo(int count, WaveTrack * left, WaveTrack *right,
                      longSampleCount lstart,
                      longSampleCount rstart, sampleCount len);

   void GetSamples(WaveTrack *track,
                   longSampleCount *start,
                   sampleCount *len);
      
   bool isOpened;
   wxString pluginName;
   AEffect *aEffect;

   sampleCount mBlockSize;
   float *buffer;
   float **fInBuffer;
   float **fOutBuffer;
   int inputs;
   int outputs;
   int numParameters;
};

class VSTEffectDialog:public wxDialog {
 public:
   VSTEffectDialog(wxWindow * parent,
                   wxString effectName,
                   int numParams,
                   VSTEffect * vst,
                   AEffect * aEffect,
                   const wxPoint & pos = wxDefaultPosition);

   ~VSTEffectDialog();

   void OnSlider(wxScrollEvent & event);
   void OnSliderCmd(wxCommandEvent & event);
   void OnOK(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);
   void OnPreview(wxCommandEvent & event);

   DECLARE_EVENT_TABLE()

 private:
   VSTEffect * vst;
   AEffect * aEffect;
   wxSlider **sliders;
   wxStaticText **labels;
   int numParams;
};

#if defined(__WXMAC__) && defined(__UNIX__)

void *NewMachOFromCFM(void *cfmfp);
void DisposeMachOFromCFM(void *ptr);
void *NewCFMFromMachO(void *machofp);
void DisposeCFMFromMachO(void *ptr);

#endif

#endif // USE_VST

