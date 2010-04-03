/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST__
#define __AUDACITY_EFFECT_NYQUIST__

#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/filename.h>
#include <wx/datetime.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/intl.h>
#include "../Effect.h"

#include "nyx.h"

class WaveTrack;

class NyqControl {
 public:
   wxString var;
   wxString name;
   int type;
   wxString label;
   wxString valStr;
   wxString lowStr;
   wxString highStr;
   double val;
   double low;
   double high;
   int ticks;
};

WX_DECLARE_OBJARRAY(NyqControl, NyqControlArray);

class EffectNyquist: public Effect {

public:
   
   EffectNyquist(wxString fname);
   virtual ~EffectNyquist();

   bool SetXlispPath();

   bool LoadedNyFile() {
      return mOK;
   }

   virtual wxString GetEffectName() {
      return mName;
   }
   
   virtual wxString GetEffectAction() {
      return mAction;
   }
   
   virtual int GetEffectFlags() {
      return mFlags;
   }

   virtual bool PromptUser();
   
   virtual bool Process();
   
private:
   
   int GetCallback(float *buffer, int channel,
                   long start, long len);
   int PutCallback(float *buffer, int channel,
                   long start, long len);

   static int StaticGetCallback(float *buffer, int channel,
                                long start, long len,
                                void *userdata);
   static int StaticPutCallback(float *buffer, int channel,
                                long start, long len,
                                void *userdata);

   bool       ProcessOne();

   void       Parse(wxString line);
   void       ParseFile();
   wxString   UnQuote(wxString s);
   double     GetCtrlValue(wxString s);

   static wxString  mXlispPath;

   wxFileName       mFileName;
   wxDateTime       mFileModified;

   bool             mInteractive;
   bool             mOK;
   wxString         mCmd;
   wxString         mName;
   wxString         mAction;
   wxString         mInfo;
   int              mFlags;
   bool             mDebug;
   wxString         mDebugOutput;

   NyqControlArray  mControls;

   int              mCurNumChannels;
   WaveTrack       *mCurTrack[2];
   longSampleCount  mCurStart[2];
   sampleCount      mCurLen;
   double           mOutputTime;
   int              mCount;
   double           mProgress;

   samplePtr        mCurBuffer[2];
   longSampleCount  mCurBufferStart[2];
   sampleCount      mCurBufferLen[2];

   WaveTrack       *mOutputTrack[2];
   
};

class NyquistDialog:public wxDialog {
 public:
   // constructors and destructors
   NyquistDialog(wxWindow * parent, wxWindowID id,
                 const wxString & title,
                 wxString info,
                 NyqControlArray *controlArray);

 private:
   NyqControlArray  *mControls;
   bool              mInHandler;

   void OnText(wxCommandEvent & event);
   void OnSlider(wxCommandEvent & event);
   void OnOk(wxCommandEvent & event);
   void OnDebug(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()

};

class NyquistInputDialog:public wxDialog {
 public:
   NyquistInputDialog(wxWindow * parent, wxWindowID id,
                      const wxString & title,
                      const wxString & prompt,
                      wxString initialCommand);

   wxString GetCommand();

 private:
   wxTextCtrl *mCommandText;

   void OnOk(wxCommandEvent & event);
   void OnDebug(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()
};

class NyquistOutputDialog:public wxDialog {
 public:
   NyquistOutputDialog(wxWindow * parent, wxWindowID id,
                       const wxString & title,
                       const wxString & prompt,
                       wxString message);

 private:
   void OnOk(wxCommandEvent & event);

 private:
   DECLARE_EVENT_TABLE()
};


#endif

