/**********************************************************************

  Audacity: A Digital Audio Editor

  Dominic Mazzoni

  This class abstracts the interface to two different resampling
  libraries:

    libresample, written by Dominic Mazzoni based on Resample-1.7
    by Julius Smith.  LGPL.

    libsamplerate, written by Erik de Castro Lopo.  GPL.  The author
    of libsamplerate requests that you not distribute a binary version
    of Audacity that links to libsamplerate and also has plug-in support.

**********************************************************************/

#include "Resample.h"
#include "Prefs.h"

#include <wx/intl.h>

#if USE_LIBRESAMPLE

#include "libresample.h"

bool Resample::ResamplingEnabled()
{
   return true;
}

wxString Resample::GetResamplingLibraryName()
{
   return _("Libresample by Dominic Mazzoni and Julius Smith");
}

int Resample::GetNumMethods()
{
   return 2;
}

wxString Resample::GetMethodName(int index)
{
   if (index == 1)
      return _("High-quality Sinc Interpolation");
   else
      return _("Fast Sinc Interpolation");
}

int Resample::GetFastMethod()
{
   return gPrefs->Read("/Quality/LibresampleSampleRateConverter",
                       (long)0);
}

int Resample::GetBestMethod()
{
   return gPrefs->Read("/Quality/LibresampleHQSampleRateConverter",
                       (long)1);
}

void Resample::SetFastMethod(int index)
{
   gPrefs->Write("/Quality/LibresampleSampleRateConverter",
                 (long)index);
}

void Resample::SetBestMethod(int index)
{
   gPrefs->Write("/Quality/LibresampleHQSampleRateConverter",
                 (long)index);   
}

Resample::Resample(bool useBestMethod, double minFactor, double maxFactor)
{
   if (useBestMethod)
      mMethod = GetBestMethod();
   else
      mMethod = GetFastMethod();

   mHandle = resample_open(mMethod, minFactor, maxFactor);
}

bool Resample::Ok()
{
   return (mHandle != NULL);
}

int Resample::Process(double  factor,
                      float  *inBuffer,
                      int     inBufferLen,
                      bool    lastFlag,
                      int    *inBufferUsed,
                      float  *outBuffer,
                      int     outBufferLen)
{
   return resample_process(mHandle, factor, inBuffer, inBufferLen,
									(int)lastFlag, inBufferUsed, outBuffer, outBufferLen);
}

Resample::~Resample()
{
   resample_close(mHandle);
}

#elif USE_LIBSAMPLERATE

#include <samplerate.h>

bool Resample::ResamplingEnabled()
{
   return true;
}

wxString Resample::GetResamplingLibraryName()
{
   return _("Libsamplerate by Erik de Castro Lopo");
}

int Resample::GetNumMethods()
{
   int i = 0;

   while(src_get_name(i))
      i++;

   return i;
}

wxString Resample::GetMethodName(int index)
{
   return wxString(src_get_name(index));
}

int Resample::GetFastMethod()
{
   return gPrefs->Read("/Quality/SampleRateConverter",
                       (long)SRC_SINC_FASTEST);
}

int Resample::GetBestMethod()
{
   return gPrefs->Read("/Quality/HQSampleRateConverter",
                       (long)SRC_SINC_FASTEST);
}

void Resample::SetFastMethod(int index)
{
   gPrefs->Write("/Quality/SampleRateConverter",
                 (long)index);
}

void Resample::SetBestMethod(int index)
{
   gPrefs->Write("/Quality/HQSampleRateConverter",
                 (long)index);   
}

Resample::Resample(bool useBestMethod, double minFactor, double maxFactor)
{
   if (minFactor < 1.0 / 12.0 || maxFactor > 12.0) {
      fprintf(stderr, "Libsamplerate only supports resampling factors\n");
      fprintf(stderr, "between 1/12 and 12\n");
      mHandle = NULL;
      return;
   }

   if (useBestMethod)
      mMethod = GetBestMethod();
   else
      mMethod = GetFastMethod();

   int err;
   SRC_STATE *state = src_new(mMethod, 1, &err);
   mHandle = (void *)state;
   mInitial = true;
}

bool Resample::Ok()
{
   return (mHandle != NULL);
}

int Resample::Process(double  factor,
                      float  *inBuffer,
                      int     inBufferLen,
                      bool    lastFlag,
                      int    *inBufferUsed,
                      float  *outBuffer,
                      int     outBufferLen)
{
   if (mInitial) {
      src_set_ratio((SRC_STATE *)mHandle, factor);
      mInitial = false;
   }

   SRC_DATA data;

   data.data_in = inBuffer;
   data.data_out = outBuffer;
   data.input_frames = inBufferLen;
   data.output_frames = outBufferLen;
   data.input_frames_used = 0;
   data.output_frames_gen = 0;
   data.end_of_input = (int)lastFlag;
   data.src_ratio = factor;

   int err = src_process((SRC_STATE *)mHandle, &data);
   if (err) {
      fprintf(stderr, _("Libsamplerate error: %d\n"), err);
      return 0;
   }

   *inBufferUsed = (int)data.input_frames_used;
   return (int)data.output_frames_gen;
}

Resample::~Resample()
{
   src_delete((SRC_STATE *)mHandle);
}

#else // No resampling support

bool Resample::ResamplingEnabled()
{
   return false;
}

wxString Resample::GetResamplingLibraryName()
{
   return _("Resampling disabled.");
}

int Resample::GetNumMethods()
{
   return 1;
}

wxString Resample::GetMethodName(int index)
{
   return _("Resampling disabled.");
}

int Resample::GetFastMethod()
{
   return 0;
}

int Resample::GetBestMethod()
{
   return 0;
}

void Resample::SetFastMethod(int)
{
}

void Resample::SetBestMethod(int)
{
}

Resample::Resample(bool, double, double)
{
}

bool Resample::Ok()
{
   return false;
}

int Resample::Process(double  factor,
                      float  *inBuffer,
                      int     inBufferLen,
                      bool    lastFlag,
                      int    *inBufferUsed,
                      float  *outBuffer,
                      int     outBufferLen)
{
   int i;
   int len = inBufferLen;

   if (len > outBufferLen)
      len = outBufferLen;

   for(i=0; i<len; i++)
      outBuffer[i] = inBuffer[i];

   return len;
}

Resample::~Resample()
{
}

#endif





