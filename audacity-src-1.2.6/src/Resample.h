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

  Since Audacity always does resampling on mono streams that are
  contiguous in memory, this class doesn't support multiple channels
  or some of the other optional features of some of these resamplers.

**********************************************************************/

#ifndef __AUDACITY_RESAMPLE_H__
#define __AUDACITY_RESAMPLE_H__

#include "Audacity.h"

#include <wx/string.h>

#include "SampleFormat.h"

class Resample
{
 public:

   /// This will return true if Audacity is being compiled with
   /// resampling support.
   static bool ResamplingEnabled();

   /// Returns the name of the library used for resampling
   /// (long format, may include author name and version number).
   static wxString GetResamplingLibraryName();

   /// Resamplers may have more than one method, offering a
   /// tradeoff between speed and quality.  This lets you query
   /// the various methods available.
   static int GetNumMethods();
   static wxString GetMethodName(int index);

   /// Audacity identifies two methods out of all of the choices:
   /// a Fast method intended for real-time audio I/O, and a Best
   /// method intended for mixing and exporting.  These are saved
   /// in the preferences when you call Set[Best,Fast]Method.
   static int GetFastMethod();
   static int GetBestMethod();
   static void SetFastMethod(int index);
   static void SetBestMethod(int index);

   /// Constructor.
   /// The first parameter lets you select either the best method or
   /// the fast method - the particular method used was set by
   /// SetFastMethod or SetBestMethod, above.  minFactor and maxFactor
   /// specify the range of factors that will be used, if you plan
   /// to vary the factor over time.  Otherwise set minFactor and
   /// maxFactor to the same value for optimized performance.
   Resample(bool useBestMethod, double minFactor, double maxFactor);

   /// Returns true if the constructor succeeded.
   bool Ok();

   /// Main processing function.  Reads up to inBufferLen samples
   /// from the input buffer, and writes up to outBufferLen samples
   /// to the output buffer.  Pass true in lastFlag if this is the
   /// end of the input; otherwise not all of the input samples will
   /// be used.  The number of input samples used is returned in
   /// inBufferUsed, and the number of output samples generated
   /// is the return value of the function.  This function may do
   /// nothing if you don't pass a large enough output buffer.
   int Process(double  factor,
               float  *inBuffer,
               int     inBufferLen,
               bool    lastFlag,
               int    *inBufferUsed,
               float  *outBuffer,
               int     outBufferLen);

   // Destructor
   ~Resample();

 private:
   int   mMethod;
   void *mHandle;
   bool  mInitial;
};

#endif // __AUDACITY_RESAMPLE_H__
