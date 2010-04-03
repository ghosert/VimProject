/**********************************************************************

  Audacity: A Digital Audio Editor

  SampleFormat.h

  Dominic Mazzoni

  This file handles converting between all of the different
  sample formats that Audacity supports, such as 16-bit,
  24-bit (packed into a 32-bit int), and 32-bit float.

  Floating-point samples use the range -1.0...1.0, inclusive.
  Integer formats use the full signed range of their data type,
  for example 16-bit samples use the range -32768...32767.
  This presents a problem, because either the conversion to
  or from a float would have to be asymmetric, or zero would
  have to be mapped to something other than zero.  (A third
  option is to use a symmetric, zero-preserving mapping that
  might clip, and check for clipping...but this option is
  both uncompelling and slow.)

  Audacity chooses to use a symmetric mapping that doesn't
  preserve 0:

  16-bit    float        16-bit
  -32768 -> -1.000000 -> -32768

       0 ->  0.000015 ->      0
             0.000000 ->      0

   32767 ->  1.000000 ->  32767

   Note that 0.0 (float) still maps to 0 (int), so it is nearly
   an ideal mapping.  An analogous mapping is used between
   24-bit ints and floats.

   Note: These things are now handled by the Dither class, which
         also replaces the CopySamples() method (msmeyer)

**********************************************************************/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SampleFormat.h"
#include "Prefs.h"
#include "Dither.h"

Dither::DitherType gLowQualityDither = Dither::none;
Dither::DitherType gHighQualityDither = Dither::none;
Dither gDitherAlgorithm;

void InitDitherers()
{
   // Read dither preferences
   // Note: We use 'triangle' dithering for now, because
   //       the 'shaped' dithering is supposed to be broken.
   gLowQualityDither = (Dither::DitherType)
   gPrefs->Read("/Quality/DitherAlgorithm", (long)Dither::none);

   gHighQualityDither = (Dither::DitherType)
   gPrefs->Read("/Quality/HQDitherAlgorithm", (long)Dither::triangle);
}

const char *GetSampleFormatStr(sampleFormat format)
{
   switch(format) {
   case int16Sample:
      return "16-bit";
   case int24Sample:
      return "24-bit";
   case floatSample:
      return "32-bit float";
   }
   return ""; // compiler food
}

samplePtr NewSamples(int count, sampleFormat format)
{
   return (samplePtr)malloc(count * SAMPLE_SIZE(format));
}

void DeleteSamples(samplePtr p)
{
   free(p);
}

void ClearSamples(samplePtr src, sampleFormat format,
                  int start, int len)
{
   int size = SAMPLE_SIZE(format);
   memset(src + start*size, 0, len*size);
}

void CopySamples(samplePtr src, sampleFormat srcFormat,
                 samplePtr dst, sampleFormat dstFormat,
                 unsigned int len,
                 bool highQuality, /* = true */
                 unsigned int stride /* = 1 */)
{
   gDitherAlgorithm.Apply(
      highQuality ? gHighQualityDither : gLowQualityDither,
      src, srcFormat, dst, dstFormat, len, stride);
}

void CopySamplesNoDither(samplePtr src, sampleFormat srcFormat,
                 samplePtr dst, sampleFormat dstFormat,
                 unsigned int len,
                 unsigned int stride /* = 1 */)
{
   gDitherAlgorithm.Apply(
      Dither::none,
      src, srcFormat, dst, dstFormat, len, stride);
}
