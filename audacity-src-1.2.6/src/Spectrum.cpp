/**********************************************************************

  Audacity: A Digital Audio Editor

  Spectrum.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include "Spectrum.h"
#include "FFT.h"

bool ComputeSpectrum(float * data, int width, int height,
                     int maxFreq, int windowSize,
                     double rate, float *grayscaleOut,
                     bool autocorrelation)
{
   int windowFunc = 3;

   if (width < windowSize)
      return false;

   if (!data || !grayscaleOut)
      return true;

   float *processed = new float[windowSize];

   int i;
   for (i = 0; i < windowSize; i++)
      processed[i] = float(0.0);
   int half = windowSize / 2;

   float *in = new float[windowSize];
   float *out = new float[windowSize];
   float *out2 = new float[windowSize];

   int start = 0;
   int windows = 0;
   while (start + windowSize <= width) {
      for (i = 0; i < windowSize; i++)
         in[i] = data[start + i];

      WindowFunc(windowFunc, windowSize, in);

      if (autocorrelation) {
         // Take FFT
         FFT(windowSize, false, in, NULL, out, out2);

         // Compute power
         for (i = 0; i < windowSize; i++)
            in[i] = (out[i] * out[i]) + (out2[i] * out2[i]);

         // Tolonen and Karjalainen recommend taking the cube root
         // of the power, instead of the square root

         for (i = 0; i < windowSize; i++)
            in[i] = pow(in[i], 1.0f / 3.0f);

         // Take FFT
         FFT(windowSize, false, in, NULL, out, out2);

         // Take real part of result
         for (i = 0; i < half; i++)
            processed[i] += out[i];
      } else {
         PowerSpectrum(windowSize, in, out);

         for (i = 0; i < half; i++)
            processed[i] += out[i];
      }

      start += half;
      windows++;
   }

   int maxSamples = int (maxFreq * windowSize / rate + 0.5);
   if (maxSamples > half)
      maxSamples = half;

   if (autocorrelation) {
      maxSamples = half;

      // Peak Pruning as described by Tolonen and Karjalainen, 2000

      // Clip at zero, copy to temp array
      for (i = 0; i < maxSamples; i++) {
         if (processed[i] < 0.0)
            processed[i] = float(0.0);
         out[i] = processed[i];
      }

      // Subtract a time-doubled signal (linearly interp.) from the original
      // (clipped) signal
      for (i = 0; i < maxSamples; i++)
         if ((i % 2) == 0)
            processed[i] -= out[i / 2];
         else
            processed[i] -= ((out[i / 2] + out[i / 2 + 1]) / 2);

      // Clip at zero again
      for (i = 0; i < maxSamples; i++)
         if (processed[i] < 0.0)
            processed[i] = float(0.0);

      // Find new max
      float max = 0;
      for (i = 1; i < maxSamples; i++)
         if (processed[i] > max)
            max = processed[i];

      // Reverse and scale
      for (i = 0; i < maxSamples; i++)
         in[i] = processed[i] / (windowSize / 4);
      for (i = 0; i < maxSamples; i++)
         processed[maxSamples - 1 - i] = in[i];
   } else {
      // Convert to decibels
      for (i = 0; i < maxSamples; i++)
         processed[i] = 10 * log10(processed[i] / windowSize / windows);
   }

   // Finally, put it into bins in grayscaleOut[], normalized to a 0.0-1.0 scale

   for (i = 0; i < height; i++) {
      float bin0 = float (i) * maxSamples / height;
      float bin1 = float (i + 1) * maxSamples / height;

      float binwidth = bin1 - bin0;

      float value = float(0.0);

      if (int (bin1) == int (bin0))
         value = processed[int (bin0)];
      else {
         value += processed[int (bin0)] * (int (bin0) + 1 - bin0);
         bin0 = 1 + int (bin0);
         while (bin0 < int (bin1)) {
            value += processed[int (bin0)];
            bin0 += 1.0;
         }
         value += processed[int (bin1)] * (bin1 - int (bin1));

         value /= binwidth;
      }

      if (!autocorrelation) {
         // Last step converts dB to a 0.0-1.0 range     
         value = (value + 80.0) / 80.0;
      }

      if (value > 1.0)
         value = float(1.0);
      if (value < 0.0)
         value = float(0.0);

      grayscaleOut[i] = value;
   }

   delete[]in;
   delete[]out;
   delete[]out2;
   delete[]processed;

   return true;
}
