/**********************************************************************

  Audacity: A Digital Audio Editor

  Landmark.cpp

  Dominic Mazzoni

  Pitch detector based on "landmark" algorithm by Cooper and Ng,
  University of Leeds SCS, Division of AI.

  \cite{cooper94}

**********************************************************************/

#include <stdlib.h>

#ifndef SQ
#define SQ(X) ((X)*(X))
#endif

int GetLandmarkPeriod(int numSamples, double *sample)
{
   const int max_zeros = 100;
   const int numLandmarks = 6;  // should be even
   const int regionSizeThreshold = 10;
   int numZeros;
   int zeros[max_zeros];
   double shape[max_zeros][numLandmarks];

   int x, z, l;

   // Find all (positive) zero-crossings

   numZeros = 0;
   for (x = 1; x < numSamples && numZeros < max_zeros; x++)
      if (sample[x - 1] < 0.0 && sample[x] >= 0.0)
         zeros[numZeros++] = x;

   if (numZeros < 3)
      return 0;

   // Extract the "shape" of each region

   for (z = 0; z < (numZeros - 1); z++) {
      int len = (zeros[z + 1] - zeros[z]);
      for (l = 0; l < (numLandmarks / 2); l++) {
         x = zeros[z] + (l + 1) * len / (numLandmarks + 2);
         shape[z][l] = sample[x];
         x = zeros[z] + (l + 2 +
                         (numLandmarks / 2)) * len / (numLandmarks + 2);
         shape[z][l + (numLandmarks / 2)] = sample[x];
      }
   }

  /***PRINT
  for(z=0; z<(numZeros-1); z++) {
	for(l=0; l<numLandmarks; l++)
	  printf("%5.2lf ",shape[z][l]);
	printf("\n");
  }*/

   // Find largest region
   int largest = 0;
   int largestSize = zeros[1] - zeros[0];

   for (z = 1; z < (numZeros - 1); z++)
      if (zeros[z + 1] - zeros[z] > largestSize) {
         largest = z;
         largestSize = zeros[z + 1] - zeros[z];
      }
   // Compare largest with all other regions

   int best = largest;
   double bestSimilarity = 0.0;

   for (z = 0; z < (numZeros - 1); z++)
      if (z != largest &&
          abs((zeros[z + 1] - zeros[z]) -
              (zeros[largest + 1] - zeros[largest]))
          < regionSizeThreshold) {

         double asq = 0.0, bsq = 0.0, ab = 0.0;
         double score = 0.0;

         for (l = 0; l < numLandmarks; l++) {

            // Should move this line out of loop
            asq += SQ(shape[largest][l]);
            bsq += SQ(shape[z][l]);
            ab += shape[largest][l] * shape[z][l];
         }

         score = ab / (asq + bsq - ab); // \cite{cooper94, Page 5}

         if (score > bestSimilarity) {
            best = z;
            bestSimilarity = score;
         }
      }

   int period = abs(zeros[best] - zeros[largest]);

   return period;
}
