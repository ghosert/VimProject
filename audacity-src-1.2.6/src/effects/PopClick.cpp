/**********************************************************************

  Audacity: A Digital Audio Editor

  PopClick.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"

#include <math.h>

#include <wx/msgdlg.h>
#include <wx/intl.h>

#include "PopClick.h"
#include "../WaveTrack.h"

EffectPopClickRemoval::EffectPopClickRemoval()
{
}

EffectPopClickRemoval::~EffectPopClickRemoval()
{
}

bool EffectPopClickRemoval::PromptUser()
{
   return true;
}

bool EffectPopClickRemoval::Process()
{
   TrackListIterator iter(mWaveTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;
      double deltat = t1 - t0;
      double tpre;
      double pretime;
      double rate = track->GetRate();
      int max_matrix_size = 1500;

      pretime = 3*deltat;
      if (pretime * rate > max_matrix_size)
         pretime = max_matrix_size / rate;
      tpre = t0 - pretime;
      tpre = tpre < trackStart? trackStart: tpre;
      tpre = tpre > t0? t0: tpre;

      if (t1 - t0 > 1) {
         ::wxMessageBox(_("Cannot remove a pop or click longer than "
                          "one second."));
      }
      else if (t1 > t0) {
         if ((t0 - tpre) * rate < 20)
            ::wxMessageBox(_("Cannot remove a pop or click at the very "
                             "beginning of a track."));
         else {
            longSampleCount pre = track->TimeToLongSamples(tpre);
            longSampleCount start = track->TimeToLongSamples(t0);
            longSampleCount end = track->TimeToLongSamples(t1);
            sampleCount preLen = (sampleCount)(start - pre);
            sampleCount postLen = (sampleCount)(end - start);
            
            if (!ProcessOne(count, track, pre, preLen, postLen))
               return false;
         }
      }

      track = (WaveTrack *) iter.Next();
      count++;
   }

   return true;
}

// Solves R*h = q, where R is the symmetric Toeplitz matrix
// whose first column is r.
// Algorithm from Roberts & Mullis, p.233
void toepsolve(int n, float *r, float *q, float *h)
{
   float *a;
   float alpha, beta;
   int j, k;

   alpha = r[0];
   n = n - 1;

   h[0] = q[0]/r[0];
   
   a = new float[(n+1) * (n+1)];
   a[(0*(n+1))+0] = 1.0;
   
   for(k = 1; k <= n; k++) {
      a[(k*(n+1))+k-1] = 0;
      a[(0*(n+1))+k] = 1.0;
      beta = 0.0;
      for (j = 0; j <= k-1; j++) {
         beta += r[k-j]*a[(j*(n+1))+k-1];
      }
      beta /= alpha;
      for (j = 1; j <= k; j++) {
         a[(j*(n+1))+k] = a[(j*(n+1))+k-1] - beta*a[((k-j)*(n+1))+k-1];
      }
      alpha *= (1 - beta*beta);
      h[k] = q[k];
      for (j = 0; j <= k-1; j++) {
         h[k] -= r[k-j]*h[j];
      }
      h[k] /= alpha;
      for (j = 0; j <= k-1; j++) {
         h[j] += a[((k-j)*(n+1))+k]*h[k];
      }
   }
   
   delete[] a;

   return;
}

bool EffectPopClickRemoval::ProcessOne(int count, WaveTrack * track,
                                       longSampleCount start,
                                       sampleCount preLen,
                                       sampleCount postLen)
{
   sampleCount totalLen = preLen + postLen;
   sampleCount i, j = 0;
   sampleCount n = preLen;
   float *buffer = new float[totalLen];
   float *AC = new float[n+1];
   float *coef = new float[n];

   track->Get((samplePtr) buffer, floatSample, start, totalLen);

   // Compute biased autocorrelation estimate

   for(i=0; i<n+1; i++) {
      double sum = 0;
      for(j=0; j<n-i; j++)
         sum += buffer[j] * buffer[i+j];
      AC[i] = sum / n;
   }

   // Solve for least-squares autoregression (LSAR) coefficients using a
   // fast Toeplitz matrix solver.

   toepsolve(n, AC, &AC[1], coef);

   // Fill in the buffer

   for(i=0; i<postLen; i++) {
      float pos = (i / (float)postLen);
      double sumd;
      float sum;
      float orig;

      orig = buffer[preLen+i];
      sumd = 0;
      for(j=0; j<n; j++)
         sumd += coef[j] * buffer[preLen+i-(j+1)];
      sum = (float)sumd;

      // Window: first 10% fades in, last 20% fades out, using Hanning window

      if (pos < 0.10) {
         float window = 0.5 - 0.5 * cos(M_PI * (pos/0.10));
         sum = window*sum + (1.0-window)*orig;
      }
      else if (pos >= 0.80) {
         float window = 0.5 + 0.5 * cos(M_PI * ((pos-0.80)/0.20));
         sum = window*sum + (1.0-window)*orig;
      }

      buffer[preLen+i] = sum;
   }

   track->Set((samplePtr)&buffer[preLen], floatSample,
              start + preLen, postLen);

   delete[] AC;
   delete[] coef;
   delete[] buffer;

   return true;
}
