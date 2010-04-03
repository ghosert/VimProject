/**********************************************************************

  Audacity: A Digital Audio Editor

  Invert.cpp

  Mark Phillips

  This class inverts the selected audio.

**********************************************************************/

#include "Invert.h"

bool EffectInvert::ProcessSimpleMono(float *buffer, sampleCount len)
{
   sampleCount i;
   for (i = 0; i < len; i++)
      buffer[i] = -buffer[i];
   return true;
}
