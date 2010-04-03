/**********************************************************************

  Audacity: A Digital Audio Editor

  Landmark.h

  Dominic Mazzoni

  Pitch detector based on "landmark" algorithm by Cooper and Ng,
  University of Leeds SCS, Division of AI.

  \cite{cooper94}

**********************************************************************/

int GetLandmarkPeriod(int numSamples, double *sample);
