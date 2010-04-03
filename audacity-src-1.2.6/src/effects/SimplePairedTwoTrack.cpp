/**********************************************************************

  Audacity: A Digital Audio Editor

  SimplePairedTwoTrack.cpp

  Vincent A. Busam
  Dominic Mazzoni

  This abstract class simplifies the implementation of a basic
  two track effect where you want the values of both tracks
  together.  Inherit from it if your effect doesn't just
  modifies a track in place and doesn't care how many samples
  it gets at a time.  Your derived class only needs to implement
  GetEffectName, GetEffectAction, and ProcessSimplePairedTwoTrack.

**********************************************************************/


//    O B S O L E T E
