// writemidi.h

#ifndef __WRITE_MIDI__
#define __WRITE_MIDI__

#include "allegro.h"

class WriteMIDI {
public:
  WriteMIDI(Seq_ptr seq);
  ~WriteMIDI();

  void           Write(FILE *fp /* , midiFileFormat = 1 */);

private:
  void           CreateEventList();
 
  FILE           *mOutFile;
  
  Seq_ptr        mSeq;

  int            mNumTracks; // number of tracks not counting tempo track
  int            mDivision;
  int            mInitialTempo;
  
  Allegro_event  **mEvents;
  int            mNumEvents;
  int            mMaxEvents;
  
  void           WriteVarinum(int num);
  void           Write16bit(int num);
  void           Write24bit(int num);
  void           Write32bit(int num);
};

#endif // #define __SIMPLE_MIDI_OUT__
