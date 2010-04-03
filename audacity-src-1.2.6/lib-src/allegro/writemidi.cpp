// writemidi.cpp

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "writemidi.h"

WriteMIDI::WriteMIDI(Seq_ptr seq)
{
  mOutFile = NULL;

  mNumTracks = 1;           // 1 not counting tempo track
  mDivision = 120;          // divisions per quarter note
  
  mInitialTempo = 500000;

  mSeq = seq;
  
  mNumEvents = 0;
  mMaxEvents = 0;
  mEvents = NULL;
  
  CreateEventList();
}

WriteMIDI::~WriteMIDI()
{
}

int EventCompareFunc(const void *p1, const void *p2)
{
  Allegro_event **e1 = (Allegro_event **)p1;
  Allegro_event **e2 = (Allegro_event **)p2;
  
  if ((*e1)->time < (*e2)->time)
    return -1;
  else if ((*e1)->time > (*e2)->time)
    return 1;
  else if ((*e1)->chan < (*e2)->chan)
    return -1;
  else if ((*e1)->chan > (*e2)->chan)
    return 1;
  else {
    fprintf(stderr, "Error: Unable to sort MIDI events.\n");
    exit(0);
    return 0;
  }
}

void WriteMIDI::CreateEventList()
{
  fprintf(stderr, "Loading MIDI events...\n");

  int numEvents = mSeq->notes.len;
  
  mNumEvents = 0;
  mMaxEvents = numEvents * 2;
  mEvents = new Allegro_event *[mMaxEvents];

  for(int i=0; i<numEvents; i++) {
  
    if (mSeq->notes[i]->type == 'n') {
      // note event must be split into note on and note off messages
    
      Allegro_note *note = (Allegro_note *)mSeq->notes[i];
    
      mEvents[mNumEvents] = new Allegro_note();
      mEvents[mNumEvents+1] = new Allegro_note();
  
      // note-on event
      *((Allegro_note *)mEvents[mNumEvents]) = *note;
      
      // note-off event
      *((Allegro_note *)mEvents[mNumEvents+1]) = *note;
      mEvents[mNumEvents+1]->time += note->dur;
      ((Allegro_note *)mEvents[mNumEvents+1])->loud = 0.0;
      
      // Use the high bits of the channel to store relative ordering of events
      mEvents[mNumEvents]->chan &= 15;
      mEvents[mNumEvents+1]->chan &= 15;
      mEvents[mNumEvents]->chan += (i << 4);
      mEvents[mNumEvents+1]->chan += (i << 4);
      
      mNumEvents += 2;
    }
    /*
    else {
      // any other event
      
      mEvents[mNumEvents] = mSeq->notes[i];
      mEvents[mNumEvents].chan &= 15;
      mEvents[mNumEvents].chan += (i << 4);

      mNumEvents++;    
    }*/
  }
  
  fprintf(stderr, "Sorting events...\n");
  
  // Now quicksort them, first by time and then by original order
  
  qsort((void *)mEvents, mNumEvents, sizeof(Allegro_event *), EventCompareFunc);
}

void WriteMIDI::Write(FILE *fp)
{
  int trackLenOffset;
  int trackEndOffset;
  int trackLen;

  fprintf(stderr, "Writing MIDI file...\n");

  mOutFile = fp;

  //
  // Header
  //

  fprintf(mOutFile, "MThd");
  Write32bit(6); // chunk length

  Write16bit(1); // format 1 MIDI file
  Write16bit(mNumTracks + 1); // extra track is the tempo track
  Write16bit(mDivision); // divisions per quarter note

  //
  // Tempo track
  //
  
  fprintf(mOutFile, "MTrk");
  
  trackLenOffset = ftell(mOutFile);
  Write32bit(0); // track len placeholder

  // Set initial tempo
  WriteVarinum(0);           // delta time
  putc(0xFF, mOutFile);      // meta-event
  putc(0x51, mOutFile);      // set tempo
  putc(0x03, mOutFile);      // len
  Write24bit(mInitialTempo);

  // End of track
  WriteVarinum(0);           // delta time
  putc(0xFF, mOutFile);
  putc(0x2F, mOutFile);
  putc(0x00, mOutFile);
  
  // Go back and write in the length of the track
  trackEndOffset = ftell(mOutFile);
  trackLen = trackEndOffset - trackLenOffset - 4;
  fseek(mOutFile, trackLenOffset, SEEK_SET);
  Write32bit(trackLen);
  fseek(mOutFile, trackEndOffset, SEEK_SET);

  //
  // Music track
  //
  
  fprintf(mOutFile, "MTrk");
  trackLenOffset = ftell(mOutFile);
  Write32bit(0); // track len placeholder

  double lastTime = 0.0;
  for(int i=0; i<mNumEvents; i++) {
  
    double deltaTime = mEvents[i]->time - lastTime;
    int deltaDivisions = deltaTime * 1000000 / mInitialTempo * mDivision;
  
    if (mEvents[i]->type == 'n') {
      WriteVarinum(deltaDivisions);
      
      Allegro_note_ptr note = (Allegro_note_ptr)(mEvents[i]);

      int vel = note->loud;
      if (vel < 0)
        vel = 0;
      if (vel > 127)
        vel = 127;
      char chan = (note->chan & 15);
      int pitch = int(note->pitch + 0.5);
      if (pitch < 1 || pitch > 127) {
        fprintf(stderr, "Bad pitch: %d", pitch);
        exit(0);
      }
      if (vel > 0)
        putc(0x90 + chan, mOutFile); // note-on
      else
        putc(0x80 + chan, mOutFile); // note-off
      putc(pitch, mOutFile);
      putc(vel, mOutFile);

      lastTime = note->time;
    }
    
    /*
    else {
      WriteVarinum(deltaDivisions);

      ...write the meta-event...

      lastTime = note->time;    
    }
    */
  }

  // End of track event
  WriteVarinum(0);           // delta time
  putc(0xFF, mOutFile);
  putc(0x2F, mOutFile);
  putc(0x00, mOutFile);

  // Go back and write in the length of the track
  trackEndOffset = ftell(mOutFile);
  trackLen = trackEndOffset - trackLenOffset - 4;
  fseek(mOutFile, trackLenOffset, SEEK_SET);
  Write32bit(trackLen);
  fseek(mOutFile, trackEndOffset, SEEK_SET);

  fprintf(stderr, "Done\n");
}

void WriteMIDI::Write16bit(int num)
{
  putc((num & 0xFF00)>>8, mOutFile);
  putc((num & 0xFF), mOutFile);
}

void WriteMIDI::Write24bit(int num)
{
  putc((num & 0xFF0000)>>16, mOutFile);
  putc((num & 0xFF00)>>8, mOutFile);
  putc((num & 0xFF), mOutFile);
}

void WriteMIDI::Write32bit(int num)
{
  putc((num & 0xFF000000)>>24, mOutFile);
  putc((num & 0xFF0000)>>16, mOutFile);
  putc((num & 0xFF00)>>8, mOutFile);
  putc((num & 0xFF), mOutFile);
}

void WriteMIDI::WriteVarinum(int value)
{
  int buffer;

  buffer = value & 0x7f;
  while ((value >>= 7) > 0) {
    buffer <<= 8;
    buffer |= 0x80;
    buffer += (value & 0x7f);
  }

  for(;;) {
    putc(buffer, mOutFile);
    if (buffer & 0x80)
      buffer >>= 8;
    else
      break;
  }
}
