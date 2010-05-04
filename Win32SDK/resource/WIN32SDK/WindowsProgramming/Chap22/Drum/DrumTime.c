/*-----------------------------------------
   DRUMFILE.C -- Timer Routines for DRUM
                 (c) Charles Petzold, 1998
  -----------------------------------------*/

#include <windows.h>
#include "drumtime.h"

#define minmax(a,x,b) (min (max (x, a), b))

#define TIMER_RES   5

void CALLBACK DrumTimerFunc (UINT, UINT, DWORD, DWORD, DWORD) ;

BOOL     bSequenceGoing, bEndSequence ;
DRUM     drum ;
HMIDIOUT hMidiOut ;
HWND     hwndNotify ;
int      iIndex ;
UINT     uTimerRes, uTimerID ;

DWORD MidiOutMessage (HMIDIOUT hMidi, int iStatus, int iChannel,
                      int iData1, int iData2)
{
     DWORD dwMessage ;
     
     dwMessage = iStatus | iChannel | (iData1 << 8) | (iData2 << 16) ;
     
     return midiOutShortMsg (hMidi, dwMessage) ;
}

void DrumSetParams (PDRUM pdrum)
{
     CopyMemory (&drum, pdrum, sizeof (DRUM)) ;
}

BOOL DrumBeginSequence (HWND hwnd)
{
     TIMECAPS tc ;
     
     hwndNotify = hwnd ;           // Save window handle for notification
     DrumEndSequence (TRUE) ;      // Stop current sequence if running
     
          // Open the MIDI Mapper output port
     
     if (midiOutOpen (&hMidiOut, MIDIMAPPER, 0, 0, 0))
          return FALSE ;
     
          // Send Program Change messages for channels 9 and 0
     
     MidiOutMessage (hMidiOut, 0xC0, 9, 0, 0) ;
     MidiOutMessage (hMidiOut, 0xC0, 0, 0, 0) ;
     
          // Begin sequence by setting a timer event
     
     timeGetDevCaps (&tc, sizeof (TIMECAPS)) ;
     uTimerRes = minmax (tc.wPeriodMin, TIMER_RES, tc.wPeriodMax) ;
     timeBeginPeriod (uTimerRes) ;
     
     uTimerID = timeSetEvent (max ((UINT) uTimerRes, (UINT) drum.iMsecPerBeat),
                              uTimerRes, DrumTimerFunc, 0, TIME_ONESHOT) ;
     
     if (uTimerID == 0)
     {
          timeEndPeriod (uTimerRes) ;
          midiOutClose (hMidiOut) ;
          return FALSE ;
     }
     
     iIndex = -1 ;
     bEndSequence = FALSE ;
     bSequenceGoing = TRUE ;
     
     return TRUE ;
}

void DrumEndSequence (BOOL bRightAway)
{
     if (bRightAway)
     {
          if (bSequenceGoing)
          {
                    // stop the timer
               if (uTimerID)
                    timeKillEvent (uTimerID) ;
               timeEndPeriod (uTimerRes) ;

                    // turn off all notes
               MidiOutMessage (hMidiOut, 0xB0, 9, 123, 0) ;
               MidiOutMessage (hMidiOut, 0xB0, 0, 123, 0) ;
               
                    // close the MIDI port
               midiOutClose (hMidiOut) ;
               bSequenceGoing = FALSE ;
          }
     }
     else
          bEndSequence = TRUE ;
}

void CALLBACK DrumTimerFunc (UINT  uID, UINT uMsg, DWORD dwUser,
                             DWORD dw1, DWORD dw2)
{
     static DWORD dwSeqPercLast [NUM_PERC], dwSeqPianLast [NUM_PERC] ;
     int          i ;
     
         // Note Off messages for channels 9 and 0
     
     if (iIndex != -1)
     {
          for (i = 0 ; i < NUM_PERC ; i++)
          {
               if (dwSeqPercLast[i] & 1 << iIndex)
                    MidiOutMessage (hMidiOut, 0x80, 9, i + 35, 0) ;
               
               if (dwSeqPianLast[i] & 1 << iIndex) 
                    MidiOutMessage (hMidiOut, 0x80, 0, i + 35, 0) ;
          }
     }
     
          // Increment index and notify window to advance bouncing ball
     
     iIndex = (iIndex + 1) % drum.iNumBeats ;
     PostMessage (hwndNotify, WM_USER_NOTIFY, iIndex, timeGetTime ()) ;
     
          // Check if ending the sequence
     
     if (bEndSequence && iIndex == 0)
     {
          PostMessage (hwndNotify, WM_USER_FINISHED, 0, 0L) ;
          return ;
     }
     
          // Note On messages for channels 9 and 0
     
     for (i = 0 ; i < NUM_PERC ; i++)
     {
          if (drum.dwSeqPerc[i] & 1 << iIndex)
               MidiOutMessage (hMidiOut, 0x90, 9, i + 35, drum.iVelocity) ;
          
          if (drum.dwSeqPian[i] & 1 << iIndex)
               MidiOutMessage (hMidiOut, 0x90, 0, i + 35, drum.iVelocity) ;
          
          dwSeqPercLast[i] = drum.dwSeqPerc[i] ;
          dwSeqPianLast[i] = drum.dwSeqPian[i] ;
     }
          // Set a new timer event
     
     uTimerID = timeSetEvent (max ((int) uTimerRes, drum.iMsecPerBeat),
                              uTimerRes, DrumTimerFunc, 0, TIME_ONESHOT) ;
     
     if (uTimerID == 0)
     {
          PostMessage (hwndNotify, WM_USER_ERROR, 0, 0) ;
     }
}
