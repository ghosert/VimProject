/*------------------------------------------------------------
   DRUMTIME.H Header File for Time Functions for DRUM Program
  ------------------------------------------------------------*/

#define NUM_PERC         47
#define WM_USER_NOTIFY   (WM_USER + 1)
#define WM_USER_FINISHED (WM_USER + 2)
#define WM_USER_ERROR    (WM_USER + 3)

#pragma pack(push, 2)

typedef struct
{
     short iMsecPerBeat ;
     short iVelocity ;
     short iNumBeats ;
     DWORD dwSeqPerc [NUM_PERC] ;
     DWORD dwSeqPian [NUM_PERC] ;
}
DRUM, * PDRUM ;

#pragma pack(pop)

void DrumSetParams     (PDRUM) ;
BOOL DrumBeginSequence (HWND)  ;
void DrumEndSequence   (BOOL)  ;
