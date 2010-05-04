/*------------------------------------------------------
   SINEWAVE.C -- Multimedia Windows Sine Wave Generator
                 (c) Charles Petzold, 1998
  ------------------------------------------------------*/

#include <windows.h>
#include <math.h>
#include "resource.h"

#define SAMPLE_RATE     11025
#define FREQ_MIN           20
#define FREQ_MAX         5000
#define FREQ_INIT         440
#define OUT_BUFFER_SIZE  4096
#define PI                  3.14159

BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName [] = TEXT ("SineWave") ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     if (-1 == DialogBox (hInstance, szAppName, NULL, DlgProc))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
     }
     return 0 ;
}

VOID FillBuffer (PBYTE pBuffer, int iFreq)
{
     static double fAngle ;
     int           i ;

     for (i = 0 ; i < OUT_BUFFER_SIZE ; i++)
     {
          pBuffer [i] = (BYTE) (127 + 127 * sin (fAngle)) ;

          fAngle += 2 * PI * iFreq / SAMPLE_RATE ;

          if (fAngle > 2 * PI)
               fAngle -= 2 * PI ;
     }
}

BOOL CALLBACK DlgProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static BOOL         bShutOff, bClosing ;
     static HWAVEOUT     hWaveOut ;
     static HWND         hwndScroll ;
     static int          iFreq = FREQ_INIT ;
     static PBYTE        pBuffer1, pBuffer2 ;
     static PWAVEHDR     pWaveHdr1, pWaveHdr2 ;
     static WAVEFORMATEX waveformat ;
     int                 iDummy ;
     
     switch (message)
     {
     case WM_INITDIALOG:
          hwndScroll = GetDlgItem (hwnd, IDC_SCROLL) ;
          SetScrollRange (hwndScroll, SB_CTL, FREQ_MIN, FREQ_MAX, FALSE) ;
          SetScrollPos   (hwndScroll, SB_CTL, FREQ_INIT, TRUE) ;
          SetDlgItemInt  (hwnd, IDC_TEXT, FREQ_INIT, FALSE) ;
          
          return TRUE ;
          
     case WM_HSCROLL:
          switch (LOWORD (wParam))
          {
          case SB_LINELEFT:   iFreq -=  1 ;  break ;
          case SB_LINERIGHT:  iFreq +=  1 ;  break ;
          case SB_PAGELEFT:   iFreq /=  2 ;  break ;
          case SB_PAGERIGHT:  iFreq *=  2 ;  break ;
               
          case SB_THUMBTRACK:
               iFreq = HIWORD (wParam) ;
               break ;
               
          case SB_TOP:
               GetScrollRange (hwndScroll, SB_CTL, &iFreq, &iDummy) ;
               break ;
               
          case SB_BOTTOM:
               GetScrollRange (hwndScroll, SB_CTL, &iDummy, &iFreq) ;
               break ;
          }
          
          iFreq = max (FREQ_MIN, min (FREQ_MAX, iFreq)) ;
          
          SetScrollPos (hwndScroll, SB_CTL, iFreq, TRUE) ;
          SetDlgItemInt (hwnd, IDC_TEXT, iFreq, FALSE) ;
          return TRUE ;
          
     case WM_COMMAND:
          switch (LOWORD (wParam))
          {
          case IDC_ONOFF:
                    // If turning on waveform, hWaveOut is NULL
               
               if (hWaveOut == NULL)
               {
                         // Allocate memory for 2 headers and 2 buffers

                    pWaveHdr1 = malloc (sizeof (WAVEHDR)) ;
                    pWaveHdr2 = malloc (sizeof (WAVEHDR)) ;
                    pBuffer1  = malloc (OUT_BUFFER_SIZE) ;
                    pBuffer2  = malloc (OUT_BUFFER_SIZE) ;

                    if (!pWaveHdr1 || !pWaveHdr2 || !pBuffer1 || !pBuffer2)
                    {
                         if (!pWaveHdr1) free (pWaveHdr1) ;
                         if (!pWaveHdr2) free (pWaveHdr2) ;
                         if (!pBuffer1)  free (pBuffer1) ;
                         if (!pBuffer2)  free (pBuffer2) ;

                         MessageBeep (MB_ICONEXCLAMATION) ;
                         MessageBox (hwnd, TEXT ("Error allocating memory!"),
                                     szAppName, MB_ICONEXCLAMATION | MB_OK) ;
                         return TRUE ;
                    }

                         // Variable to indicate Off button pressed

                    bShutOff = FALSE ;
                         
                         // Open waveform audio for output
                         
                    waveformat.wFormatTag      = WAVE_FORMAT_PCM ;
                    waveformat.nChannels       = 1 ;
                    waveformat.nSamplesPerSec  = SAMPLE_RATE ;
                    waveformat.nAvgBytesPerSec = SAMPLE_RATE ;
                    waveformat.nBlockAlign     = 1 ;
                    waveformat.wBitsPerSample  = 8 ;
                    waveformat.cbSize          = 0 ;
                         
                    if (waveOutOpen (&hWaveOut, WAVE_MAPPER, &waveformat,
                                     (DWORD) hwnd, 0, CALLBACK_WINDOW)
                              != MMSYSERR_NOERROR)
                    {
                         free (pWaveHdr1) ;
                         free (pWaveHdr2) ;
                         free (pBuffer1) ;
                         free (pBuffer2) ;

                         hWaveOut = NULL ;
                         MessageBeep (MB_ICONEXCLAMATION) ;
                         MessageBox (hwnd, 
                              TEXT ("Error opening waveform audio device!"),
                              szAppName, MB_ICONEXCLAMATION | MB_OK) ;
                         return TRUE ;
                    }

                         // Set up headers and prepare them

                    pWaveHdr1->lpData          = pBuffer1 ;
                    pWaveHdr1->dwBufferLength  = OUT_BUFFER_SIZE ;
                    pWaveHdr1->dwBytesRecorded = 0 ;
                    pWaveHdr1->dwUser          = 0 ;
                    pWaveHdr1->dwFlags         = 0 ;
                    pWaveHdr1->dwLoops         = 1 ;
                    pWaveHdr1->lpNext          = NULL ;
                    pWaveHdr1->reserved        = 0 ;
                    
                    waveOutPrepareHeader (hWaveOut, pWaveHdr1, 
                                          sizeof (WAVEHDR)) ;

                    pWaveHdr2->lpData          = pBuffer2 ;
                    pWaveHdr2->dwBufferLength  = OUT_BUFFER_SIZE ;
                    pWaveHdr2->dwBytesRecorded = 0 ;
                    pWaveHdr2->dwUser          = 0 ;
                    pWaveHdr2->dwFlags         = 0 ;
                    pWaveHdr2->dwLoops         = 1 ;
                    pWaveHdr2->lpNext          = NULL ;
                    pWaveHdr2->reserved        = 0 ;
                    
                    waveOutPrepareHeader (hWaveOut, pWaveHdr2,
                                          sizeof (WAVEHDR)) ;
               }
                    // If turning off waveform, reset waveform audio
               else
               {
                    bShutOff = TRUE ;
                    waveOutReset (hWaveOut) ;
               }
               return TRUE ;
          }
          break ;

               // Message generated from waveOutOpen call
               
     case MM_WOM_OPEN:
          SetDlgItemText (hwnd, IDC_ONOFF, TEXT ("Turn Off")) ;

               // Send two buffers to waveform output device
                    
          FillBuffer (pBuffer1, iFreq) ;
          waveOutWrite (hWaveOut, pWaveHdr1, sizeof (WAVEHDR)) ;
                    
          FillBuffer (pBuffer2, iFreq) ;
          waveOutWrite (hWaveOut, pWaveHdr2, sizeof (WAVEHDR)) ;
          return TRUE ;

               // Message generated when a buffer is finished
                    
     case MM_WOM_DONE:
          if (bShutOff)
          {
               waveOutClose (hWaveOut) ;
               return TRUE ;
          }

               // Fill and send out a new buffer

          FillBuffer (((PWAVEHDR) lParam)->lpData, iFreq) ;
          waveOutWrite (hWaveOut, (PWAVEHDR) lParam, sizeof (WAVEHDR)) ;
          return TRUE ;
          
     case MM_WOM_CLOSE:
          waveOutUnprepareHeader (hWaveOut, pWaveHdr1, sizeof (WAVEHDR)) ;
          waveOutUnprepareHeader (hWaveOut, pWaveHdr2, sizeof (WAVEHDR)) ;

          free (pWaveHdr1) ;
          free (pWaveHdr2) ;
          free (pBuffer1) ;
          free (pBuffer2) ;

          hWaveOut = NULL ;
          SetDlgItemText (hwnd, IDC_ONOFF, TEXT ("Turn On")) ;
          
          if (bClosing)
               EndDialog (hwnd, 0) ;
          
          return TRUE ;
          
     case WM_SYSCOMMAND:
          switch (wParam)
          {
          case SC_CLOSE:
               if (hWaveOut != NULL)
               {
                    bShutOff = TRUE ;
                    bClosing = TRUE ;
                    
                    waveOutReset (hWaveOut) ;
               }
               else
                    EndDialog (hwnd, 0) ;
               
               return TRUE ;
          }
          break ;
     }
     return FALSE ;
}
