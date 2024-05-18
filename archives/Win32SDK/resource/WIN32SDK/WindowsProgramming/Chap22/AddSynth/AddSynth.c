/*---------------------------------------------------
   ADDSYNTH.C -- Additive Synthesis Sound Generation
                 (c) Charles Petzold, 1998
  ---------------------------------------------------*/

#include <windows.h>
#include <math.h>
#include "addsynth.h"
#include "resource.h"

#define ID_TIMER             1
#define SAMPLE_RATE      22050
#define MAX_PARTIALS        21
#define PI             3.14159

BOOL CALLBACK DlgProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName [] = TEXT ("AddSynth") ;

// Sine wave generator
// -------------------

double SineGenerator (double dFreq, double * pdAngle)
{
     double dAmp ;
     
     dAmp = sin (* pdAngle) ;
     * pdAngle += 2 * PI * dFreq / SAMPLE_RATE ;
     
     if (* pdAngle >= 2 * PI)
          * pdAngle -= 2 * PI ;
     
     return dAmp ;
}

// Fill a buffer with composite waveform
// -------------------------------------

VOID FillBuffer (INS ins, PBYTE pBuffer, int iNumSamples)
{
     static double dAngle [MAX_PARTIALS] ;
     double        dAmp, dFrq, dComp, dFrac ;
     int           i, iPrt, iMsecTime, iCompMaxAmp, iMaxAmp, iSmp ;
     
          // Calculate the composite maximum amplitude
     
     iCompMaxAmp = 0 ;
     
     for (iPrt = 0 ; iPrt < ins.iNumPartials ; iPrt++)
     {
          iMaxAmp = 0 ;
          
          for (i = 0 ; i < ins.pprt[iPrt].iNumAmp ; i++)
               iMaxAmp = max (iMaxAmp, ins.pprt[iPrt].pEnvAmp[i].iValue) ;
          
          iCompMaxAmp += iMaxAmp ;
     }
     
          // Loop through each sample
     
     for (iSmp = 0 ; iSmp < iNumSamples ; iSmp++)
     {
          dComp = 0 ;
          iMsecTime = (int) (1000 * iSmp / SAMPLE_RATE) ;
          
               // Loop through each partial
          
          for (iPrt = 0 ; iPrt < ins.iNumPartials ; iPrt++)
          {
               dAmp = 0 ;
               dFrq = 0 ;
               
               for (i = 0 ; i < ins.pprt[iPrt].iNumAmp - 1 ; i++)
               {
                    if (iMsecTime >= ins.pprt[iPrt].pEnvAmp[i  ].iTime &&
                         iMsecTime <= ins.pprt[iPrt].pEnvAmp[i+1].iTime)
                    {
                         dFrac = (double) (iMsecTime -
                              ins.pprt[iPrt].pEnvAmp[i  ].iTime) /
                              (ins.pprt[iPrt].pEnvAmp[i+1].iTime -
                              ins.pprt[iPrt].pEnvAmp[i  ].iTime) ;
                         
                         dAmp = dFrac  * ins.pprt[iPrt].pEnvAmp[i+1].iValue +
                              (1-dFrac) * ins.pprt[iPrt].pEnvAmp[i  ].iValue ;
                         
                         break ;
                    }
               }
               
               for (i = 0 ; i < ins.pprt[iPrt].iNumFrq - 1 ; i++)
               {
                    if (iMsecTime >= ins.pprt[iPrt].pEnvFrq[i  ].iTime &&
                         iMsecTime <= ins.pprt[iPrt].pEnvFrq[i+1].iTime)
                    {
                         dFrac = (double) (iMsecTime -
                              ins.pprt[iPrt].pEnvFrq[i  ].iTime) /
                              (ins.pprt[iPrt].pEnvFrq[i+1].iTime -
                              ins.pprt[iPrt].pEnvFrq[i  ].iTime) ;
                         
                         dFrq = dFrac  * ins.pprt[iPrt].pEnvFrq[i+1].iValue +
                              (1-dFrac) * ins.pprt[iPrt].pEnvFrq[i  ].iValue ;
                         
                         break ;
                    }
               }
               dComp += dAmp * SineGenerator (dFrq, dAngle + iPrt) ;
          }
          pBuffer[iSmp] = (BYTE) (127 + 127 * dComp / iCompMaxAmp) ;
     }
}

// Make a waveform file
// --------------------

BOOL MakeWaveFile (INS ins, TCHAR * szFileName)
{
     DWORD        dwWritten ;
     HANDLE       hFile ;
     int          iChunkSize, iPcmSize, iNumSamples ;
     PBYTE        pBuffer ;
     WAVEFORMATEX waveform ;

     hFile = CreateFile (szFileName, GENERIC_WRITE, 0, NULL,
                         CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL) ;
 
     if (hFile == NULL)
          return FALSE ;
     
     iNumSamples = ((long) ins.iMsecTime * SAMPLE_RATE / 1000 + 1) / 2 * 2 ;
     iPcmSize    = sizeof (PCMWAVEFORMAT) ;
     iChunkSize  = 12 + iPcmSize + 8 + iNumSamples ;
     
     if (NULL == (pBuffer = malloc (iNumSamples)))
     {
          CloseHandle (hFile) ;
          return FALSE ;
     }
     
     FillBuffer (ins, pBuffer, iNumSamples) ;
     
     waveform.wFormatTag      = WAVE_FORMAT_PCM ;
     waveform.nChannels       = 1 ;
     waveform.nSamplesPerSec  = SAMPLE_RATE ;
     waveform.nAvgBytesPerSec = SAMPLE_RATE ;
     waveform.nBlockAlign     = 1 ;
     waveform.wBitsPerSample  = 8 ;
     waveform.cbSize          = 0 ;
     
     WriteFile (hFile, "RIFF",       4, &dwWritten, NULL) ;
     WriteFile (hFile, &iChunkSize,  4, &dwWritten, NULL) ;
     WriteFile (hFile, "WAVEfmt ",   8, &dwWritten, NULL) ;
     WriteFile (hFile, &iPcmSize,    4, &dwWritten, NULL) ;
     WriteFile (hFile, &waveform, sizeof (WAVEFORMATEX) - 2, &dwWritten, NULL) ;
     WriteFile (hFile, "data",       4, &dwWritten, NULL) ;
     WriteFile (hFile, &iNumSamples, 4, &dwWritten, NULL) ;
     WriteFile (hFile, pBuffer,      iNumSamples,  &dwWritten, NULL) ;
     
     CloseHandle (hFile) ;
     free (pBuffer) ;
     
     if ((int) dwWritten != iNumSamples)
     {
          DeleteFile (szFileName) ;
          return FALSE ;
     }
     return TRUE ;
}

void TestAndCreateFile (HWND hwnd, INS ins, TCHAR * szFileName, int idButton)
{
     TCHAR szMessage [64] ;
     
     if (-1 != GetFileAttributes (szFileName))
          EnableWindow (GetDlgItem (hwnd, idButton), TRUE) ;
     else
     {
          if (MakeWaveFile (ins, szFileName))
               EnableWindow (GetDlgItem (hwnd, idButton), TRUE) ;
          else
          {
               wsprintf (szMessage, TEXT ("Could not create %x."), szFileName) ;
               MessageBeep (MB_ICONEXCLAMATION) ;
               MessageBox (hwnd, szMessage, szAppName,
                                 MB_OK | MB_ICONEXCLAMATION) ;
          }
     }
}

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

BOOL CALLBACK DlgProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static TCHAR * szTrum = TEXT ("Trumpet.wav") ;
     static TCHAR * szOboe = TEXT ("Oboe.wav") ;
     static TCHAR * szClar = TEXT ("Clarinet.wav") ;
     
     switch (message)
     {
     case WM_INITDIALOG:
          SetTimer (hwnd, ID_TIMER, 1, NULL) ;
          return TRUE ;
          
     case WM_TIMER:
          KillTimer (hwnd, ID_TIMER) ;
          SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
          ShowCursor (TRUE) ;
          
          TestAndCreateFile (hwnd, insTrum, szTrum, IDC_TRUMPET) ;
          TestAndCreateFile (hwnd, insOboe, szOboe, IDC_OBOE) ;
          TestAndCreateFile (hwnd, insClar, szClar, IDC_CLARINET) ;
          
          SetDlgItemText (hwnd, IDC_TEXT, TEXT (" ")) ;
          SetFocus (GetDlgItem (hwnd, IDC_TRUMPET)) ;
          
          ShowCursor (FALSE) ;
          SetCursor (LoadCursor (NULL, IDC_ARROW)) ;
          return TRUE ;
          
     case WM_COMMAND:
          switch (LOWORD (wParam))
          {
          case IDC_TRUMPET:
               PlaySound (szTrum, NULL, SND_FILENAME | SND_SYNC) ;
               return TRUE ;
               
          case IDC_OBOE:
               PlaySound (szOboe, NULL, SND_FILENAME | SND_SYNC) ;
               return TRUE ;
               
          case IDC_CLARINET:
               PlaySound (szClar, NULL, SND_FILENAME |SND_SYNC) ;
               return TRUE ;
          }
          break ;
          
     case WM_SYSCOMMAND:
          switch (LOWORD (wParam))
          {
          case SC_CLOSE:
               EndDialog (hwnd, 0) ;
               return TRUE ;
          }
          break ;
     }
     return FALSE ;
}
