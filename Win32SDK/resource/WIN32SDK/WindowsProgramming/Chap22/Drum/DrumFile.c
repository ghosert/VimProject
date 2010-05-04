/*------------------------------------------
   DRUMFILE.C -- File I/O Routines for DRUM
                 (c) Charles Petzold, 1998
  ------------------------------------------*/

#include <windows.h>
#include <commdlg.h>
#include "drumtime.h"
#include "drumfile.h"

OPENFILENAME ofn = { sizeof (OPENFILENAME) } ;

TCHAR * szFilter[] = { TEXT ("Drum Files (*.DRM)"),  
                       TEXT ("*.drm"), TEXT ("") } ;

TCHAR szDrumID   [] = TEXT ("DRUM") ;
TCHAR szListID   [] = TEXT ("LIST") ;
TCHAR szInfoID   [] = TEXT ("INFO") ;
TCHAR szSoftID   [] = TEXT ("ISFT") ;
TCHAR szDateID   [] = TEXT ("ISCD") ;
TCHAR szFmtID    [] = TEXT ("fmt ") ;
TCHAR szDataID   [] = TEXT ("data") ;
char  szSoftware [] = "DRUM by Charles Petzold, Programming Windows" ;

TCHAR szErrorNoCreate    [] = TEXT ("File %s could not be opened for writing.");
TCHAR szErrorCannotWrite [] = TEXT ("File %s could not be written to.") ;
TCHAR szErrorNotFound    [] = TEXT ("File %s not found or cannot be opened.") ;
TCHAR szErrorNotDrum     [] = TEXT ("File %s is not a standard DRUM file.") ;
TCHAR szErrorUnsupported [] = TEXT ("File %s is not a supported DRUM file.") ;
TCHAR szErrorCannotRead  [] = TEXT ("File %s cannot be read.") ;

BOOL DrumFileOpenDlg (HWND hwnd, TCHAR * szFileName, TCHAR * szTitleName)
{
     ofn.hwndOwner         = hwnd ;
     ofn.lpstrFilter       = szFilter [0] ;
     ofn.lpstrFile         = szFileName ;
     ofn.nMaxFile          = MAX_PATH ;
     ofn.lpstrFileTitle    = szTitleName ;
     ofn.nMaxFileTitle     = MAX_PATH ;
     ofn.Flags             = OFN_CREATEPROMPT ;
     ofn.lpstrDefExt       = TEXT ("drm") ;
     
     return GetOpenFileName (&ofn) ;
}

BOOL DrumFileSaveDlg (HWND hwnd, TCHAR * szFileName, TCHAR * szTitleName)
{
     ofn.hwndOwner         = hwnd ;
     ofn.lpstrFilter       = szFilter [0] ;
     ofn.lpstrFile         = szFileName ;
     ofn.nMaxFile          = MAX_PATH ;
     ofn.lpstrFileTitle    = szTitleName ;
     ofn.nMaxFileTitle     = MAX_PATH ;
     ofn.Flags             = OFN_OVERWRITEPROMPT ;
     ofn.lpstrDefExt       = TEXT ("drm") ;
     
     return GetSaveFileName (&ofn) ;
}

TCHAR * DrumFileWrite (DRUM * pdrum, TCHAR * szFileName)
{
     char        szDateBuf [16] ;
     HMMIO       hmmio ;
     int         iFormat = 2 ;
     MMCKINFO    mmckinfo [3] ;
     SYSTEMTIME  st ;
     WORD        wError = 0 ;
     
     memset (mmckinfo, 0, 3 * sizeof (MMCKINFO)) ;
     
          // Recreate the file for writing
     
     if ((hmmio = mmioOpen (szFileName, NULL,
               MMIO_CREATE | MMIO_WRITE | MMIO_ALLOCBUF)) == NULL)
          return szErrorNoCreate ;
     
          // Create a "RIFF" chunk with a "CPDR" type
     
     mmckinfo[0].fccType = mmioStringToFOURCC (szDrumID, 0) ;
     
     wError |= mmioCreateChunk (hmmio, &mmckinfo[0], MMIO_CREATERIFF) ;
     
          // Create "LIST" sub-chunk with an "INFO" type
     
     mmckinfo[1].fccType = mmioStringToFOURCC (szInfoID, 0) ;
     
     wError |= mmioCreateChunk (hmmio, &mmckinfo[1], MMIO_CREATELIST) ;
     
          // Create "ISFT" sub-sub-chunk
     
     mmckinfo[2].ckid = mmioStringToFOURCC (szSoftID, 0) ;
     
     wError |= mmioCreateChunk (hmmio, &mmckinfo[2], 0) ;
     wError |= (mmioWrite (hmmio, szSoftware, sizeof (szSoftware)) !=
                                              sizeof (szSoftware)) ;
     wError |= mmioAscend (hmmio, &mmckinfo[2], 0) ;
     
          // Create a time string

     GetLocalTime (&st) ;
     
     wsprintfA (szDateBuf, "%04d-%02d-%02d", st.wYear, st.wMonth, st.wDay) ;
     
          // Create "ISCD" sub-sub-chunk
     
     mmckinfo[2].ckid = mmioStringToFOURCC (szDateID, 0) ;
     
     wError |= mmioCreateChunk (hmmio, &mmckinfo[2], 0) ;
     wError |= (mmioWrite (hmmio, szDateBuf, (strlen (szDateBuf) + 1)) !=
                                       (int) (strlen (szDateBuf) + 1)) ;
     wError |= mmioAscend (hmmio, &mmckinfo[2], 0) ;
     wError |= mmioAscend (hmmio, &mmckinfo[1], 0) ;
     
          // Create "fmt " sub-chunk
     
     mmckinfo[1].ckid = mmioStringToFOURCC (szFmtID, 0) ;
     
     wError |= mmioCreateChunk (hmmio, &mmckinfo[1], 0) ;
     wError |= (mmioWrite (hmmio, (PSTR) &iFormat, sizeof (int)) !=
                                                   sizeof (int)) ;
     wError |= mmioAscend (hmmio, &mmckinfo[1], 0) ;
     
          // Create the "data" sub-chunk
     
     mmckinfo[1].ckid = mmioStringToFOURCC (szDataID, 0) ;
     
     wError |= mmioCreateChunk (hmmio, &mmckinfo[1], 0) ;
     wError |= (mmioWrite (hmmio, (PSTR) pdrum, sizeof (DRUM)) !=
                                                sizeof (DRUM)) ;
     wError |= mmioAscend (hmmio, &mmckinfo[1], 0) ;
     wError |= mmioAscend (hmmio, &mmckinfo[0], 0) ;
     
          // Clean up and return
     
     wError |= mmioClose (hmmio, 0) ;
     
     if (wError)
     {
          mmioOpen (szFileName, NULL, MMIO_DELETE) ;
          return szErrorCannotWrite ;
     }
     return NULL ;
}

TCHAR * DrumFileRead (DRUM * pdrum, TCHAR * szFileName)
{
     DRUM     drum ;
     HMMIO    hmmio ;
     int      i, iFormat ;
     MMCKINFO mmckinfo [3] ;
     
     ZeroMemory (mmckinfo, 2 * sizeof (MMCKINFO)) ;
     
         // Open the file
     
     if ((hmmio = mmioOpen (szFileName, NULL, MMIO_READ)) == NULL)
          return szErrorNotFound ;
     
          // Locate a "RIFF" chunk with a "DRUM" form-type
     
     mmckinfo[0].ckid = mmioStringToFOURCC (szDrumID, 0) ;
     
     if (mmioDescend (hmmio, &mmckinfo[0], NULL, MMIO_FINDRIFF))
     {
          mmioClose (hmmio, 0) ;
          return szErrorNotDrum ;
     }
     
          // Locate, read, and verify the "fmt " sub-chunk
     
     mmckinfo[1].ckid = mmioStringToFOURCC (szFmtID, 0) ;
     
     if (mmioDescend (hmmio, &mmckinfo[1], &mmckinfo[0], MMIO_FINDCHUNK))
     {
          mmioClose (hmmio, 0) ;
          return szErrorNotDrum ;
     }
     
     if (mmckinfo[1].cksize != sizeof (int))
     {
          mmioClose (hmmio, 0) ;
          return szErrorUnsupported ;
     }
     
     if (mmioRead (hmmio, (PSTR) &iFormat, sizeof (int)) != sizeof (int))
     {
          mmioClose (hmmio, 0) ;
          return szErrorCannotRead ;
     }
     
     if (iFormat != 1 && iFormat != 2)
     {
          mmioClose (hmmio, 0) ;
          return szErrorUnsupported ;
     }
     
          // Go to end of "fmt " sub-chunk
     
     mmioAscend (hmmio, &mmckinfo[1], 0) ;
     
          // Locate, read, and verify the "data" sub-chunk
     
     mmckinfo[1].ckid = mmioStringToFOURCC (szDataID, 0) ;
  
     if (mmioDescend (hmmio, &mmckinfo[1], &mmckinfo[0], MMIO_FINDCHUNK))
     {
          mmioClose (hmmio, 0) ;
          return szErrorNotDrum ;
     }
     
     if (mmckinfo[1].cksize != sizeof (DRUM))
     {
          mmioClose (hmmio, 0) ;
          return szErrorUnsupported ;
     }
     
     if (mmioRead (hmmio, (LPSTR) &drum, sizeof (DRUM)) != sizeof (DRUM))
     {
          mmioClose (hmmio, 0) ;
          return szErrorCannotRead ;
     }
     
          // Close the file 
     
     mmioClose (hmmio, 0) ;

          // Convert format 1 to format 2 and copy the DRUM structure data

     if (iFormat == 1)
     {
          for (i = 0 ; i < NUM_PERC ; i++)
          {
               drum.dwSeqPerc [i] = drum.dwSeqPian [i] ;
               drum.dwSeqPian [i] = 0 ;
          }
     }
     
     memcpy (pdrum, &drum, sizeof (DRUM)) ;
     return NULL ;
}
