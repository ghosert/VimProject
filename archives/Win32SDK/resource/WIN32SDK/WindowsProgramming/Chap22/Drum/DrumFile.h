/*-------------------------------------------------------
   DRUMFILE.H Header File for File I/O Routines for DRUM
  -------------------------------------------------------*/

BOOL    DrumFileOpenDlg (HWND, TCHAR *, TCHAR *) ;
BOOL    DrumFileSaveDlg (HWND, TCHAR *, TCHAR *) ;

TCHAR * DrumFileWrite   (DRUM *, TCHAR *) ;
TCHAR * DrumFileRead    (DRUM *, TCHAR *) ;
