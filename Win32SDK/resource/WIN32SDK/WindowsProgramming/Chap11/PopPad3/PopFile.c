/*------------------------------------------
   POPFILE.C -- Popup Editor File Functions
  ------------------------------------------*/

#include <windows.h>
#include <commdlg.h>

static OPENFILENAME ofn ;

void PopFileInitialize (HWND hwnd)
{
     static TCHAR szFilter[] = TEXT ("Text Files (*.TXT)\0*.txt\0")  \
                               TEXT ("ASCII Files (*.ASC)\0*.asc\0") \
                               TEXT ("All Files (*.*)\0*.*\0\0") ;
     
     ofn.lStructSize       = sizeof (OPENFILENAME) ;
     ofn.hwndOwner         = hwnd ;
     ofn.hInstance         = NULL ;
     ofn.lpstrFilter       = szFilter ;
     ofn.lpstrCustomFilter = NULL ;
     ofn.nMaxCustFilter    = 0 ;
     ofn.nFilterIndex      = 0 ;
     ofn.lpstrFile         = NULL ;          // Set in Open and Close functions
     ofn.nMaxFile          = MAX_PATH ;
     ofn.lpstrFileTitle    = NULL ;          // Set in Open and Close functions
     ofn.nMaxFileTitle     = MAX_PATH ;
     ofn.lpstrInitialDir   = NULL ;
     ofn.lpstrTitle        = NULL ;
     ofn.Flags             = 0 ;             // Set in Open and Close functions
     ofn.nFileOffset       = 0 ;
     ofn.nFileExtension    = 0 ;
     ofn.lpstrDefExt       = TEXT ("txt") ;
     ofn.lCustData         = 0L ;
     ofn.lpfnHook          = NULL ;
     ofn.lpTemplateName    = NULL ;
}

BOOL PopFileOpenDlg (HWND hwnd, PTSTR pstrFileName, PTSTR pstrTitleName)
{
     ofn.hwndOwner         = hwnd ;
     ofn.lpstrFile         = pstrFileName ;
     ofn.lpstrFileTitle    = pstrTitleName ;
     ofn.Flags             = OFN_HIDEREADONLY | OFN_CREATEPROMPT ;
     
     return GetOpenFileName (&ofn) ;
}

BOOL PopFileSaveDlg (HWND hwnd, PTSTR pstrFileName, PTSTR pstrTitleName)
{
     ofn.hwndOwner         = hwnd ;
     ofn.lpstrFile         = pstrFileName ;
     ofn.lpstrFileTitle    = pstrTitleName ;
     ofn.Flags             = OFN_OVERWRITEPROMPT ;
     
     return GetSaveFileName (&ofn) ;
}

BOOL PopFileRead (HWND hwndEdit, PTSTR pstrFileName)
{
     BYTE   bySwap ;
     DWORD  dwBytesRead ;
     HANDLE hFile ;
     int    i, iFileLength, iUniTest ;
     PBYTE  pBuffer, pText, pConv ;

          // Open the file.

     if (INVALID_HANDLE_VALUE == 
               (hFile = CreateFile (pstrFileName, GENERIC_READ, FILE_SHARE_READ,
                                    NULL, OPEN_EXISTING, 0, NULL)))
          return FALSE ;

          // Get file size in bytes and allocate memory for read.
          // Add an extra two bytes for zero termination.
                    
     iFileLength = GetFileSize (hFile, NULL) ; 
     pBuffer = malloc (iFileLength + 2) ;

          // Read file and put terminating zeros at end.
     
     ReadFile (hFile, pBuffer, iFileLength, &dwBytesRead, NULL) ;
     CloseHandle (hFile) ;
     pBuffer[iFileLength] = '\0' ;
     pBuffer[iFileLength + 1] = '\0' ;

          // Test to see if the text is Unicode

     iUniTest = IS_TEXT_UNICODE_SIGNATURE | IS_TEXT_UNICODE_REVERSE_SIGNATURE ;
     
     if (IsTextUnicode (pBuffer, iFileLength, &iUniTest))
     {
          pText = pBuffer + 2 ;
          iFileLength -= 2 ;

          if (iUniTest & IS_TEXT_UNICODE_REVERSE_SIGNATURE)
          {
               for (i = 0 ; i < iFileLength / 2 ; i++)
               {
                    bySwap = ((BYTE *) pText) [2 * i] ;
                    ((BYTE *) pText) [2 * i] = ((BYTE *) pText) [2 * i + 1] ;
                    ((BYTE *) pText) [2 * i + 1] = bySwap ;
               }
          }

               // Allocate memory for possibly converted string

          pConv = malloc (iFileLength + 2) ;

               // If the edit control is not Unicode, convert Unicode text to 
               // non-Unicode (ie, in general, wide character).

#ifndef UNICODE
          WideCharToMultiByte (CP_ACP, 0, (PWSTR) pText, -1, pConv, 
                               iFileLength + 2, NULL, NULL) ;

               // If the edit control is Unicode, just copy the string
#else
          lstrcpy ((PTSTR) pConv, (PTSTR) pText) ;
#endif

     }
     else      // the file is not Unicode
     {
          pText = pBuffer ;

               // Allocate memory for possibly converted string.

          pConv = malloc (2 * iFileLength + 2) ;

               // If the edit control is Unicode, convert ASCII text.

#ifdef UNICODE
          MultiByteToWideChar (CP_ACP, 0, pText, -1, (PTSTR) pConv, 
                               iFileLength + 1) ;

               // If not, just copy buffer
#else
          lstrcpy ((PTSTR) pConv, (PTSTR) pText) ;
#endif
     }
     
     SetWindowText (hwndEdit, (PTSTR) pConv) ;
     free (pBuffer) ;
     free (pConv) ;
   
     return TRUE ;
}

BOOL PopFileWrite (HWND hwndEdit, PTSTR pstrFileName)
{
     DWORD  dwBytesWritten ;
     HANDLE hFile ;
     int    iLength ;
     PTSTR  pstrBuffer ;
     WORD   wByteOrderMark = 0xFEFF ;

          // Open the file, creating it if necessary
     
     if (INVALID_HANDLE_VALUE == 
               (hFile = CreateFile (pstrFileName, GENERIC_WRITE, 0, 
                                    NULL, CREATE_ALWAYS, 0, NULL)))
          return FALSE ;

          // Get the number of characters in the edit control and allocate
          // memory for them.
     
     iLength = GetWindowTextLength (hwndEdit) ;
     pstrBuffer = (PTSTR) malloc ((iLength + 1) * sizeof (TCHAR)) ;
     
     if (!pstrBuffer)
     {
          CloseHandle (hFile) ;
          return FALSE ;
     }

          // If the edit control will return Unicode text, write the
          // byte order mark to the file.

#ifdef UNICODE
     WriteFile (hFile, &wByteOrderMark, 2, &dwBytesWritten, NULL) ;
#endif

          // Get the edit buffer and write that out to the file.
     
     GetWindowText (hwndEdit, pstrBuffer, iLength + 1) ;
     WriteFile (hFile, pstrBuffer, iLength * sizeof (TCHAR), 
                &dwBytesWritten, NULL) ;
     
     if ((iLength * sizeof (TCHAR)) != (int) dwBytesWritten)
     {
          CloseHandle (hFile) ;
          free (pstrBuffer) ;
          return FALSE ;
     }
     
     CloseHandle (hFile) ;
     free (pstrBuffer) ;
     
     return TRUE ;
}
