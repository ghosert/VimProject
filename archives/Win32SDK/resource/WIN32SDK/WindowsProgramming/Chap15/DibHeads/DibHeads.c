/*-----------------------------------------------
   DIBHEADS.C -- Displays DIB Header Information
                 (c) Charles Petzold, 1998
  -----------------------------------------------*/

#define WINVER 0x0500
#include <windows.h>
#include "resource.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName[] = TEXT ("DibHeads") ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     HACCEL   hAccel ;
     HWND     hwnd ;
     MSG      msg ;
     WNDCLASS wndclass ;

     wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
     wndclass.lpfnWndProc   = WndProc ;
     wndclass.cbClsExtra    = 0 ;
     wndclass.cbWndExtra    = 0 ;
     wndclass.hInstance     = hInstance ;
     wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
     wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
     wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH) ;
     wndclass.lpszMenuName  = szAppName ;
     wndclass.lpszClassName = szAppName ;

     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"), 
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("DIB Headers"),
                          WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT, 
                          NULL, NULL, hInstance, NULL) ;
     
     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;

     hAccel = LoadAccelerators (hInstance, szAppName) ;
     
     while (GetMessage (&msg, NULL, 0, 0))
     {
          if (!TranslateAccelerator (hwnd, hAccel, &msg))
          {
               TranslateMessage (&msg) ;
               DispatchMessage (&msg) ;
          }
     }
     return msg.wParam ;
}

void Printf (HWND hwnd, TCHAR * szFormat, ...)
{
     TCHAR   szBuffer [1024] ;
     va_list pArgList ;

     va_start (pArgList, szFormat) ;
     wvsprintf (szBuffer, szFormat, pArgList) ;
     va_end (pArgList) ;

     SendMessage (hwnd, EM_SETSEL, (WPARAM) -1, (LPARAM) -1) ;
     SendMessage (hwnd, EM_REPLACESEL, FALSE, (LPARAM) szBuffer) ;
     SendMessage (hwnd, EM_SCROLLCARET, 0, 0) ;
}

void DisplayDibHeaders (HWND hwnd, TCHAR * szFileName)
{
     static TCHAR     * szInfoName [] = { TEXT ("BITMAPCOREHEADER"), 
                                          TEXT ("BITMAPINFOHEADER"),
                                          TEXT ("BITMAPV4HEADER"),
                                          TEXT ("BITMAPV5HEADER") } ;
     static TCHAR     * szCompression [] = { TEXT ("BI_RGB"), TEXT ("BI_RLE8"),
                                             TEXT ("BI_RLE4"),
                                             TEXT ("BI_BITFIELDS"),
                                             TEXT ("unknown") } ;
     BITMAPCOREHEADER * pbmch ;
     BITMAPFILEHEADER * pbmfh ;
     BITMAPV5HEADER   * pbmih ;
     BOOL               bSuccess ;
     DWORD              dwFileSize, dwHighSize, dwBytesRead ;
     HANDLE             hFile ;
     int                i ;
     PBYTE              pFile ;
     TCHAR            * szV ;

          // Display the file name

     Printf (hwnd, TEXT ("File: %s\r\n\r\n"), szFileName) ;

          // Open the file

     hFile = CreateFile (szFileName, GENERIC_READ, FILE_SHARE_READ, NULL,
                         OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL) ;

     if (hFile == INVALID_HANDLE_VALUE)
     {
          Printf (hwnd, TEXT ("Cannot open file.\r\n\r\n")) ;
          return ;
     }
          
          // Get the size of the file

     dwFileSize = GetFileSize (hFile, &dwHighSize) ;

     if (dwHighSize)
     {
          Printf (hwnd, TEXT ("Cannot deal with >4G files.\r\n\r\n")) ;
          CloseHandle (hFile) ;
          return ;
     }

          // Allocate memory for the file

     pFile = malloc (dwFileSize) ;

     if (!pFile)
     {
          Printf (hwnd, TEXT ("Cannot allocate memory.\r\n\r\n")) ;
          CloseHandle (hFile) ;
          return ;
     }

          // Read the file

     SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
     ShowCursor (TRUE) ;

     bSuccess = ReadFile (hFile, pFile, dwFileSize, &dwBytesRead, NULL) ;

     ShowCursor (FALSE) ;
     SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

     if (!bSuccess || (dwBytesRead != dwFileSize))
     {
          Printf (hwnd, TEXT ("Could not read file.\r\n\r\n")) ;
          CloseHandle (hFile) ;
          free (pFile) ;
          return ;
     }

          // Close the file

     CloseHandle (hFile) ;

          // Display file size

     Printf (hwnd, TEXT ("File size = %u bytes\r\n\r\n"), dwFileSize) ;

          // Display BITMAPFILEHEADER structure

     pbmfh = (BITMAPFILEHEADER *) pFile ;

     Printf (hwnd, TEXT ("BITMAPFILEHEADER\r\n")) ;
     Printf (hwnd, TEXT ("\t.bfType = 0x%X\r\n"), pbmfh->bfType) ;
     Printf (hwnd, TEXT ("\t.bfSize = %u\r\n"), pbmfh->bfSize) ;
     Printf (hwnd, TEXT ("\t.bfReserved1 = %u\r\n"), pbmfh->bfReserved1) ;
     Printf (hwnd, TEXT ("\t.bfReserved2 = %u\r\n"), pbmfh->bfReserved2) ;
     Printf (hwnd, TEXT ("\t.bfOffBits = %u\r\n\r\n"), pbmfh->bfOffBits) ;

          // Determine which information structure we have

     pbmih = (BITMAPV5HEADER *) (pFile + sizeof (BITMAPFILEHEADER)) ;

     switch (pbmih->bV5Size)
     {
     case sizeof (BITMAPCOREHEADER):  i = 0 ;                       break ;
     case sizeof (BITMAPINFOHEADER):  i = 1 ;  szV = TEXT ("i")  ;  break ;
     case sizeof (BITMAPV4HEADER):    i = 2 ;  szV = TEXT ("V4") ;  break ;
     case sizeof (BITMAPV5HEADER):    i = 3 ;  szV = TEXT ("V5") ;  break ;
     default:
          Printf (hwnd, TEXT ("Unknown header size of %u.\r\n\r\n"), 
                        pbmih->bV5Size) ;
          free (pFile) ;
          return ;
     }

     Printf (hwnd, TEXT ("%s\r\n"), szInfoName[i]) ;

          // Display the BITMAPCOREHEADER fields

     if (pbmih->bV5Size == sizeof (BITMAPCOREHEADER))
     {
          pbmch = (BITMAPCOREHEADER *) pbmih ;

          Printf (hwnd, TEXT ("\t.bcSize = %u\r\n"), pbmch->bcSize) ;
          Printf (hwnd, TEXT ("\t.bcWidth = %u\r\n"), pbmch->bcWidth) ;
          Printf (hwnd, TEXT ("\t.bcHeight = %u\r\n"), pbmch->bcHeight) ;
          Printf (hwnd, TEXT ("\t.bcPlanes = %u\r\n"), pbmch->bcPlanes) ;
          Printf (hwnd, TEXT ("\t.bcBitCount = %u\r\n\r\n"), pbmch->bcBitCount);
          free (pFile) ;
          return ;
     }

          // Display the BITMAPINFOHEADER fields

     Printf (hwnd, TEXT ("\t.b%sSize = %u\r\n"), szV, pbmih->bV5Size) ;
     Printf (hwnd, TEXT ("\t.b%sWidth = %i\r\n"), szV, pbmih->bV5Width);
     Printf (hwnd, TEXT ("\t.b%sHeight = %i\r\n"), szV, pbmih->bV5Height) ;
     Printf (hwnd, TEXT ("\t.b%sPlanes = %u\r\n"), szV, pbmih->bV5Planes) ;
     Printf (hwnd, TEXT ("\t.b%sBitCount = %u\r\n"), szV, pbmih->bV5BitCount) ;
     Printf (hwnd, TEXT ("\t.b%sCompression = %s\r\n"), szV, 
                   szCompression [min (4, pbmih->bV5Compression)]) ;

     Printf (hwnd, TEXT ("\t.b%sSizeImage = %u\r\n"), szV, pbmih->bV5SizeImage);
     Printf (hwnd, TEXT ("\t.b%sXPelsPerMeter = %i\r\n"), szV, 
                   pbmih->bV5XPelsPerMeter) ;
     Printf (hwnd, TEXT ("\t.b%sYPelsPerMeter = %i\r\n"), szV, 
                   pbmih->bV5YPelsPerMeter) ;
     Printf (hwnd, TEXT ("\t.b%sClrUsed = %i\r\n"), szV, pbmih->bV5ClrUsed) ;
     Printf (hwnd, TEXT ("\t.b%sClrImportant = %i\r\n\r\n"), szV, 
                   pbmih->bV5ClrImportant) ;

     if (pbmih->bV5Size == sizeof (BITMAPINFOHEADER))
     {
          if (pbmih->bV5Compression == BI_BITFIELDS)
          {
               Printf (hwnd, TEXT ("Red Mask   = %08X\r\n"), 
                             pbmih->bV5RedMask) ;
               Printf (hwnd, TEXT ("Green Mask = %08X\r\n"), 
                             pbmih->bV5GreenMask) ;
               Printf (hwnd, TEXT ("Blue Mask  = %08X\r\n\r\n"), 
                             pbmih->bV5BlueMask) ;
          }
          free (pFile) ;
          return ;
     }

          // Display additional BITMAPV4HEADER fields

     Printf (hwnd, TEXT ("\t.b%sRedMask   = %08X\r\n"), szV, 
                   pbmih->bV5RedMask) ;
     Printf (hwnd, TEXT ("\t.b%sGreenMask = %08X\r\n"), szV, 
                   pbmih->bV5GreenMask) ;
     Printf (hwnd, TEXT ("\t.b%sBlueMask  = %08X\r\n"), szV, 
                   pbmih->bV5BlueMask) ;
     Printf (hwnd, TEXT ("\t.b%sAlphaMask = %08X\r\n"), szV, 
                   pbmih->bV5AlphaMask) ;
     Printf (hwnd, TEXT ("\t.b%sCSType = %u\r\n"), szV, 
                   pbmih->bV5CSType) ;
     Printf (hwnd, TEXT ("\t.b%sEndpoints.ciexyzRed.ciexyzX   = %08X\r\n"), szV, 
                   pbmih->bV5Endpoints.ciexyzRed.ciexyzX) ;
     Printf (hwnd, TEXT ("\t.b%sEndpoints.ciexyzRed.ciexyzY   = %08X\r\n"), szV, 
                   pbmih->bV5Endpoints.ciexyzRed.ciexyzY) ;
     Printf (hwnd, TEXT ("\t.b%sEndpoints.ciexyzRed.ciexyzZ   = %08X\r\n"), szV, 
                   pbmih->bV5Endpoints.ciexyzRed.ciexyzZ) ;
     Printf (hwnd, TEXT ("\t.b%sEndpoints.ciexyzGreen.ciexyzX = %08X\r\n"), szV, 
                   pbmih->bV5Endpoints.ciexyzGreen.ciexyzX) ;
     Printf (hwnd, TEXT ("\t.b%sEndpoints.ciexyzGreen.ciexyzY = %08X\r\n"), szV, 
                   pbmih->bV5Endpoints.ciexyzGreen.ciexyzY) ;
     Printf (hwnd, TEXT ("\t.b%sEndpoints.ciexyzGreen.ciexyzZ = %08X\r\n"), szV, 
                   pbmih->bV5Endpoints.ciexyzGreen.ciexyzZ) ;
     Printf (hwnd, TEXT ("\t.b%sEndpoints.ciexyzBlue.ciexyzX  = %08X\r\n"), szV,
                   pbmih->bV5Endpoints.ciexyzBlue.ciexyzX) ;
     Printf (hwnd, TEXT ("\t.b%sEndpoints.ciexyzBlue.ciexyzY  = %08X\r\n"), szV, 
                   pbmih->bV5Endpoints.ciexyzBlue.ciexyzY) ;
     Printf (hwnd, TEXT ("\t.b%sEndpoints.ciexyzBlue.ciexyzZ  = %08X\r\n"), szV, 
                   pbmih->bV5Endpoints.ciexyzBlue.ciexyzZ) ;
     Printf (hwnd, TEXT ("\t.b%sGammaRed   = %08X\r\n"), szV, 
                   pbmih->bV5GammaRed) ;
     Printf (hwnd, TEXT ("\t.b%sGammaGreen = %08X\r\n"), szV, 
                   pbmih->bV5GammaGreen) ;
     Printf (hwnd, TEXT ("\t.b%sGammaBlue  = %08X\r\n\r\n"), szV, 
                   pbmih->bV5GammaBlue) ;

     if (pbmih->bV5Size == sizeof (BITMAPV4HEADER))
     {
          free (pFile) ;
          return ;
     }

          // Display additional BITMAPV5HEADER fields

     Printf (hwnd, TEXT ("\t.b%sIntent = %u\r\n"), szV, pbmih->bV5Intent) ;
     Printf (hwnd, TEXT ("\t.b%sProfileData = %u\r\n"), szV, 
                   pbmih->bV5ProfileData) ;
     Printf (hwnd, TEXT ("\t.b%sProfileSize = %u\r\n"), szV, 
                   pbmih->bV5ProfileSize) ;
     Printf (hwnd, TEXT ("\t.b%sReserved = %u\r\n\r\n"), szV, 
                   pbmih->bV5Reserved) ;

     free (pFile) ;
     return ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static HWND         hwndEdit ;
     static OPENFILENAME ofn ;
     static TCHAR        szFileName [MAX_PATH], szTitleName [MAX_PATH] ;
     static TCHAR        szFilter[] = TEXT ("Bitmap Files (*.BMP)\0*.bmp\0")
                                      TEXT ("All Files (*.*)\0*.*\0\0") ;
     
     switch (message)
     {
     case WM_CREATE:
          hwndEdit = CreateWindow (TEXT ("edit"), NULL,
                         WS_CHILD | WS_VISIBLE | WS_BORDER | 
                              WS_VSCROLL | WS_HSCROLL |
                              ES_MULTILINE | ES_AUTOVSCROLL | ES_READONLY, 
                         0, 0, 0, 0, hwnd, (HMENU) 1,
                         ((LPCREATESTRUCT) lParam)->hInstance, NULL) ;

          ofn.lStructSize       = sizeof (OPENFILENAME) ;
          ofn.hwndOwner         = hwnd ;
          ofn.hInstance         = NULL ;
          ofn.lpstrFilter       = szFilter ;
          ofn.lpstrCustomFilter = NULL ;
          ofn.nMaxCustFilter    = 0 ;
          ofn.nFilterIndex      = 0 ;
          ofn.lpstrFile         = szFileName ;
          ofn.nMaxFile          = MAX_PATH ;
          ofn.lpstrFileTitle    = szTitleName ;
          ofn.nMaxFileTitle     = MAX_PATH ;
          ofn.lpstrInitialDir   = NULL ;
          ofn.lpstrTitle        = NULL ;
          ofn.Flags             = 0 ;
          ofn.nFileOffset       = 0 ;
          ofn.nFileExtension    = 0 ;
          ofn.lpstrDefExt       = TEXT ("bmp") ;
          ofn.lCustData         = 0 ;
          ofn.lpfnHook          = NULL ;
          ofn.lpTemplateName    = NULL ;
          return 0 ;

     case WM_SIZE:
          MoveWindow (hwndEdit, 0, 0, LOWORD (lParam), HIWORD (lParam), TRUE) ;
          return 0 ;

     case WM_COMMAND:
          switch (LOWORD (wParam))
          {
          case IDM_FILE_OPEN:
               if (GetOpenFileName (&ofn))
                    DisplayDibHeaders (hwndEdit, szFileName) ;

               return 0 ;
          }
          break ;
     
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
