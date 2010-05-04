/*----------------------------------------
   DIBCONV.C -- Converts a DIB to a DDB
                (c) Charles Petzold, 1998
  ----------------------------------------*/

#include <windows.h>
#include <commdlg.h>
#include "resource.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName[] = TEXT ("DibConv") ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
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

     hwnd = CreateWindow (szAppName, TEXT ("DIB to DDB Conversion"),
                          WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT, 
                          NULL, NULL, hInstance, NULL) ;

     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;

     while (GetMessage (&msg, NULL, 0, 0))
     {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ;

     }
     return msg.wParam ;
}

HBITMAP CreateBitmapObjectFromDibFile (HDC hdc, PTSTR szFileName)
{
     BITMAPFILEHEADER * pbmfh ;
     BOOL               bSuccess ;
     DWORD              dwFileSize, dwHighSize, dwBytesRead ;
     HANDLE             hFile ;
     HBITMAP            hBitmap ;

          // Open the file: read access, prohibit write access

     hFile = CreateFile (szFileName, GENERIC_READ, FILE_SHARE_READ, NULL, 
                         OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL) ;

     if (hFile == INVALID_HANDLE_VALUE)
          return NULL ;

          // Read in the whole file

     dwFileSize = GetFileSize (hFile, &dwHighSize) ;

     if (dwHighSize)
     {
          CloseHandle (hFile) ;
          return NULL ;
     }

     pbmfh = malloc (dwFileSize) ;

     if (!pbmfh)
     {
          CloseHandle (hFile) ;
          return NULL ;
     }

     bSuccess = ReadFile (hFile, pbmfh, dwFileSize, &dwBytesRead, NULL) ;
     CloseHandle (hFile) ;

          // Verify the file

     if (!bSuccess || (dwBytesRead != dwFileSize)         
                   || (pbmfh->bfType != * (WORD *) "BM") 
                   || (pbmfh->bfSize != dwFileSize))
     {
          free (pbmfh) ;
          return NULL ;
     }

          // Create the DDB 

     hBitmap = CreateDIBitmap (hdc,              
                               (BITMAPINFOHEADER *) (pbmfh + 1),
                               CBM_INIT,
                               (BYTE *) pbmfh + pbmfh->bfOffBits,
                               (BITMAPINFO *) (pbmfh + 1),
                               DIB_RGB_COLORS) ;
     free (pbmfh) ;

     return hBitmap ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static HBITMAP      hBitmap ;
     static int          cxClient, cyClient ;
     static OPENFILENAME ofn ;
     static TCHAR        szFileName [MAX_PATH], szTitleName [MAX_PATH] ;
     static TCHAR        szFilter[] = TEXT ("Bitmap Files (*.BMP)\0*.bmp\0")
                                      TEXT ("All Files (*.*)\0*.*\0\0") ;
     BITMAP              bitmap ;
     HDC                 hdc, hdcMem ;
     PAINTSTRUCT         ps ;

     switch (message)
     {
     case WM_CREATE:
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
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;

     case WM_COMMAND:
          switch (LOWORD (wParam))
          {
          case IDM_FILE_OPEN:

                    // Show the File Open dialog box

               if (!GetOpenFileName (&ofn))
                    return 0 ;
               
                    // If there's an existing DIB, delete it

               if (hBitmap)
               {
                    DeleteObject (hBitmap) ;
                    hBitmap = NULL ;
               }
                    // Create the DDB from the DIB

               SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
               ShowCursor (TRUE) ;

               hdc = GetDC (hwnd) ;
               hBitmap = CreateBitmapObjectFromDibFile (hdc, szFileName) ;
               ReleaseDC (hwnd, hdc) ;

               ShowCursor (FALSE) ;
               SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

                    // Invalidate the client area for later update

               InvalidateRect (hwnd, NULL, TRUE) ;

               if (hBitmap == NULL)
               {
                    MessageBox (hwnd, TEXT ("Cannot load DIB file"), 
                                szAppName, MB_OK | MB_ICONEXCLAMATION) ;
               }
               return 0 ;
          }
          break ;

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;

          if (hBitmap)
          {
               GetObject (hBitmap, sizeof (BITMAP), &bitmap) ;

               hdcMem = CreateCompatibleDC (hdc) ;
               SelectObject (hdcMem, hBitmap) ;

               BitBlt (hdc,    0, 0, bitmap.bmWidth, bitmap.bmHeight, 
                       hdcMem, 0, 0, SRCCOPY) ;

               DeleteDC (hdcMem) ;
          }

          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          if (hBitmap)
               DeleteObject (hBitmap) ;

          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
