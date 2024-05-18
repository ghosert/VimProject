/*---------------------------------------
   BLOWUP.C -- Video Magnifier Program
               (c) Charles Petzold, 1998
  ---------------------------------------*/

#include <windows.h>
#include <stdlib.h>      // for abs definition
#include "resource.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName [] = TEXT ("Blowup") ;
     HACCEL       hAccel ;
     HWND         hwnd ;
     MSG          msg ;
     WNDCLASS     wndclass ;

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
     
     hwnd = CreateWindow (szAppName, TEXT ("Blow-Up Mouse Demo"), 
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

void InvertBlock (HWND hwndScr, HWND hwnd, POINT ptBeg, POINT ptEnd)
{
     HDC hdc ;

     hdc = GetDCEx (hwndScr, NULL, DCX_CACHE | DCX_LOCKWINDOWUPDATE) ;
     ClientToScreen (hwnd, &ptBeg) ;
     ClientToScreen (hwnd, &ptEnd) ;
     PatBlt (hdc, ptBeg.x, ptBeg.y, ptEnd.x - ptBeg.x, ptEnd.y - ptBeg.y,
             DSTINVERT) ;
     ReleaseDC (hwndScr, hdc) ;
}

HBITMAP CopyBitmap (HBITMAP hBitmapSrc)
{
     BITMAP  bitmap ;
     HBITMAP hBitmapDst ;
     HDC     hdcSrc, hdcDst ;

     GetObject (hBitmapSrc, sizeof (BITMAP), &bitmap) ;
     hBitmapDst = CreateBitmapIndirect (&bitmap) ;

     hdcSrc = CreateCompatibleDC (NULL) ;
     hdcDst = CreateCompatibleDC (NULL) ;

     SelectObject (hdcSrc, hBitmapSrc) ;
     SelectObject (hdcDst, hBitmapDst) ;

     BitBlt (hdcDst, 0, 0, bitmap.bmWidth, bitmap.bmHeight,
             hdcSrc, 0, 0, SRCCOPY) ;

     DeleteDC (hdcSrc) ;
     DeleteDC (hdcDst) ;

     return hBitmapDst ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static BOOL    bCapturing, bBlocking ;
     static HBITMAP hBitmap ;
     static HWND    hwndScr ;
     static POINT   ptBeg, ptEnd ;
     BITMAP         bm ;
     HBITMAP        hBitmapClip ;
     HDC            hdc, hdcMem ;
     int            iEnable ;
     PAINTSTRUCT    ps ;
     RECT           rect ;

     switch (message)
     {
     case WM_LBUTTONDOWN:
          if (!bCapturing)
          {
               if (LockWindowUpdate (hwndScr = GetDesktopWindow ()))
               {
                    bCapturing = TRUE ;
                    SetCapture (hwnd) ;
                    SetCursor (LoadCursor (NULL, IDC_CROSS)) ;
               }
               else
                    MessageBeep (0) ;
          }
          return 0 ;

     case WM_RBUTTONDOWN:
          if (bCapturing)
          {
               bBlocking = TRUE ;
               ptBeg.x = LOWORD (lParam) ;
               ptBeg.y = HIWORD (lParam) ;
               ptEnd = ptBeg ;
               InvertBlock (hwndScr, hwnd, ptBeg, ptEnd) ;
          }
          return 0 ;

     case WM_MOUSEMOVE:
          if (bBlocking)
          {
               InvertBlock (hwndScr, hwnd, ptBeg, ptEnd) ;
               ptEnd.x = LOWORD (lParam) ;
               ptEnd.y = HIWORD (lParam) ;
               InvertBlock (hwndScr, hwnd, ptBeg, ptEnd) ;
          }
          return 0 ;

     case WM_LBUTTONUP:
     case WM_RBUTTONUP:
          if (bBlocking)
          {
               InvertBlock (hwndScr, hwnd, ptBeg, ptEnd) ;
               ptEnd.x = LOWORD (lParam) ;
               ptEnd.y = HIWORD (lParam) ;

               if (hBitmap)
               {
                    DeleteObject (hBitmap) ;
                    hBitmap = NULL ;
               }

               hdc = GetDC (hwnd) ;
               hdcMem = CreateCompatibleDC (hdc) ;
               hBitmap = CreateCompatibleBitmap (hdc, 
                                   abs (ptEnd.x - ptBeg.x),
                                   abs (ptEnd.y - ptBeg.y)) ;

               SelectObject (hdcMem, hBitmap) ;

               StretchBlt (hdcMem, 0, 0, abs (ptEnd.x - ptBeg.x),
                                         abs (ptEnd.y - ptBeg.y), 
                           hdc, ptBeg.x, ptBeg.y, ptEnd.x - ptBeg.x, 
                                                  ptEnd.y - ptBeg.y, SRCCOPY) ;

               DeleteDC (hdcMem) ;
               ReleaseDC (hwnd, hdc) ;
               InvalidateRect (hwnd, NULL, TRUE) ;
          }
          if (bBlocking || bCapturing)
          {
               bBlocking = bCapturing = FALSE ;
               SetCursor (LoadCursor (NULL, IDC_ARROW)) ;
               ReleaseCapture () ;
               LockWindowUpdate (NULL) ;
          }
          return 0 ;

     case WM_INITMENUPOPUP:
          iEnable = IsClipboardFormatAvailable (CF_BITMAP) ? 
                              MF_ENABLED : MF_GRAYED ;

          EnableMenuItem ((HMENU) wParam, IDM_EDIT_PASTE, iEnable) ;

          iEnable = hBitmap ? MF_ENABLED : MF_GRAYED ;

          EnableMenuItem ((HMENU) wParam, IDM_EDIT_CUT,    iEnable) ;
          EnableMenuItem ((HMENU) wParam, IDM_EDIT_COPY,   iEnable) ;
          EnableMenuItem ((HMENU) wParam, IDM_EDIT_DELETE, iEnable) ;
          return 0 ;

     case WM_COMMAND:
          switch (LOWORD (wParam))
          {
          case IDM_EDIT_CUT:
          case IDM_EDIT_COPY:
               if (hBitmap)
               {
                    hBitmapClip = CopyBitmap (hBitmap) ;
                    OpenClipboard (hwnd) ;
                    EmptyClipboard () ;
                    SetClipboardData (CF_BITMAP, hBitmapClip) ;
               }
               if (LOWORD (wParam) == IDM_EDIT_COPY)
                    return 0 ;
                                        // fall through for IDM_EDIT_CUT
          case IDM_EDIT_DELETE:
               if (hBitmap)
               {
                    DeleteObject (hBitmap) ;
                    hBitmap = NULL ;
               }
               InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;

          case IDM_EDIT_PASTE:
               if (hBitmap)
               {
                    DeleteObject (hBitmap) ;
                    hBitmap = NULL ;
               }
               OpenClipboard (hwnd) ;
               hBitmapClip = GetClipboardData (CF_BITMAP) ;

               if (hBitmapClip)
                    hBitmap = CopyBitmap (hBitmapClip) ;

               CloseClipboard () ;
               InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;
          }
          break ;

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;

          if (hBitmap)
          {
               GetClientRect (hwnd, &rect) ;

               hdcMem = CreateCompatibleDC (hdc) ;
               SelectObject (hdcMem, hBitmap) ;
               GetObject (hBitmap, sizeof (BITMAP), (PSTR) &bm) ;
               SetStretchBltMode (hdc, COLORONCOLOR) ;

               StretchBlt (hdc,    0, 0, rect.right, rect.bottom,
                           hdcMem, 0, 0, bm.bmWidth, bm.bmHeight, SRCCOPY) ;

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
