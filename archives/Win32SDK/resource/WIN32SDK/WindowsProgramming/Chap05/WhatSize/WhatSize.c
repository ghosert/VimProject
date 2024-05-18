/*-----------------------------------------
   WHATSIZE.C -- What Size is the Window?
                 (c) Charles Petzold, 1998
  -----------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("WhatSize") ;
     HWND         hwnd ;
     MSG          msg ;
     WNDCLASS     wndclass ;
     
     wndclass.style         = CS_HREDRAW | CS_VREDRAW;
     wndclass.lpfnWndProc   = WndProc ;
     wndclass.cbClsExtra    = 0 ;
     wndclass.cbWndExtra    = 0 ;
     wndclass.hInstance     = hInstance ;
     wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
     wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
     wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("What Size is the Window?"),
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

void Show (HWND hwnd, HDC hdc, int xText, int yText, int iMapMode,
           TCHAR * szMapMode)
{
     TCHAR szBuffer [60] ;
     RECT  rect ;
     
     SaveDC (hdc) ;
     
     SetMapMode (hdc, iMapMode) ;
     GetClientRect (hwnd, &rect) ;
     DPtoLP (hdc, (PPOINT) &rect, 2) ;
     
     RestoreDC (hdc, -1) ;
     
     TextOut (hdc, xText, yText, szBuffer,
              wsprintf (szBuffer, TEXT ("%-20s %7d %7d %7d %7d"), szMapMode,
              rect.left, rect.right, rect.top, rect.bottom)) ;
}  

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static TCHAR szHeading [] =
          TEXT ("Mapping Mode            Left   Right     Top  Bottom") ;
     static TCHAR szUndLine [] = 
          TEXT ("------------            ----   -----     ---  ------") ;
     static int   cxChar, cyChar ;
     HDC          hdc ;
     PAINTSTRUCT  ps ;
     TEXTMETRIC   tm ;
     
     switch (message)
     {
     case WM_CREATE:
          hdc = GetDC (hwnd) ;
          SelectObject (hdc, GetStockObject (SYSTEM_FIXED_FONT)) ;
          
          GetTextMetrics (hdc, &tm) ;
          cxChar = tm.tmAveCharWidth ;
          cyChar = tm.tmHeight + tm.tmExternalLeading ;
          
          ReleaseDC (hwnd, hdc) ;
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          SelectObject (hdc, GetStockObject (SYSTEM_FIXED_FONT)) ;
          
          SetMapMode (hdc, MM_ANISOTROPIC) ;
          SetWindowExtEx (hdc, 1, 1, NULL) ;
          SetViewportExtEx (hdc, cxChar, cyChar, NULL) ;
          
          TextOut (hdc, 1, 1, szHeading, lstrlen (szHeading)) ;
          TextOut (hdc, 1, 2, szUndLine, lstrlen (szUndLine)) ;
          
          Show (hwnd, hdc, 1, 3, MM_TEXT,      TEXT ("TEXT (pixels)")) ;
          Show (hwnd, hdc, 1, 4, MM_LOMETRIC,  TEXT ("LOMETRIC (.1 mm)")) ;
          Show (hwnd, hdc, 1, 5, MM_HIMETRIC,  TEXT ("HIMETRIC (.01 mm)")) ;
          Show (hwnd, hdc, 1, 6, MM_LOENGLISH, TEXT ("LOENGLISH (.01 in)")) ;
          Show (hwnd, hdc, 1, 7, MM_HIENGLISH, TEXT ("HIENGLISH (.001 in)")) ;
          Show (hwnd, hdc, 1, 8, MM_TWIPS,     TEXT ("TWIPS (1/1440 in)")) ;
          
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
