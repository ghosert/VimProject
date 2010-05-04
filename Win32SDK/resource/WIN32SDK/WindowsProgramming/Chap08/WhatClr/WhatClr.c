/*------------------------------------------
   WHATCLR.C -- Displays Color Under Cursor
                (c) Charles Petzold, 1998
  ------------------------------------------*/

#include <windows.h>

#define ID_TIMER    1

void FindWindowSize (int *, int *) ;
LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("WhatClr") ;
     HWND         hwnd ;
     int          cxWindow, cyWindow ;
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
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     FindWindowSize (&cxWindow, &cyWindow) ;
     
     hwnd = CreateWindow (szAppName, TEXT ("What Color"),
                          WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_BORDER,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          cxWindow, cyWindow,
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

void FindWindowSize (int * pcxWindow, int * pcyWindow)
{
     HDC        hdcScreen ;
     TEXTMETRIC tm ;
     
     hdcScreen = CreateIC (TEXT ("DISPLAY"), NULL, NULL, NULL) ;
     GetTextMetrics (hdcScreen, &tm) ;
     DeleteDC (hdcScreen) ;
     
     * pcxWindow = 2 * GetSystemMetrics (SM_CXBORDER)  + 
                        12 * tm.tmAveCharWidth ;

     * pcyWindow = 2 * GetSystemMetrics (SM_CYBORDER)  +
                       GetSystemMetrics (SM_CYCAPTION) + 
                         2 * tm.tmHeight ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static COLORREF cr, crLast ;
     static HDC      hdcScreen ;
     HDC             hdc ;
     PAINTSTRUCT     ps ;
     POINT           pt ;
     RECT            rc ;
     TCHAR           szBuffer [16] ;
     
     switch (message)
     {
     case WM_CREATE:
          hdcScreen = CreateDC (TEXT ("DISPLAY"), NULL, NULL, NULL) ;
          SetTimer (hwnd, ID_TIMER, 100, NULL) ;
          return 0 ;

     case WM_DISPLAYCHANGE:
          DeleteDC (hdcScreen) ;
          hdcScreen = CreateDC (TEXT ("DISPLAY"), NULL, NULL, NULL) ;
          return 0 ;

     case WM_TIMER:
          GetCursorPos (&pt) ;
          cr = GetPixel (hdcScreen, pt.x, pt.y) ;
          
          if (cr != crLast)
          {
               crLast = cr ;
               InvalidateRect (hwnd, NULL, FALSE) ;
          }
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          GetClientRect (hwnd, &rc) ;
          
          wsprintf (szBuffer, TEXT ("  %02X %02X %02X  "),
                    GetRValue (cr), GetGValue (cr), GetBValue (cr)) ;
          
          DrawText (hdc, szBuffer, -1, &rc,
                    DT_SINGLELINE | DT_CENTER | DT_VCENTER) ;
          
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          DeleteDC (hdcScreen) ;
          KillTimer (hwnd, ID_TIMER) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
