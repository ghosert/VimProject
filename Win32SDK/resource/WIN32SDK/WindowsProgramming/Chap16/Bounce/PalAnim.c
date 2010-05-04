/*----------------------------------------------
   PALANIM.C -- Palette Animation Shell Program
                (c) Charles Petzold, 1998
  ----------------------------------------------*/

#include <windows.h>

extern HPALETTE CreateRoutine  (HWND) ;
extern void     PaintRoutine   (HDC, int, int) ;
extern void     TimerRoutine   (HDC, HPALETTE) ;
extern void     DestroyRoutine (HWND, HPALETTE) ;

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

extern TCHAR szAppName [] ;
extern TCHAR szTitle [] ;

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
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, szTitle, 
                          WS_OVERLAPPEDWINDOW, 
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL, NULL, hInstance, NULL) ;

     if (!hwnd)
          return 0 ;

     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;

     while (GetMessage (&msg, NULL, 0, 0))
     {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ;
     }
     return msg.wParam ;
}

BOOL CheckDisplay (HWND hwnd)
{
     HDC hdc ;
     int iPalSize ;

     hdc = GetDC (hwnd) ;
     iPalSize = GetDeviceCaps (hdc, SIZEPALETTE) ;
     ReleaseDC (hwnd, hdc) ;

     if (iPalSize != 256)
     {
          MessageBox (hwnd, TEXT ("This program requires that the video ")
                            TEXT ("display mode have a 256-color palette."),
                      szAppName, MB_ICONERROR) ;
          return FALSE ;
     }
     return TRUE ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static HPALETTE hPalette ;
     static int      cxClient, cyClient ;
     HDC             hdc ;
     PAINTSTRUCT     ps ;

     switch (message)
     {
     case WM_CREATE:
          if (!CheckDisplay (hwnd))
               return -1 ;

          hPalette = CreateRoutine (hwnd) ;
          return 0 ;
     
     case WM_DISPLAYCHANGE:
          if (!CheckDisplay (hwnd))
               DestroyWindow (hwnd) ;

          return 0 ;

     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;

          SelectPalette (hdc, hPalette, FALSE) ;
          RealizePalette (hdc) ;

          PaintRoutine (hdc, cxClient, cyClient) ;

          EndPaint (hwnd, &ps) ;
          return 0 ;

     case WM_TIMER:
          hdc = GetDC (hwnd) ;

          SelectPalette (hdc, hPalette, FALSE) ;

          TimerRoutine (hdc, hPalette) ;

          ReleaseDC (hwnd, hdc) ;
          return 0 ;

     case WM_QUERYNEWPALETTE:
          if (!hPalette)
               return FALSE ;

          hdc = GetDC (hwnd) ;
          SelectPalette (hdc, hPalette, FALSE) ;
          RealizePalette (hdc) ;
          InvalidateRect (hwnd, NULL, TRUE) ;

          ReleaseDC (hwnd, hdc) ;
          return TRUE ;

     case WM_PALETTECHANGED:
          if (!hPalette || (HWND) wParam == hwnd)
               break ;

          hdc = GetDC (hwnd) ;
          SelectPalette (hdc, hPalette, FALSE) ;
          RealizePalette (hdc) ;
          UpdateColors (hdc) ;

          ReleaseDC (hwnd, hdc) ;
          break ;

     case WM_DESTROY:
          DestroyRoutine (hwnd, hPalette) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
