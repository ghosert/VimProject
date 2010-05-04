/*-----------------------------------------
   BEEPER1.C  -- Timer Demo Program No. 1
                 (c) Charles Petzold, 1998
  -----------------------------------------*/

#include <windows.h>

#define ID_TIMER    1

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("Beeper1") ;
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
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("Program requires Windows NT!"), 
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Beeper1 Timer Demo"),
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

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static BOOL fFlipFlop = FALSE ;
     HBRUSH      hBrush ;
     HDC         hdc ;
     PAINTSTRUCT ps ;
     RECT        rc ;
     
     switch (message)
     {
     case WM_CREATE:
          SetTimer (hwnd, ID_TIMER, 1000, NULL) ;
          return 0 ;

     case WM_TIMER :
          MessageBeep (-1) ;          
          fFlipFlop = !fFlipFlop ;
          InvalidateRect (hwnd, NULL, FALSE) ;
          return 0 ;
          
     case WM_PAINT :
          hdc = BeginPaint (hwnd, &ps) ;
          
          GetClientRect (hwnd, &rc) ;
          hBrush = CreateSolidBrush (fFlipFlop ? RGB(255,0,0) : RGB(0,0,255)) ;
          FillRect (hdc, &rc, hBrush) ;

          EndPaint (hwnd, &ps) ;
          DeleteObject (hBrush) ;
          return 0 ;
          
     case WM_DESTROY :
          KillTimer (hwnd, ID_TIMER) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
