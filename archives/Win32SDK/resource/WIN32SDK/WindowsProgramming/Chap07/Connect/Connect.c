/*--------------------------------------------------
   CONNECT.C -- Connect-the-Dots Mouse Demo Program
                (c) Charles Petzold, 1998
  --------------------------------------------------*/

#include <windows.h>

#define MAXPOINTS 1000

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("Connect") ;
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
     
     hwnd = CreateWindow (szAppName, TEXT ("Connect-the-Points Mouse Demo"),
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
     static POINT pt[MAXPOINTS] ;
     static int   iCount ;
     HDC          hdc ;
     int          i, j ;
     PAINTSTRUCT  ps ;

     switch (message)
     {
     case WM_LBUTTONDOWN:
          iCount = 0 ;
          InvalidateRect (hwnd, NULL, TRUE) ;
          return 0 ;
          
     case WM_MOUSEMOVE:
          if (wParam & MK_LBUTTON && iCount < 1000)
          {
               pt[iCount  ].x = LOWORD (lParam) ;
               pt[iCount++].y = HIWORD (lParam) ;
               
               hdc = GetDC (hwnd) ;
               SetPixel (hdc, LOWORD (lParam), HIWORD (lParam), 0) ;
               ReleaseDC (hwnd, hdc) ;
          }
          return 0 ;
          
     case WM_LBUTTONUP:
          InvalidateRect (hwnd, NULL, FALSE) ;
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
          ShowCursor (TRUE) ;
          
          for (i = 0 ; i < iCount - 1 ; i++)
               for (j = i + 1 ; j < iCount ; j++)
               {
                    MoveToEx (hdc, pt[i].x, pt[i].y, NULL) ;
                    LineTo   (hdc, pt[j].x, pt[j].y) ;
               }
               
          ShowCursor (FALSE) ;
          SetCursor (LoadCursor (NULL, IDC_ARROW)) ;
               
          EndPaint (hwnd, &ps) ;
          return 0 ;
               
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
