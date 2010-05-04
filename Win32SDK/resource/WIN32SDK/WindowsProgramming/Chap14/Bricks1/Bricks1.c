/*----------------------------------------
   BRICKS1.C -- LoadBitmap Demonstration
                (c) Charles Petzold, 1998
  ----------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName [] = TEXT ("Bricks1") ;
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
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("LoadBitmap Demo"), 
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
     static HBITMAP hBitmap ;
     static int     cxClient, cyClient, cxSource, cySource ;
     BITMAP         bitmap ;
     HDC            hdc, hdcMem ;
     HINSTANCE      hInstance ;
     int            x, y ;
     PAINTSTRUCT    ps ;
     
     switch (message)
     {
     case WM_CREATE:
          hInstance = ((LPCREATESTRUCT) lParam)->hInstance ;

          hBitmap = LoadBitmap (hInstance, TEXT ("Bricks")) ;

          GetObject (hBitmap, sizeof (BITMAP), &bitmap) ;

          cxSource = bitmap.bmWidth ;
          cySource = bitmap.bmHeight ;

          return 0 ;

     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;

          hdcMem = CreateCompatibleDC (hdc) ;
          SelectObject (hdcMem, hBitmap) ;

          for (y = 0 ; y < cyClient ; y += cySource)
          for (x = 0 ; x < cxClient ; x += cxSource)
          {
               BitBlt (hdc, x, y, cxSource, cySource, hdcMem, 0, 0, SRCCOPY) ;
          }

          DeleteDC (hdcMem) ;
          EndPaint (hwnd, &ps) ;
          return 0 ;

     case WM_DESTROY:
          DeleteObject (hBitmap) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
