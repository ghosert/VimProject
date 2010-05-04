/*----------------------------------------------
   METAFILE.C -- Metafile Demonstration Program
                 (c) Charles Petzold, 1998
  ----------------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName [] = TEXT ("Metafile") ;
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
     
     hwnd = CreateWindow (szAppName, TEXT ("Metafile Demonstration"),
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
     static HMETAFILE hmf ;
     static int       cxClient, cyClient ;
     HBRUSH           hBrush ;
     HDC              hdc, hdcMeta ;
     int              x, y ;
     PAINTSTRUCT      ps ;
     
     switch (message)
     {
     case WM_CREATE:
          hdcMeta = CreateMetaFile (NULL) ;
          hBrush  = CreateSolidBrush (RGB (0, 0, 255)) ;
          
          Rectangle (hdcMeta, 0, 0, 100, 100) ;
          
          MoveToEx (hdcMeta,   0,   0, NULL) ;
          LineTo   (hdcMeta, 100, 100) ;
          MoveToEx (hdcMeta,   0, 100, NULL) ;
          LineTo   (hdcMeta, 100,   0) ;
          
          SelectObject (hdcMeta, hBrush) ;
          Ellipse (hdcMeta, 20, 20, 80, 80) ;
          
          hmf = CloseMetaFile (hdcMeta) ;
          
          DeleteObject (hBrush) ;
          return 0 ;
          
     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          SetMapMode (hdc, MM_ANISOTROPIC) ;
          SetWindowExtEx (hdc, 1000, 1000, NULL) ;
          SetViewportExtEx (hdc, cxClient, cyClient, NULL) ;
          
          for (x = 0 ; x < 10 ; x++)
          for (y = 0 ; y < 10 ; y++)
          {
               SetWindowOrgEx (hdc, -100 * x, -100 * y, NULL) ;
               PlayMetaFile (hdc, hmf) ;
          }
          EndPaint (hwnd, &ps) ;
          return 0 ;
               
     case WM_DESTROY:
          DeleteMetaFile (hmf) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
