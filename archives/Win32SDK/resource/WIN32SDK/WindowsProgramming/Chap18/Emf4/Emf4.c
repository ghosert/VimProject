/*-------------------------------------
   EMF4.C -- Enhanced Metafile Demo #4
             (c) Charles Petzold, 1998
  -------------------------------------*/

#define OEMRESOURCE
#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("EMF4") ;
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
     wndclass.hbrBackground = GetStockObject (WHITE_BRUSH) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Enhanced Metafile Demo #4"),
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
     BITMAP       bm ;
     HBITMAP      hbm ;
     HDC          hdc, hdcEMF, hdcMem ;
     HENHMETAFILE hemf ;
     PAINTSTRUCT  ps ;
     RECT         rect ;
     
     switch (message)
     {
     case WM_CREATE:
          hdcEMF = CreateEnhMetaFile (NULL, TEXT ("emf4.emf"), NULL,
                                      TEXT ("EMF4\0EMF Demo #4\0")) ;
          
          hbm = LoadBitmap (NULL, MAKEINTRESOURCE (OBM_CLOSE)) ;
          
          GetObject (hbm, sizeof (BITMAP), &bm) ;

          hdcMem = CreateCompatibleDC (hdcEMF) ;
          
          SelectObject (hdcMem, hbm) ;
          
          StretchBlt (hdcEMF, 100, 100, 100, 100,
                      hdcMem,   0,   0, bm.bmWidth, bm.bmHeight, SRCCOPY) ;
          
          DeleteDC (hdcMem) ;
          DeleteObject (hbm) ;
          
          hemf = CloseEnhMetaFile (hdcEMF) ;
          
          DeleteEnhMetaFile (hemf) ;
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          GetClientRect (hwnd, &rect) ;
          
          rect.left   =     rect.right  / 4 ;
          rect.right  = 3 * rect.right  / 4 ;
          rect.top    =     rect.bottom / 4 ;
          rect.bottom = 3 * rect.bottom / 4 ;
          
          hemf = GetEnhMetaFile (TEXT ("emf4.emf")) ;
          
          PlayEnhMetaFile (hdc, hemf, &rect) ;
          DeleteEnhMetaFile (hemf) ;
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
