/*-------------------------------------------
   BITMASK.C -- Bitmap Masking Demonstration
                (c) Charles Petzold, 1998
  -------------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName [] = TEXT ("BitMask") ;
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
     wndclass.hbrBackground = (HBRUSH) GetStockObject (LTGRAY_BRUSH) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Bitmap Masking Demo"), 
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
     static HBITMAP   hBitmapImag, hBitmapMask ;
     static HINSTANCE hInstance ;
     static int       cxClient, cyClient, cxBitmap, cyBitmap ;
     BITMAP           bitmap ;
     HDC              hdc, hdcMemImag, hdcMemMask ;
     int              x, y ;
     PAINTSTRUCT      ps ;
     
     switch (message)
     {
     case WM_CREATE:
          hInstance = ((LPCREATESTRUCT) lParam)->hInstance ;

               // Load the original image and get its size

          hBitmapImag = LoadBitmap (hInstance, TEXT ("Matthew")) ;
          GetObject (hBitmapImag, sizeof (BITMAP), &bitmap) ;
          cxBitmap = bitmap.bmWidth ;
          cyBitmap = bitmap.bmHeight ;

               // Select the original image into a memory DC

          hdcMemImag  = CreateCompatibleDC (NULL) ;
          SelectObject (hdcMemImag, hBitmapImag) ;

               // Create the monochrome mask bitmap and memory DC

          hBitmapMask = CreateBitmap (cxBitmap, cyBitmap, 1, 1, NULL) ;
          hdcMemMask = CreateCompatibleDC (NULL) ;
          SelectObject (hdcMemMask, hBitmapMask) ;

               // Color the mask bitmap black with a white ellipse
          
          SelectObject (hdcMemMask, GetStockObject (BLACK_BRUSH)) ;
          Rectangle (hdcMemMask, 0, 0, cxBitmap, cyBitmap) ;
          SelectObject (hdcMemMask, GetStockObject (WHITE_BRUSH)) ;
          Ellipse (hdcMemMask, 0, 0, cxBitmap, cyBitmap) ;

               // Mask the original image

          BitBlt (hdcMemImag, 0, 0, cxBitmap, cyBitmap, 
                  hdcMemMask, 0, 0, SRCAND) ;

          DeleteDC (hdcMemImag) ;
          DeleteDC (hdcMemMask) ;
          return 0 ;

     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;

               // Select bitmaps into memory DCs

          hdcMemImag = CreateCompatibleDC (hdc) ;
          SelectObject (hdcMemImag, hBitmapImag) ;

          hdcMemMask = CreateCompatibleDC (hdc) ;
          SelectObject (hdcMemMask, hBitmapMask) ;

               // Center image

          x = (cxClient - cxBitmap) / 2 ;
          y = (cyClient - cyBitmap) / 2 ;
               
               // Do the bitblts

          BitBlt (hdc, x, y, cxBitmap, cyBitmap, hdcMemMask, 0, 0, 0x220326) ;
          BitBlt (hdc, x, y, cxBitmap, cyBitmap, hdcMemImag, 0, 0, SRCPAINT) ;

          DeleteDC (hdcMemImag) ;
          DeleteDC (hdcMemMask) ;
          EndPaint (hwnd, &ps) ;
          return 0 ;

     case WM_DESTROY:
          DeleteObject (hBitmapImag) ;
          DeleteObject (hBitmapMask) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
