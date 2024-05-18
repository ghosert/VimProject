#include <windows.h>
#include <math.h>

#define TWO_PI (2.0 * 3.14159)

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("Clover") ;
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
     
     hwnd = CreateWindow (szAppName, TEXT ("Draw a Clover"),
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

LRESULT CALLBACK WndProc (HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
     static HRGN hRgnClip ;
     static int  cxClient, cyClient ;
     double      fAngle, fRadius ;
     HCURSOR     hCursor ;
     HDC         hdc ;
     HRGN        hRgnTemp[6] ; // HRGN just like RECT is a self-defined area.
     int         i ;
     PAINTSTRUCT ps ;
     
     switch (iMsg)
     {
     // The task of WM_SIZE case is used to create a self-defined region as valid client area. 
     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;

          hCursor = SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
          ShowCursor (TRUE) ;
          
          if (hRgnClip)
               DeleteObject (hRgnClip) ; // delete old region before create a new one.
          
          // The apis below are used to create RGN just like RECT
          // other apis including CreateRoundRectRgn & CreatePolygonRgn etc..
          // These apis will create region and be ready to be selected, after that the valid client area
          // will be changed to the region created instead of tranditional rect area.
          hRgnTemp[0] = CreateEllipticRgn (0, cyClient / 3,
                                           cxClient / 2, 2 * cyClient / 3) ;
          hRgnTemp[1] = CreateEllipticRgn (cxClient / 2, cyClient / 3,
                                           cxClient, 2 * cyClient / 3) ;
          hRgnTemp[2] = CreateEllipticRgn (cxClient / 3, 0,
                                           2 * cxClient / 3, cyClient / 2) ;
          hRgnTemp[3] = CreateEllipticRgn (cxClient / 3, cyClient / 2,
                                           2 * cxClient / 3, cyClient) ;
          hRgnTemp[4] = CreateRectRgn (0, 0, 1, 1) ;
          hRgnTemp[5] = CreateRectRgn (0, 0, 1, 1) ;
          hRgnClip    = CreateRectRgn (0, 0, 1, 1) ;

          // combie region from the second and the third regions to the first region with the fourth style.
          // and the first region should be initialized with a small region.
          // RGN_AND: inter sect
          // RGN_OR: srcRgn1 || srcRgn2
          // RGN_XOR: !RGN_AND
          // RGN_DIFF: the part belong to srcRgn1 but not belong to srcRgn2
          // RGN_COPY: srcRgn1 and ignore srcRgn2 totally.
          // Return value: NULLREGION; SIMPLEREGION; COMPLEXREGION; ERROR
          CombineRgn (hRgnTemp[4], hRgnTemp[0], hRgnTemp[1], RGN_OR) ;
          CombineRgn (hRgnTemp[5], hRgnTemp[2], hRgnTemp[3], RGN_OR) ;
          CombineRgn (hRgnClip,    hRgnTemp[4], hRgnTemp[5], RGN_XOR) ;
          
          for (i = 0 ; i < 6 ; i++)
               DeleteObject (hRgnTemp[i]) ;
          
          SetCursor (hCursor) ;
          ShowCursor (FALSE) ;
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          // draw on region api
          // FillRgn, FrameRgn, InvertRgn, PaintRgn like FillRect, FrameRect ... etc.
          HBRUSH hBrush = CreateSolidBrush(RGB(255, 0, 0));
          FillRgn(hdc, hRgnClip, hBrush);
          DeleteObject(hBrush);
          
          // set the center position as the origin.
          SetViewportOrgEx (hdc, cxClient / 2, cyClient / 2, NULL) ;
          
          // select self-defined region as valid client area.
          // so the painting will only happen in self-defined region.
          SelectClipRgn (hdc, hRgnClip) ; // equals to SelectObject(hdc, hRgnClip);
          
          // Use InvalidateRgn and ValidRgn instead of InvalidRect and ValidRect.
          
          fRadius = _hypot (cxClient / 2.0, cyClient / 2.0) ;
          
          for (fAngle = 0.0 ; fAngle < TWO_PI ; fAngle += TWO_PI / 360)
          {
               MoveToEx (hdc, 0, 0, NULL) ;
               LineTo (hdc, (int) ( fRadius * cos (fAngle) + 0.5),
                            (int) (-fRadius * sin (fAngle) + 0.5)) ;
          }
          EndPaint (hwnd, &ps) ;
          return 0 ;

     case WM_DESTROY:
          DeleteObject (hRgnClip) ; // destroy region
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, iMsg, wParam, lParam) ;
}

