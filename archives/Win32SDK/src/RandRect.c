/*------------------------------------------
   RANDRECT.C -- Displays Random Rectangles
                 (c) Charles Petzold, 1998
  ------------------------------------------*/

#include <windows.h>
#include <stdlib.h>           // for the rand function

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;
void DrawRectangle (HWND) ;

int cxClient, cyClient ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("RandRect") ;
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
     
     hwnd = CreateWindow (szAppName, TEXT ("Random Rectangles"),
                          WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL, NULL, hInstance, NULL) ;
     
     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;
     
     while (TRUE)
     {
    	 // Not like GetMessage will return 0 if WM_QUIT message has been got.
    	 // PeekMessage will return 1 if there is a message, or return 0 if there is no message, return 0 means we can take the time to do our job.
    	 // In this case GetMessage will only wait until the next message comes.
          if (PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
          {
        	  // As we discussed above, we should handle the WM_QUIT message by ourself to quit while circlement
        	  // for PeekMessage will not handle it automaticly while GetMessage will.
               if (msg.message == WM_QUIT)
                    break ;
               TranslateMessage (&msg) ;
               DispatchMessage (&msg) ;
          }
          else
               DrawRectangle (hwnd) ;
     }
     return msg.wParam ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
     switch (iMsg)
     {
     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, iMsg, wParam, lParam) ;
}

void DrawRectangle (HWND hwnd)
{
     HBRUSH hBrush ;
     HDC    hdc ;
     RECT   rect ;
     
     if (cxClient == 0 || cyClient == 0)
          return ;
     
     SetRect (&rect, rand () % cxClient, rand () % cyClient,
                     rand () % cxClient, rand () % cyClient) ; // Api to set the RECT
     // Other apis about rect.
     // OffsetRect(&rect, x, y);
     // InflateRect(&rect, x, y); expand the rect.
     // SetRectEmpty(&rect); set 0 value to the points in rect.
     // CopyRect(&DestRect, &SrcRect); equals to DestRect = SrcRect
     // IntersectRect(&DestRect, &SrcRect1, &SrcRect2); get the intersect rect to DestRect from srcrect1 and srcrect2
     // UnionRect(&DestRect, &SrcRect1, &SrcRect2)
     // IsRectEmpty(&rect);
     // PtInRect(&rect, point); whether point is in rect.
     
     hBrush = CreateSolidBrush (
                    RGB (rand () % 256, rand () % 256, rand () % 256)) ;
     hdc = GetDC (hwnd) ;
     
 // fill a rect just like ploygon api, but this one doesn't need SelectObject api invoking
 // because the third parameter is allocated as a brush already.
     FillRect (hdc, &rect, hBrush) ; 
     // FrameRect(hdc, &rect, hBrush); // use hBrush to draw a rect without filling internal area. Remember it use brush not like Rectangle use pen
     // InvertRect(hdc, &rect); // invert the color of the rect.
     ReleaseDC (hwnd, hdc) ;
     DeleteObject (hBrush) ;
}     

