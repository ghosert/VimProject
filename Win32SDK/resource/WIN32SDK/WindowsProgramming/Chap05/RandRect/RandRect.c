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
          if (PeekMessage (&msg, NULL, 0, 0, PM_REMOVE))
          {
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
                     rand () % cxClient, rand () % cyClient) ;
     
     hBrush = CreateSolidBrush (
                    RGB (rand () % 256, rand () % 256, rand () % 256)) ;

     hdc = GetDC (hwnd) ;
     FillRect (hdc, &rect, hBrush) ;
     ReleaseDC (hwnd, hdc) ;
     DeleteObject (hBrush) ;
}     
