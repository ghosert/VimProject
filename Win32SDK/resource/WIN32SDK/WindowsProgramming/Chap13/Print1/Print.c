/*-----------------------------------------------------------
   PRINT.C -- Common routines for Print1, Print2, and Print3
  -----------------------------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;
BOOL PrintMyPage (HWND) ;

extern HINSTANCE hInst ;
extern TCHAR     szAppName[] ;
extern TCHAR     szCaption[] ;

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
     
     hInst = hInstance ;
     
     hwnd = CreateWindow (szAppName, szCaption,
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

void PageGDICalls (HDC hdcPrn, int cxPage, int cyPage)
{
     static TCHAR szTextStr[] = TEXT ("Hello, Printer!") ;
     
     Rectangle (hdcPrn, 0, 0, cxPage, cyPage) ;
     
     MoveToEx (hdcPrn, 0, 0, NULL) ;
     LineTo   (hdcPrn, cxPage, cyPage) ;
     MoveToEx (hdcPrn, cxPage, 0, NULL) ;
     LineTo   (hdcPrn, 0, cyPage) ;
     
     SaveDC (hdcPrn) ;
     
     SetMapMode       (hdcPrn, MM_ISOTROPIC) ;
     SetWindowExtEx   (hdcPrn, 1000, 1000, NULL) ;
     SetViewportExtEx (hdcPrn, cxPage / 2, -cyPage / 2, NULL) ;
     SetViewportOrgEx (hdcPrn, cxPage / 2,  cyPage / 2, NULL) ;
     
     Ellipse (hdcPrn, -500, 500, 500, -500) ;
     
     SetTextAlign (hdcPrn, TA_BASELINE | TA_CENTER) ;
     TextOut (hdcPrn, 0, 0, szTextStr, lstrlen (szTextStr)) ;

     RestoreDC (hdcPrn, -1) ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static int   cxClient, cyClient ;
     HDC          hdc ;
     HMENU        hMenu ;
     PAINTSTRUCT  ps ;
     
     switch (message)
     {
     case WM_CREATE:
          hMenu = GetSystemMenu (hwnd, FALSE) ;
          AppendMenu (hMenu, MF_SEPARATOR, 0, NULL) ;
          AppendMenu (hMenu, 0, 1, TEXT ("&Print")) ;
          return 0 ;
          
     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;
          
     case WM_SYSCOMMAND:
          if (wParam == 1)
          {
               if (!PrintMyPage (hwnd))
                    MessageBox (hwnd, TEXT ("Could not print page!"),
                                szAppName, MB_OK | MB_ICONEXCLAMATION) ;
               return 0 ;
          }
          break ;
          
     case WM_PAINT :
          hdc = BeginPaint (hwnd, &ps) ;
          
          PageGDICalls (hdc, cxClient, cyClient) ;
          
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY :
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
