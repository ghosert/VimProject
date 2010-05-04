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
     
     wndclass.style         = CS_HREDRAW | CS_VREDRAW ; // add CS_DBLCLKS for getting WM_XBUTTONDBLCLK message.
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
     // If there is a mouse exits.
     // BOOL fMouse = GetSystemMetrics(SM_MOUSEPRESENT);
     // The number of button on the mouse.
     // int cButtons = GetSystemMetrics(SM_CMOUSEBUTTONS);
     
     // And WndProc will get NON-CLIENT MOUSE message while mouse point to NON-CLIENT area like title or border.
     // They are WM_NCLBUTTONDOWN WM_NCXXXXXXX, also for these messages, lParam will be used to get x, y value, but they are screen coordinate.
     // Use ScreenToClient or ClientToScreen to translate the coordinate.
     // Gennerally, you don't have to handle WM_NCxxxxxx message, for they could be handled by DefWindowProc like: maximize a window or move a window.
     
     // If you want to get double click message like WM_LBUTTONDBLCLK, you should add class style CS_DBLCLKS above.
     case WM_LBUTTONDOWN:
          iCount = 0 ;
          InvalidateRect (hwnd, NULL, TRUE) ;
          return 0 ;
          
     case WM_MOUSEMOVE:
    	 // Use wParam to get the information about which button has been clicked and the state of shift and ctrl key.
    	 // MK_LBUTTON MK_MBUTTON MK_RBUTTON are mostly used for WM_MOUSEMOVE while MK_SHIFT MK_CONTROL are mostly used for WM_XBUTTONDOWN
          if (wParam & MK_LBUTTON && iCount < 1000)
          {
        	  // For the MOUSE message, lParam is used to get the x, y value.
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
          
          // IDC_*** means the style of mouse
          SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
          // RULE1: If there is no mouse installed, the default value in ShowCursor equals to -1 otherwise it equals to 0
          // RULE2: If the default value is >= 0, the cursor will be shown.
          // RULE3: ShowCursor(TRUE) will +1 for the default value while ShowCursor(FALSE) will -1 for the default value.
          // So If there is no mouse here, it will show cursor anyway.
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

