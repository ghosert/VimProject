/*-------------------------------------------------
   CHECKER4.C -- Mouse Hit-Test Demo Program No. 4
                 (c) Charles Petzold, 1998

  -------------------------------------------------*/

#include <windows.h>
#define DIVISIONS 5

LRESULT CALLBACK WndProc   (HWND, UINT, WPARAM, LPARAM) ;
LRESULT CALLBACK ChildWndProc (HWND, UINT, WPARAM, LPARAM) ;

int   idFocus = 0 ;
TCHAR szChildClass[] = TEXT ("Checker4_Child") ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("Checker4") ;
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
     
     // Redefine the child wndProc and lpszClassName, cbWndExtra
     wndclass.lpfnWndProc   = ChildWndProc ;
     // This field is set to a sizeof long, and will be used by SetWindowLong(hwnd, 0, 0) & GetWindowLong(hwnd, 0) below.
     wndclass.cbWndExtra    = sizeof (long) ;
     wndclass.hIcon         = NULL ;
     wndclass.lpszClassName = szChildClass ;
     
     RegisterClass (&wndclass) ;
     
     // CreateWindow is using szAppName means it will invoke the WnProc of wndclass which has the same value(szAppName) for lpszClassName field.
     hwnd = CreateWindow (szAppName, TEXT ("Checker4 Mouse Hit-Test Demo"),
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
     static HWND hwndChild[DIVISIONS][DIVISIONS] ;
     int         cxBlock, cyBlock, x, y ;
     
     switch (message)
     {
     case WM_CREATE :
          for (x = 0 ; x < DIVISIONS ; x++)
               for (y = 0 ; y < DIVISIONS ; y++)
     // CreateWindow is using szChildClass means it will invoke the ChildWnProc of wndclass which has the same value(szChildClass) for lpszClassName field.
                    hwndChild[x][y] = CreateWindow (szChildClass, NULL,
                              WS_CHILDWINDOW | WS_VISIBLE, // WS_CHILDWINDOW for a child window
                              0, 0, 0, 0, // left-top and width-heigth are set to 0 while they will be reset by MoveWindow below.
                              hwnd, (HMENU) (y << 8 | x), // set the parent hwnd and child window id. store x, y information to the id.
                              // Set the instance which is the same to the first parameter of WinMain method.
                              // You can also use a global variable to save instance and set it here.
                              (HINSTANCE) GetWindowLong (hwnd, GWL_HINSTANCE), 
                              NULL) ;
          return 0 ;
               
     case WM_SIZE :
          cxBlock = LOWORD (lParam) / DIVISIONS ;
          cyBlock = HIWORD (lParam) / DIVISIONS ;
          
          for (x = 0 ; x < DIVISIONS ; x++)
                for (y = 0 ; y < DIVISIONS ; y++)
                	// Move a window with position and size defined.
                    MoveWindow (hwndChild[x][y],
                                x * cxBlock, y * cyBlock,
                                cxBlock, cyBlock, TRUE) ;
          return 0 ;
                       
     case WM_LBUTTONDOWN :
          MessageBeep (0) ;
          return 0 ;

          // On set-focus message, set focus to child window
          
          
     // Only WM_SETFOCUS will be received by parent window first not child window, so the message should be passed to child window here.
     // And other messages like WM_LBUTTONDOWN or WM_KEYDOWN will be received by child window first not parent window, so these message should be 
     // passed to parent window proc if necessary, see these messages in ChildWndProc below.
     case WM_SETFOCUS:
    	 // GetDlgItem will return hwnd of child window by using parent hwnd and child window id which is defined by (HMENU) (y << 8 | x) above.
    	 // the child window which is on focus will be draw a dash rectangle.
          SetFocus (GetDlgItem (hwnd, idFocus)) ;
          return 0 ;

          // On key-down message, possibly change the focus window

     case WM_KEYDOWN:
          x = idFocus & 0xFF ;
          y = idFocus >> 8 ;

          switch (wParam)
          {
          case VK_UP:    y-- ;                    break ;
          case VK_DOWN:  y++ ;                    break ;
          case VK_LEFT:  x-- ;                    break ;
          case VK_RIGHT: x++ ;                    break ;
          case VK_HOME:  x = y = 0 ;              break ;
          case VK_END:   x = y = DIVISIONS - 1 ;  break ;
          default:       return 0 ;
          }

          x = (x + DIVISIONS) % DIVISIONS ;
          y = (y + DIVISIONS) % DIVISIONS ;

          idFocus = y << 8 | x ;

          SetFocus (GetDlgItem (hwnd, idFocus)) ;
          return 0 ;

     case WM_DESTROY :
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

LRESULT CALLBACK ChildWndProc (HWND hwnd, UINT message, 
                               WPARAM wParam, LPARAM lParam)
{
     HDC         hdc ;
     PAINTSTRUCT ps ;
     RECT        rect ;
     
     switch (message)
     {
     case WM_CREATE :
    	 // Set cbWndExtra field(has been allocated with sizeof(long) above) of wndclass struct with a 0 value. means it should not be draw a X signal.
          SetWindowLong (hwnd, 0, 0) ;       // on/off flag
          return 0 ;

     case WM_KEYDOWN:
               // Send most key presses to the parent window
          
          if (wParam != VK_RETURN && wParam != VK_SPACE)
          {
               SendMessage (GetParent (hwnd), message, wParam, lParam) ;
               return 0 ;
          }
               // For Return and Space, fall through to toggle the square
          
     case WM_LBUTTONDOWN :
    	 // Get the value of cbWndExtra field(has been allocated with sizeof(long) above) of wndclass struct first.
    	 // Set cbWndExtra with a 1^x value 1^x. x here will be 0 or 1 means only x=1 will make 1^x 0 or it will be 1.
    	 // means whether to draw a X signal.
          SetWindowLong (hwnd, 0, 1 ^ GetWindowLong (hwnd, 0)) ;
          SetFocus (hwnd) ;
          InvalidateRect (hwnd, NULL, FALSE) ;
          return 0 ;

               // For focus messages, invalidate the window for repaint
          
     case WM_SETFOCUS:
    	 // Get child window id here just like GetDlgItem (hwnd, idFocus) to get the child hwnd.
          idFocus = GetWindowLong (hwnd, GWL_ID) ;

               // Fall through

     case WM_KILLFOCUS:
          InvalidateRect (hwnd, NULL, TRUE) ;
          return 0 ;
          
     case WM_PAINT :
          hdc = BeginPaint (hwnd, &ps) ;
          
          GetClientRect (hwnd, &rect) ;
          Rectangle (hdc, 0, 0, rect.right, rect.bottom) ;

               // Draw the "x" mark
          
          if (GetWindowLong (hwnd, 0))
          {
               MoveToEx (hdc, 0,          0, NULL) ;
               LineTo   (hdc, rect.right, rect.bottom) ;
               MoveToEx (hdc, 0,          rect.bottom, NULL) ;
               LineTo   (hdc, rect.right, 0) ;
          }

               // Draw the "focus" rectangle
          
          if (hwnd == GetFocus ())
          {
               rect.left   += rect.right / 10 ;
               rect.right  -= rect.left ;
               rect.top    += rect.bottom / 10 ;
               rect.bottom -= rect.top ;

               SelectObject (hdc, GetStockObject (NULL_BRUSH)) ;
               SelectObject (hdc, CreatePen (PS_DASH, 0, 0)) ;
               Rectangle (hdc, rect.left, rect.top, rect.right, rect.bottom) ;
               DeleteObject (SelectObject (hdc, GetStockObject (BLACK_PEN))) ;
          }

          EndPaint (hwnd, &ps) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

