/*----------------------------------------
   COLORS1.C -- Colors Using Scroll Bars
                (c) Charles Petzold, 1998
  ----------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc    (HWND, UINT, WPARAM, LPARAM) ;
LRESULT CALLBACK ScrollProc (HWND, UINT, WPARAM, LPARAM) ;

int     idFocus ;
WNDPROC OldScroll[3] ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("Colors1") ;
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
     wndclass.hbrBackground = CreateSolidBrush (0) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }

     hwnd = CreateWindow (szAppName, TEXT ("Color Scroll"),
                          WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL, NULL, hInstance, NULL) ;
     
     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;
     
     while (GetMessage (&msg, NULL, 0, 0))
     {
          TranslateMessage (&msg) ;
          DispatchMessage  (&msg) ;
     }
     return msg.wParam ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static COLORREF crPrim[3] = { RGB (255, 0, 0), RGB (0, 255, 0),
                                   RGB (0, 0, 255) } ;
     static HBRUSH  hBrush[3], hBrushStatic ;
     static HWND    hwndScroll[3], hwndLabel[3], hwndValue[3], hwndRect ;
     static int     color[3], cyChar ;
     static RECT    rcColor ;
     static TCHAR * szColorLabel[] = { TEXT ("Red"), TEXT ("Green"), 
                                       TEXT ("Blue") } ;
     HINSTANCE      hInstance ;
     int            i, cxClient, cyClient ;
     TCHAR          szBuffer[10] ;
     
     switch (message)
     {
     case WM_CREATE :
          hInstance = (HINSTANCE) GetWindowLong (hwnd, GWL_HINSTANCE) ;
          
               // Create the white-rectangle window against which the 
               // scroll bars will be positioned. The child window ID is 9.
          
          hwndRect = CreateWindow (TEXT ("static"), NULL, // create a static child window which never receive the mouse or keyboard messages.
        		                                          // And will never send parent window a WM_COMMAND message.
                                   WS_CHILD | WS_VISIBLE | SS_WHITERECT, // static style is SS_****
                                   0, 0, 0, 0,
                                   hwnd, (HMENU) 9, hInstance, NULL) ;
          
          for (i = 0 ; i < 3 ; i++)
          {
                    // The three scroll bars have IDs 0, 1, and 2, with
                    // scroll bar ranges from 0 through 255.
               
               hwndScroll[i] = CreateWindow (TEXT ("scrollbar"), NULL, // create a scrollbar child window
                                             WS_CHILD | WS_VISIBLE | 
                                             // WS_TABSTOP means get focus when the mouse click on scrollbar.
                                             WS_TABSTOP | SBS_VERT, // SBS_VERT & SBS_HORZ style. please notice any other SBS_*** sytles.
                                             0, 0, 0, 0,  // you can control the position and the width, heigth for scroll child window here.
                                             hwnd, (HMENU) i, hInstance, NULL) ;
               
               SetScrollRange (hwndScroll[i], SB_CTL, 0, 255, FALSE) ; // for window scroll bar it is SetScrollRange(hwnd, SB_VERT/SB_HORZ, ...)
               SetScrollPos   (hwndScroll[i], SB_CTL, 0, FALSE) ; // for window scroll bar it is SetScrollPos(hwnd, SB_VERT/SB_HORZ, ...)
               
                    // The three color-name labels have IDs 3, 4, and 5, 
                    // and text strings "Red", "Green", and "Blue".
               
               hwndLabel [i] = CreateWindow (TEXT ("static"), szColorLabel[i],
                                             WS_CHILD | WS_VISIBLE | SS_CENTER, // SS_LEFT, SS_RIGHT, SS_CENTER
                                             0, 0, 0, 0, 
                                             hwnd, (HMENU) (i + 3), 
                                             hInstance, NULL) ;
               
                    // The three color-value text fields have IDs 6, 7, 
                    // and 8, and initial text strings of "0".
               
               hwndValue [i] = CreateWindow (TEXT ("static"), TEXT ("0"), // Set 0 text in static, but can be changed with SetWindowText api later.
                                             WS_CHILD | WS_VISIBLE | SS_CENTER,
                                             0, 0, 0, 0,
                                             hwnd, (HMENU) (i + 6), 
                                             hInstance, NULL) ;
               
               // Set a new winproc for scroll and return oldproc which will be called by CallWindowProc later.
               OldScroll[i] = (WNDPROC) SetWindowLong (hwndScroll[i], 
                                             GWL_WNDPROC, (LONG) ScrollProc) ;
               
               hBrush[i] = CreateSolidBrush (crPrim[i]) ;
          }
          
          hBrushStatic = CreateSolidBrush (
                              GetSysColor (COLOR_BTNHIGHLIGHT)) ;
          
          cyChar = HIWORD (GetDialogBaseUnits ()) ;
          return 0 ;
          
     case WM_SIZE :
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;

          SetRect (&rcColor, cxClient / 2, 0, cxClient, cyClient) ;
          
          MoveWindow (hwndRect, 0, 0, cxClient / 2, cyClient, TRUE) ;
          
          for (i = 0 ; i < 3 ; i++)
          {
               MoveWindow (hwndScroll[i], // you can control the width and heigth for scroll child window here.
            		                      // if you want to get the same style with window scroll you can use GetSystemMetrics(SM_CYHSCROLL/SM_CXVSCROLL)
                           (2 * i + 1) * cxClient / 14, 2 * cyChar,
                           cxClient / 14, cyClient - 4 * cyChar, TRUE) ;
               
               MoveWindow (hwndLabel[i],
                           (4 * i + 1) * cxClient / 28, cyChar / 2,
                           cxClient / 7, cyChar, TRUE) ;
               
               MoveWindow (hwndValue[i],
                           (4 * i + 1) * cxClient / 28, 
                           cyClient - 3 * cyChar / 2,
                           cxClient / 7, cyChar, TRUE) ;
          }
          SetFocus (hwnd) ;
          return 0 ;
          
     case WM_SETFOCUS : // pass the focus to child window always for using keyboard to control scroll child window.
          SetFocus (hwndScroll[idFocus]) ;
          return 0 ;
          
          // unlike button child window, scrollbar child window will send parent window WM_VSCROLL & WM_HSCROLL message instead of WM_COMMAND
          // scrollbar child window here just like the window bars which is added by WS_VSCROLL and WS_HSCROLL
     case WM_VSCROLL :
    	 // Use lParam here to distinguish the window scroll and scroll child window.
          i = GetWindowLong ((HWND) lParam, GWL_ID) ; // (HWND) lParam means hwnd for child window. And i here is the child window id.
          
          switch (LOWORD (wParam))
          {
          case SB_PAGEDOWN :
               color[i] += 15 ;
                                             // fall through
          case SB_LINEDOWN :
               color[i] = min (255, color[i] + 1) ;
               break ;
               
          case SB_PAGEUP :
               color[i] -= 15 ;
                                             // fall through
          case SB_LINEUP :
               color[i] = max (0, color[i] - 1) ;
               break ;
               
          case SB_TOP :
               color[i] = 0 ;
               break ;
               
          case SB_BOTTOM :
               color[i] = 255 ;
               break ;
               
          case SB_THUMBPOSITION :
          case SB_THUMBTRACK :
               color[i] = HIWORD (wParam) ;
               break ;
               
          default :
               break ;
          }
          SetScrollPos  (hwndScroll[i], SB_CTL, color[i], TRUE) ;
          wsprintf (szBuffer, TEXT ("%i"), color[i]) ; // a way to convert the int to char.
          SetWindowText (hwndValue[i], szBuffer) ;
          
          // Change the wndclass.hbrbackground with new brush and delete the old one. new one will be applied when redrawing.
          DeleteObject ((HBRUSH) 
               SetClassLong (hwnd, GCL_HBRBACKGROUND, (LONG) 
                    CreateSolidBrush (RGB (color[0], color[1], color[2])))) ;
          
          InvalidateRect (hwnd, &rcColor, TRUE) ; // redraw the right side of static area with new brush.
          return 0 ;
          
          // other similar message is WM_CTLCOLORXXXXX, see WM_CTLCOLORSTATIC below.
     case WM_CTLCOLORSCROLLBAR :
          i = GetWindowLong ((HWND) lParam, GWL_ID) ; // change the brush.
          return (LRESULT) hBrush[i] ;
               
     case WM_CTLCOLORSTATIC : // here is a chance to set text color and background color for static. And return a brush.
          i = GetWindowLong ((HWND) lParam, GWL_ID) ;
               
          if (i >= 3 && i <= 8)    // static text controls
          {
               SetTextColor ((HDC) wParam, crPrim[i % 3]) ;
               SetBkColor ((HDC) wParam, GetSysColor (COLOR_BTNHIGHLIGHT));
               return (LRESULT) hBrushStatic ;
          }
          break ;
               
     case WM_SYSCOLORCHANGE :
          DeleteObject (hBrushStatic) ;
          hBrushStatic = CreateSolidBrush (GetSysColor (COLOR_BTNHIGHLIGHT)) ;
          return 0 ;

     case WM_DESTROY :
          DeleteObject ((HBRUSH)
               SetClassLong (hwnd, GCL_HBRBACKGROUND, (LONG) 
                    GetStockObject (WHITE_BRUSH))) ; // delete the old brush and set a system brush.
               
          for (i = 0 ; i < 3 ; i++)
               DeleteObject (hBrush[i]) ;
               
          DeleteObject (hBrushStatic) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
     
LRESULT CALLBACK ScrollProc (HWND hwnd, UINT message, 
                             WPARAM wParam, LPARAM lParam)
{
     int id = GetWindowLong (hwnd, GWL_ID) ;
          
     switch (message)
     {
     // scrollbar child window will only concern the left, right, up, down key by defalt, so we need ScrollProc here to support VK_TAB and VK_SHIFT
     // it's one of the hook technology we are using.
     case WM_KEYDOWN : // Allow the scroll can handle the keyboard message. VK_TAB for next scrollbar while VK_SHIFT + VK_TAB for previous scrollbar.
          if (wParam == VK_TAB)
               SetFocus (GetDlgItem (GetParent (hwnd), 
                    (id + (GetKeyState (VK_SHIFT) < 0 ? 2 : 1)) % 3)) ;
          break ;
               
     case WM_SETFOCUS :
          idFocus = id ;
          break ;
     }
     return CallWindowProc (OldScroll[id], hwnd, message, wParam, lParam) ; // continue the old proc although we are using the new winproc now.
}

