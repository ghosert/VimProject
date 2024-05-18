/*----------------------------------------
   BTNLOOK.C -- Button Look Program
                (c) Charles Petzold, 1998
  ----------------------------------------*/

#include <windows.h>

struct
{
     int     iStyle ;
     TCHAR * szText ;
}
button[] =
{
     BS_PUSHBUTTON,      TEXT ("PUSHBUTTON"),
     BS_DEFPUSHBUTTON,   TEXT ("DEFPUSHBUTTON"),
     BS_CHECKBOX,        TEXT ("CHECKBOX"), 
     BS_AUTOCHECKBOX,    TEXT ("AUTOCHECKBOX"),
     BS_RADIOBUTTON,     TEXT ("RADIOBUTTON"),
     BS_3STATE,          TEXT ("3STATE"),
     BS_AUTO3STATE,      TEXT ("AUTO3STATE"),
     BS_GROUPBOX,        TEXT ("GROUPBOX"),
     BS_AUTORADIOBUTTON, TEXT ("AUTORADIO"),
     BS_OWNERDRAW,       TEXT ("OWNERDRAW") // this style will indicated that programmer will draw the style of button instead of "button" winclass.
                                            // And windows will send a WM_DRAWITEM message to parent window with lParam which is pointed to DRAWITEMSTRUCT struct to draw the button.
} ;

#define NUM (sizeof button / sizeof button[0])

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("BtnLook") ;
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
     wndclass.hbrBackground = (HBRUSH) (COLOR_BTNFACE + 1); // Use system color here.
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Button Look"),
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
     static HWND  hwndButton[NUM] ;
     static RECT  rect ;
     static TCHAR szTop[]    = TEXT ("message            wParam       lParam"),
                  szUnd[]    = TEXT ("_______            ______       ______"),
                  szFormat[] = TEXT ("%-16s%04X-%04X    %04X-%04X"),
                  szBuffer[50] ;
     static int   cxChar, cyChar ;
     HDC          hdc ;
     PAINTSTRUCT  ps ;
     int          i ;
     
     switch (message)
     {
     case WM_CREATE :
          cxChar = LOWORD (GetDialogBaseUnits ()) ; // same to GetTextMetrics
          cyChar = HIWORD (GetDialogBaseUnits ()) ;
          
          for (i = 0 ; i < NUM ; i++)
               hwndButton[i] = CreateWindow ( TEXT("button"),  // There should be a defined WNDCLASS & WNPROC for the child window "button", so we just use it
                                   button[i].szText,
                                   WS_CHILD | WS_VISIBLE | button[i].iStyle, // Define the child window style and button styles.
                                                                             // because WS_VISIBLE, you don't have to invoke ShowWindow api to show the window or you have to.
                                   // while you can invoke ShowWindow(hwndChild, SH_HIDE) to hide the window.
                                   // IsWindowVisible(hwndChild) to see whether the window is visible or not. EnableWindow(hwndChild, FALSE) to disable the child window.
                                   // IsWindowEnabled(hwndChild)
                                   cxChar, cyChar * (1 + 2 * i),
                                   20 * cxChar, 7 * cyChar / 4,
                                   hwnd, (HMENU) i, // allocate a hmenu as child window id for each child window.
                                   ((LPCREATESTRUCT) lParam)->hInstance, NULL) ; // Set the hInstance for child window which is same to WinMain method.
          return 0 ;

     case WM_SIZE :
          rect.left   = 24 * cxChar ;
          rect.top    =  2 * cyChar ;
          rect.right  = LOWORD (lParam) ;
          rect.bottom = HIWORD (lParam) ;
          return 0 ;

     case WM_PAINT :
          InvalidateRect (hwnd, &rect, TRUE) ; // erase and then redraw the right side of the window.
          
          hdc = BeginPaint (hwnd, &ps) ;
          SelectObject (hdc, GetStockObject (SYSTEM_FIXED_FONT)) ;
          SetBkMode (hdc, TRANSPARENT) ;
          
          TextOut (hdc, 24 * cxChar, cyChar, szTop, lstrlen (szTop)) ;
          TextOut (hdc, 24 * cxChar, cyChar, szUnd, lstrlen (szUnd)) ;
          
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DRAWITEM : // receive this message because of BS_OWNERDRAW style.
     case WM_COMMAND : // receive this message when click the buttons.
          ScrollWindow (hwnd, 0, -cyChar, &rect, &rect) ; // scroll the window instead of redrawing the area, but the area which is not covered by scrolling will be invalid,
                                                          // and be redrawn later.
          
          hdc = GetDC (hwnd) ;
          SelectObject (hdc, GetStockObject (SYSTEM_FIXED_FONT)) ;
          
          // COLOR_BTNFACE is the color for button. If you want to change the button color, don't use SetSysColor for it will affect all the buttons in Windows.
          // You can handle the WM_CTLCOLORBTN message instead, other similar messages for other type of child window is WM_CTLCOLOR****
          SetBkColor(hdc, GetSysColor(COLOR_BTNFACE));
          SetTextColor(hdc, GetSysColor(COLOR_WINDOWTEXT));
          
          TextOut (hdc, 24 * cxChar, cyChar * (rect.bottom / cyChar - 1),
                   szBuffer,
                   wsprintf (szBuffer, szFormat,
                         message == WM_DRAWITEM ? TEXT ("WM_DRAWITEM") : 
                                                  TEXT ("WM_COMMAND"),
                         HIWORD (wParam), LOWORD (wParam), // LOWORD (wParam) means child window id which has been defined in CreateWindow function.
                                                           // HIWORD (wParam) means Notification Code.
                                                           // Notification Code inlucding BT_CLICKED & BT_****
                         HIWORD (lParam), LOWORD (lParam))) ; // (HWND)lParam means hwnd of child window.
          
          ReleaseDC (hwnd, hdc) ;
          ValidateRect (hwnd, &rect) ; // valid rect to avoid redrawing.
          // You can also send WM_XXXX message & BM_GETCHECK or other BM_XXXXXX messages from parent window to child window. 
          int id = LOWORD(wParam); // child window id equals to LOWORD(wParam)
          // id = GetWindowLong(hwndChild, GWL_ID); // you can also get id if you know hwndChild.
          HWND hwndChild = (HWND) lParam; // hwndChild equals to (HWND) lParam
          // hwndChild = GetDlgItem(hwnd, id); // you can also get hwndChild if you know hwnd and id.
          
          TCHAR szString[10];
          wsprintf(szString, TEXT("%d"), id); // convert int to char and print it.
          SetWindowText(hwndChild, szString); // for the window, it changed the title of the window. for the button, it changed the text on the button.
          
          // GetWindowText(hwnd, pszBuffer, iMaxLength) GetWindowTextLength(hwnd)
          
          // SendMessage(hwndChild, BM_SETSTATE, 1, 0) // press the button.
          // SendMessage(hwndChild, BM_SETSTATE, 0, 0) // release the button.
          break ;
     
          // it seems that the code below does not work.
//     case WM_CTLCOLORBTN :
//    	 hdc = (HDC) wParam; // while lParam here means hwnd of the child window.
//    	 SetTextColor(hdc, GetSysColor(COLOR_BTNHILIGHT));
//         SetBkColor(hdc, GetSysColor(COLOR_BTNFACE));
//    	 return (COLOR_BTNFACE + 1); // Use system color here.
          
     case WM_DESTROY :
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

