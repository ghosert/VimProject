/*---------------------------------------------
   HEAD.C -- Displays beginning (head) of file
             (c) Charles Petzold, 1998
  ---------------------------------------------*/

#include <windows.h>

#define ID_LIST     1
#define ID_TEXT     2

#define MAXREAD     8192
#define DIRATTR     (DDL_READWRITE | DDL_READONLY | DDL_HIDDEN | DDL_SYSTEM | \
                     DDL_DIRECTORY | DDL_ARCHIVE  | DDL_DRIVES)
#define DTFLAGS     (DT_WORDBREAK | DT_EXPANDTABS | DT_NOCLIP | DT_NOPREFIX)

LRESULT CALLBACK WndProc  (HWND, UINT, WPARAM, LPARAM) ;
LRESULT CALLBACK ListProc (HWND, UINT, WPARAM, LPARAM) ;

WNDPROC OldList ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("head") ;
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
     wndclass.hbrBackground = (HBRUSH) (COLOR_BTNFACE + 1) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("head"),
                          WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
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
     static BOOL     bValidFile ;
     static BYTE     buffer[MAXREAD] ;
     static HWND     hwndList, hwndText ;
     static RECT     rect ;
     static TCHAR    szFile[MAX_PATH + 1] ;
     HANDLE          hFile ;
     HDC             hdc ;
     int             i, cxChar, cyChar ;
     PAINTSTRUCT     ps ;
     TCHAR           szBuffer[MAX_PATH + 1] ;
     
     switch (message)
     {
     case WM_CREATE :
          cxChar = LOWORD (GetDialogBaseUnits ()) ;
          cyChar = HIWORD (GetDialogBaseUnits ()) ;
          
          rect.left = 20 * cxChar ;
          rect.top  =  3 * cyChar ;
          
          hwndList = CreateWindow (TEXT ("listbox"), NULL,
                              WS_CHILDWINDOW | WS_VISIBLE | LBS_STANDARD,
                              cxChar, cyChar * 3,
                              cxChar * 13 + GetSystemMetrics (SM_CXVSCROLL),
                              cyChar * 10,
                              hwnd, (HMENU) ID_LIST,
                              (HINSTANCE) GetWindowLong (hwnd, GWL_HINSTANCE),
                              NULL) ;

          GetCurrentDirectory (MAX_PATH + 1, szBuffer) ;
          
          hwndText = CreateWindow (TEXT ("static"), szBuffer,
                              WS_CHILDWINDOW | WS_VISIBLE | SS_LEFT,
                              cxChar, cyChar, cxChar * MAX_PATH, cyChar,
                              hwnd, (HMENU) ID_TEXT,
                              (HINSTANCE) GetWindowLong (hwnd, GWL_HINSTANCE),
                              NULL) ;
          
          OldList = (WNDPROC) SetWindowLong (hwndList, GWL_WNDPROC,
                                               (LPARAM) ListProc) ;
          
          SendMessage (hwndList, LB_DIR, DIRATTR, (LPARAM) TEXT ("*.*")) ;
          return 0 ;
          
     case WM_SIZE :
          rect.right  = LOWORD (lParam) ;
          rect.bottom = HIWORD (lParam) ;
          return 0 ;
          
     case WM_SETFOCUS :
          SetFocus (hwndList) ;
          return 0 ;
          
     case WM_COMMAND :
          if (LOWORD (wParam) == ID_LIST && HIWORD (wParam) == LBN_DBLCLK)
          {
               if (LB_ERR == (i = SendMessage (hwndList, LB_GETCURSEL, 0, 0)))
                    break ;
               
               SendMessage (hwndList, LB_GETTEXT, i, (LPARAM) szBuffer) ;
               
               if (INVALID_HANDLE_VALUE != (hFile = CreateFile (szBuffer, 
                         GENERIC_READ, FILE_SHARE_READ, NULL, 
                         OPEN_EXISTING, 0, NULL)))

               {
                    CloseHandle (hFile) ;
                    bValidFile = TRUE ;
                    lstrcpy (szFile, szBuffer) ;
                    GetCurrentDirectory (MAX_PATH + 1, szBuffer) ;

                    if (szBuffer [lstrlen (szBuffer) - 1] != '\\')
                         lstrcat (szBuffer, TEXT ("\\")) ;
                    SetWindowText (hwndText, lstrcat (szBuffer, szFile)) ;
               }
               else
               {
                    bValidFile = FALSE ;
                    szBuffer [lstrlen (szBuffer) - 1] = '\0' ;

                         // If setting the directory doesn't work, maybe it's
                         // a drive change, so try that.

                    if (!SetCurrentDirectory (szBuffer + 1))
                    {
                         szBuffer [3] = ':' ;
                         szBuffer [4] = '\0' ;
                         SetCurrentDirectory (szBuffer + 2) ;
                    }

                         // Get the new directory name and fill the list box.

                    GetCurrentDirectory (MAX_PATH + 1, szBuffer) ;
                    SetWindowText (hwndText, szBuffer) ;
                    SendMessage (hwndList, LB_RESETCONTENT, 0, 0) ;
                    SendMessage (hwndList, LB_DIR, DIRATTR, 
                                           (LPARAM) TEXT ("*.*")) ;
               }
               InvalidateRect (hwnd, NULL, TRUE) ;
          }
          return 0 ;
          
     case WM_PAINT :
          if (!bValidFile)
               break ;

          if (INVALID_HANDLE_VALUE == (hFile = CreateFile (szFile, GENERIC_READ, 
                    FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL)))
          {
               bValidFile = FALSE ;
               break ;
          }

          ReadFile (hFile, buffer, MAXREAD, &i, NULL) ;
          CloseHandle (hFile) ;

               // i now equals the number of bytes in buffer.
               // Commence getting a device context for displaying text.

          hdc = BeginPaint (hwnd, &ps) ;
          SelectObject (hdc, GetStockObject (SYSTEM_FIXED_FONT)) ;
          SetTextColor (hdc, GetSysColor (COLOR_BTNTEXT)) ;
          SetBkColor   (hdc, GetSysColor (COLOR_BTNFACE)) ;

               // Assume the file is ASCII

          DrawTextA (hdc, buffer, i, &rect, DTFLAGS) ;

          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY :
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
     
LRESULT CALLBACK ListProc (HWND hwnd, UINT message, 
                           WPARAM wParam, LPARAM lParam)
{
     if (message == WM_KEYDOWN && wParam == VK_RETURN)
          SendMessage (GetParent (hwnd), WM_COMMAND, 
                       MAKELONG (1, LBN_DBLCLK), (LPARAM) hwnd) ;
          
     return CallWindowProc (OldList, hwnd, message, wParam, lParam) ;
}
