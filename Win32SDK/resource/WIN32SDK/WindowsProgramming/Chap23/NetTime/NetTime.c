/*-------------------------------------------------------
   NETTIME.C -- Sets System Clock from Internet Services
                (c) Charles Petzold, 1998
  -------------------------------------------------------*/

#include <windows.h>
#include "resource.h"

#define WM_SOCKET_NOTIFY (WM_USER + 1)
#define ID_TIMER         1

LRESULT CALLBACK WndProc   (HWND, UINT, WPARAM, LPARAM) ;
BOOL    CALLBACK MainDlg   (HWND, UINT, WPARAM, LPARAM) ;
BOOL    CALLBACK ServerDlg (HWND, UINT, WPARAM, LPARAM) ;

void ChangeSystemTime (HWND hwndEdit, ULONG ulTime) ;
void FormatUpdatedTime (HWND hwndEdit, SYSTEMTIME * pstOld, 
                                       SYSTEMTIME * pstNew) ;
void EditPrintf (HWND hwndEdit, TCHAR * szFormat, ...) ;

HINSTANCE hInst ;
HWND      hwndModeless ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("NetTime") ;
     HWND         hwnd ;
     MSG          msg ;
     RECT         rect ;
     WNDCLASS     wndclass ;

     hInst = hInstance ;

     wndclass.style         = 0 ;
     wndclass.lpfnWndProc   = WndProc ;
     wndclass.cbClsExtra    = 0 ;
     wndclass.cbWndExtra    = 0 ;
     wndclass.hInstance     = hInstance ;
     wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
     wndclass.hCursor       = NULL ;
     wndclass.hbrBackground = NULL ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;

     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"), 
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Set System Clock from Internet"),
                          WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU |
                               WS_BORDER | WS_MINIMIZEBOX,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL, NULL, hInstance, NULL) ;

          // Create the modeless dialog box to go on top of the window

     hwndModeless = CreateDialog (hInstance, szAppName, hwnd, MainDlg) ;

          // Size the main parent window to the size of the dialog box.  
          //   Show both windows.

     GetWindowRect (hwndModeless, &rect) ;
     AdjustWindowRect (&rect, WS_CAPTION | WS_BORDER, FALSE) ;

     SetWindowPos (hwnd, NULL, 0, 0, rect.right - rect.left,
                   rect.bottom - rect.top, SWP_NOMOVE) ;

     ShowWindow (hwndModeless, SW_SHOW) ;     
     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;

          // Normal message loop when a modeless dialog box is used.

     while (GetMessage (&msg, NULL, 0, 0))
     {
          if (hwndModeless == 0 || !IsDialogMessage (hwndModeless, &msg))
          {
               TranslateMessage (&msg) ;
               DispatchMessage (&msg) ;
          }
     }
     return msg.wParam ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     switch (message)
     {
     case WM_SETFOCUS:
          SetFocus (hwndModeless) ;
          return 0 ;

     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

BOOL CALLBACK MainDlg (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static char   szIPAddr[32] = { "132.163.135.130" } ;
     static HWND   hwndButton, hwndEdit ;
     static SOCKET sock ;
     static struct sockaddr_in sa ;
     static TCHAR  szOKLabel[32] ;
     int           iError, iSize ;
     unsigned long ulTime ;
     WORD          wEvent, wError ;
     WSADATA       WSAData ;     
 
     switch (message)
     {
     case WM_INITDIALOG:
          hwndButton = GetDlgItem (hwnd, IDOK) ;
          hwndEdit = GetDlgItem (hwnd, IDC_TEXTOUT) ;
          return TRUE ;

     case WM_COMMAND:
          switch (LOWORD (wParam))
          {
          case IDC_SERVER:
               DialogBoxParam (hInst, TEXT ("Servers"), hwnd, ServerDlg, 
                               (LPARAM) szIPAddr) ;
               return TRUE ;

          case IDOK:
                    // Call "WSAStartup" and display description text

               if (iError = WSAStartup (MAKEWORD(2,0), &WSAData))
               {
                    EditPrintf (hwndEdit, TEXT ("Startup error #%i.\r\n"), 
                                          iError) ;
                    return TRUE ;
               }
               EditPrintf (hwndEdit, TEXT ("Started up %hs\r\n"), 
                                     WSAData.szDescription);

                    // Call "socket"

               sock = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP) ;

               if (sock == INVALID_SOCKET)
               {
                    EditPrintf (hwndEdit, 
                                TEXT ("Socket creation error #%i.\r\n"), 
                                WSAGetLastError ()) ;
                    WSACleanup () ;
                    return TRUE ;
               }
               EditPrintf (hwndEdit, TEXT ("Socket %i created.\r\n"), sock) ;

                    // Call "WSAAsyncSelect" 

               if (SOCKET_ERROR == WSAAsyncSelect (sock, hwnd, WM_SOCKET_NOTIFY, 
                                                   FD_CONNECT | FD_READ))
               {
                    EditPrintf (hwndEdit, 
                                TEXT ("WSAAsyncSelect error #%i.\r\n"),
                                WSAGetLastError ()) ;
                    closesocket (sock) ;
                    WSACleanup () ;
                    return TRUE ;
               }

                    // Call "connect" with IP address and time-server port

               sa.sin_family           = AF_INET ;
               sa.sin_port             = htons (IPPORT_TIMESERVER) ; 
               sa.sin_addr.S_un.S_addr = inet_addr (szIPAddr) ;

               connect(sock, (SOCKADDR *) &sa, sizeof (sa)) ;

                    // "connect" will return SOCKET_ERROR because even if it
                    // succeeds, it will require blocking. The following only
                    // reports unexpected errors.

               if (WSAEWOULDBLOCK != (iError = WSAGetLastError ()))
               {
                    EditPrintf (hwndEdit, TEXT ("Connect error #%i.\r\n"), 
                                          iError) ;
                    closesocket (sock) ;
                    WSACleanup () ;
                    return TRUE ;
               }
               EditPrintf (hwndEdit, TEXT ("Connecting to %hs..."), szIPAddr) ;
     
                    // The result of the "connect" call will be reported 
                    // through the WM_SOCKET_NOTIFY message.
                    // Set timer and change the button to "Cancel"

               SetTimer (hwnd, ID_TIMER, 1000, NULL) ;
               GetWindowText (hwndButton, szOKLabel, sizeof (szOKLabel) /
                                                     sizeof (TCHAR)) ;
               SetWindowText (hwndButton, TEXT ("Cancel")) ;
               SetWindowLong (hwndButton, GWL_ID, IDCANCEL) ;
               return TRUE ;

          case IDCANCEL:
               closesocket (sock) ;
               sock = 0 ;
               WSACleanup () ;
               SetWindowText (hwndButton, szOKLabel) ;
               SetWindowLong (hwndButton, GWL_ID, IDOK) ;

               KillTimer (hwnd, ID_TIMER) ;
               EditPrintf (hwndEdit, TEXT ("\r\nSocket closed.\r\n")) ;
               return TRUE ;

          case IDC_CLOSE:
               if (sock)
                    SendMessage (hwnd, WM_COMMAND, IDCANCEL, 0) ;

               DestroyWindow (GetParent (hwnd)) ;
               return TRUE ;
          }
          return FALSE ;

     case WM_TIMER:
          EditPrintf (hwndEdit, TEXT (".")) ;
          return TRUE ;

     case WM_SOCKET_NOTIFY:
          wEvent = WSAGETSELECTEVENT (lParam) ;   // ie, LOWORD
          wError = WSAGETSELECTERROR (lParam) ;   // ie, HIWORD

               // Process two events specified in WSAAsyncSelect

          switch (wEvent)
          {
               // This event occurs as a result of the "connect" call

          case FD_CONNECT:
               EditPrintf (hwndEdit, TEXT ("\r\n")) ;

               if (wError)
               {
                    EditPrintf (hwndEdit, TEXT ("Connect error #%i."), 
                                          wError) ;
                    SendMessage (hwnd, WM_COMMAND, IDCANCEL, 0) ;
                    return TRUE ;
               }
               EditPrintf (hwndEdit, TEXT ("Connected to %hs.\r\n"), szIPAddr) ;

                    // Try to receive data. The call will generate an error
                    // of WSAEWOULDBLOCK and an event of FD_READ

               recv (sock, (char *) &ulTime, 4, MSG_PEEK) ;
               EditPrintf (hwndEdit, TEXT ("Waiting to receive...")) ;
               return TRUE ;

                    // This even occurs when the "recv" call can be made
               
          case FD_READ:
               KillTimer (hwnd, ID_TIMER) ;
               EditPrintf (hwndEdit, TEXT ("\r\n")) ;

               if (wError)
               {
                    EditPrintf (hwndEdit, TEXT ("FD_READ error #%i."), 
                                          wError) ;
                    SendMessage (hwnd, WM_COMMAND, IDCANCEL, 0) ;
                    return TRUE ;
               }
                    // Get the time and swap the bytes

               iSize = recv (sock, (char *) &ulTime, 4, 0) ;
               ulTime = ntohl (ulTime) ;
               EditPrintf (hwndEdit, 
                           TEXT ("Received current time of %u seconds ")
                           TEXT ("since Jan. 1 1900.\r\n"), ulTime) ;

                    // Change the system time
     
               ChangeSystemTime (hwndEdit, ulTime) ;
               SendMessage (hwnd, WM_COMMAND, IDCANCEL, 0) ;
               return TRUE ;
          }
          return FALSE ;
     }
     return FALSE ;
}

BOOL CALLBACK ServerDlg (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static char * szServer ;
     static WORD   wServer = IDC_SERVER1 ;
     char          szLabel [64] ;

     switch (message)
     {
     case WM_INITDIALOG:
          szServer = (char *) lParam ;
          CheckRadioButton (hwnd, IDC_SERVER1, IDC_SERVER10, wServer) ;
          return TRUE ;

     case WM_COMMAND:
          switch (LOWORD (wParam))
          {
          case IDC_SERVER1:
          case IDC_SERVER2:
          case IDC_SERVER3:
          case IDC_SERVER4:
          case IDC_SERVER5:
          case IDC_SERVER6:
          case IDC_SERVER7:
          case IDC_SERVER8:
          case IDC_SERVER9:
          case IDC_SERVER10:
               wServer = LOWORD (wParam) ;
               return TRUE ;

          case IDOK:
               GetDlgItemTextA (hwnd, wServer, szLabel, sizeof (szLabel)) ;
               strtok (szLabel, "(") ;
               strcpy (szServer, strtok (NULL, ")")) ;
               EndDialog (hwnd, TRUE) ;
               return TRUE ;

          case IDCANCEL:
               EndDialog (hwnd, FALSE) ;
               return TRUE ;
          }
          break ;
     }
     return FALSE ;
}

void ChangeSystemTime (HWND hwndEdit, ULONG ulTime)
{
     FILETIME      ftNew ;
     LARGE_INTEGER li ;
     SYSTEMTIME    stOld, stNew ;

     GetLocalTime (&stOld) ;

     stNew.wYear         = 1900 ;
     stNew.wMonth        = 1 ;
     stNew.wDay          = 1 ;
     stNew.wHour         = 0 ;
     stNew.wMinute       = 0 ;
     stNew.wSecond       = 0 ;
     stNew.wMilliseconds = 0 ;

     SystemTimeToFileTime (&stNew, &ftNew) ;
     li = * (LARGE_INTEGER *) &ftNew ;
     li.QuadPart += (LONGLONG) 10000000 * ulTime ; 
     ftNew = * (FILETIME *) &li ;
     FileTimeToSystemTime (&ftNew, &stNew) ;

     if (SetSystemTime (&stNew))
     {
          GetLocalTime (&stNew) ;
          FormatUpdatedTime (hwndEdit, &stOld, &stNew) ;
     }
     else
          EditPrintf (hwndEdit, TEXT ("Could NOT set new date and time.")) ;
}

void FormatUpdatedTime (HWND hwndEdit, SYSTEMTIME * pstOld, SYSTEMTIME * pstNew)
{
     TCHAR szDateOld [64], szTimeOld [64], szDateNew [64], szTimeNew [64] ;

     GetDateFormat (LOCALE_USER_DEFAULT, LOCALE_NOUSEROVERRIDE | DATE_SHORTDATE,
                    pstOld, NULL, szDateOld, sizeof (szDateOld)) ;
     
     GetTimeFormat (LOCALE_USER_DEFAULT, LOCALE_NOUSEROVERRIDE | 
                         TIME_NOTIMEMARKER | TIME_FORCE24HOURFORMAT,
                    pstOld, NULL, szTimeOld, sizeof (szTimeOld)) ;

     GetDateFormat (LOCALE_USER_DEFAULT, LOCALE_NOUSEROVERRIDE | DATE_SHORTDATE,
                    pstNew, NULL, szDateNew, sizeof (szDateNew)) ;
     
     GetTimeFormat (LOCALE_USER_DEFAULT, LOCALE_NOUSEROVERRIDE | 
                         TIME_NOTIMEMARKER | TIME_FORCE24HOURFORMAT,
                    pstNew, NULL, szTimeNew, sizeof (szTimeNew)) ;

     EditPrintf (hwndEdit, 
                 TEXT ("System date and time successfully changed ")
                 TEXT ("from\r\n\t%s, %s.%03i to\r\n\t%s, %s.%03i."), 
                 szDateOld, szTimeOld, pstOld->wMilliseconds,
                 szDateNew, szTimeNew, pstNew->wMilliseconds) ;
}

void EditPrintf (HWND hwndEdit, TCHAR * szFormat, ...)
{
     TCHAR   szBuffer [1024] ;
     va_list pArgList ;

     va_start (pArgList, szFormat) ;
     wvsprintf (szBuffer, szFormat, pArgList) ;
     va_end (pArgList) ;

     SendMessage (hwndEdit, EM_SETSEL, (WPARAM) -1, (LPARAM) -1) ;
     SendMessage (hwndEdit, EM_REPLACESEL, FALSE, (LPARAM) szBuffer) ;
     SendMessage (hwndEdit, EM_SCROLLCARET, 0, 0) ;
}
