/*-----------------------------------------
   POORMENU.C -- The Poor Person's Menu
                 (c) Charles Petzold, 1998
  -----------------------------------------*/

#include <windows.h>

#define IDM_SYS_ABOUT   1
#define IDM_SYS_HELP    2
#define IDM_SYS_REMOVE  3

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

static TCHAR szAppName[] = TEXT ("PoorMenu") ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     HMENU    hMenu ;
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
     
     hwnd = CreateWindow (szAppName, TEXT ("The Poor-Person's Menu"),
                          WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL, NULL, hInstance, NULL) ;
     
     hMenu = GetSystemMenu (hwnd, FALSE) ;
     
     AppendMenu (hMenu, MF_SEPARATOR, 0,           NULL) ;
     AppendMenu (hMenu, MF_STRING, IDM_SYS_ABOUT,  TEXT ("About...")) ;
     AppendMenu (hMenu, MF_STRING, IDM_SYS_HELP,   TEXT ("Help...")) ;
     AppendMenu (hMenu, MF_STRING, IDM_SYS_REMOVE, TEXT ("Remove Additions")) ;
     
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
     switch (message)
     {
     case WM_SYSCOMMAND:
          switch (LOWORD (wParam))
          {
          case IDM_SYS_ABOUT:
               MessageBox (hwnd, TEXT ("A Poor-Person's Menu Program\n")
                                 TEXT ("(c) Charles Petzold, 1998"),
                           szAppName, MB_OK | MB_ICONINFORMATION) ;
               return 0 ;
               
          case IDM_SYS_HELP:
               MessageBox (hwnd, TEXT ("Help not yet implemented!"),
                           szAppName, MB_OK | MB_ICONEXCLAMATION) ;
               return 0 ;
               
          case IDM_SYS_REMOVE:
               GetSystemMenu (hwnd, TRUE) ;
               return 0 ;
          }
          break ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
