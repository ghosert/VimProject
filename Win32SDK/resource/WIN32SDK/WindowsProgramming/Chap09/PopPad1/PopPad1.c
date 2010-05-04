/*-------------------------------------------------------
   POPPAD1.C -- Popup Editor using child window edit box
                (c) Charles Petzold, 1998
  -------------------------------------------------------*/

#include <windows.h>

#define ID_EDIT     1

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM);

TCHAR szAppName[] = TEXT ("PopPad1") ;

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
     
     hwnd = CreateWindow (szAppName, szAppName,
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
     static HWND hwndEdit ;
     
     switch (message)
     {
     case WM_CREATE :
          hwndEdit = CreateWindow (TEXT ("edit"), NULL,
                         WS_CHILD | WS_VISIBLE | WS_HSCROLL | WS_VSCROLL |
                                   WS_BORDER | ES_LEFT | ES_MULTILINE |
                                   ES_AUTOHSCROLL | ES_AUTOVSCROLL,
                         0, 0, 0, 0, hwnd, (HMENU) ID_EDIT,
                         ((LPCREATESTRUCT) lParam) -> hInstance, NULL) ;
          return 0 ;
          
     case WM_SETFOCUS :
          SetFocus (hwndEdit) ;
          return 0 ;
          
     case WM_SIZE : 
          MoveWindow (hwndEdit, 0, 0, LOWORD (lParam), HIWORD (lParam), TRUE) ;
          return 0 ;
          
     case WM_COMMAND :
          if (LOWORD (wParam) == ID_EDIT)
               if (HIWORD (wParam) == EN_ERRSPACE || 
                         HIWORD (wParam) == EN_MAXTEXT)

                    MessageBox (hwnd, TEXT ("Edit control out of space."),
                                szAppName, MB_OK | MB_ICONSTOP) ;
          return 0 ;
               
     case WM_DESTROY :
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
