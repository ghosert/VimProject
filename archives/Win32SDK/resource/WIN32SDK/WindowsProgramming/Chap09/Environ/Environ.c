/*----------------------------------------
   ENVIRON.C -- Environment List Box
                (c) Charles Petzold, 1998
  ----------------------------------------*/

#include <windows.h>

#define ID_LIST     1
#define ID_TEXT     2

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("Environ") ;
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
     wndclass.hbrBackground = (HBRUSH) (COLOR_WINDOW + 1) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Environment List Box"),
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

void FillListBox (HWND hwndList) 
{
     int     iLength ;
     TCHAR * pVarBlock, * pVarBeg, * pVarEnd, * pVarName ;

     pVarBlock = GetEnvironmentStrings () ;  // Get pointer to environment block

     while (*pVarBlock)
     {
          if (*pVarBlock != '=')   // Skip variable names beginning with '='
          {
               pVarBeg = pVarBlock ;              // Beginning of variable name
               while (*pVarBlock++ != '=') ;      // Scan until '='
               pVarEnd = pVarBlock - 1 ;          // Points to '=' sign
               iLength = pVarEnd - pVarBeg ;      // Length of variable name

                    // Allocate memory for the variable name and terminating
                    // zero. Copy the variable name and append a zero.

               pVarName = calloc (iLength + 1, sizeof (TCHAR)) ;
               CopyMemory (pVarName, pVarBeg, iLength * sizeof (TCHAR)) ;
               pVarName[iLength] = '\0' ;

                    // Put the variable name in the list box and free memory.

               SendMessage (hwndList, LB_ADDSTRING, 0, (LPARAM) pVarName) ;
               free (pVarName) ;
          }
          while (*pVarBlock++ != '\0') ;     // Scan until terminating zero
     }
     FreeEnvironmentStrings (pVarBlock) ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static HWND  hwndList, hwndText ;
     int          iIndex, iLength, cxChar, cyChar ;
     TCHAR      * pVarName, * pVarValue ;

     switch (message)
     {
     case WM_CREATE :
          cxChar = LOWORD (GetDialogBaseUnits ()) ;
          cyChar = HIWORD (GetDialogBaseUnits ()) ;

               // Create listbox and static text windows.
          
          hwndList = CreateWindow (TEXT ("listbox"), NULL,
                              WS_CHILD | WS_VISIBLE | LBS_STANDARD,
                              cxChar, cyChar * 3,
                              cxChar * 16 + GetSystemMetrics (SM_CXVSCROLL),
                              cyChar * 5,
                              hwnd, (HMENU) ID_LIST,
                              (HINSTANCE) GetWindowLong (hwnd, GWL_HINSTANCE),
                              NULL) ;
          
          hwndText = CreateWindow (TEXT ("static"), NULL,
                              WS_CHILD | WS_VISIBLE | SS_LEFT,
                              cxChar, cyChar, 
                              GetSystemMetrics (SM_CXSCREEN), cyChar,
                              hwnd, (HMENU) ID_TEXT,
                              (HINSTANCE) GetWindowLong (hwnd, GWL_HINSTANCE),
                              NULL) ;

          FillListBox (hwndList) ;
          return 0 ;
          
     case WM_SETFOCUS :
          SetFocus (hwndList) ;
          return 0 ;
          
     case WM_COMMAND :
          if (LOWORD (wParam) == ID_LIST && HIWORD (wParam) == LBN_SELCHANGE)
          {
                    // Get current selection.

               iIndex  = SendMessage (hwndList, LB_GETCURSEL, 0, 0) ;
               iLength = SendMessage (hwndList, LB_GETTEXTLEN, iIndex, 0) + 1 ;
               pVarName = calloc (iLength, sizeof (TCHAR)) ;
               SendMessage (hwndList, LB_GETTEXT, iIndex, (LPARAM) pVarName) ;

                    // Get environment string.

               iLength = GetEnvironmentVariable (pVarName, NULL, 0) ;
               pVarValue = calloc (iLength, sizeof (TCHAR)) ;
               GetEnvironmentVariable (pVarName, pVarValue, iLength) ;

                    // Show it in window.
               
               SetWindowText (hwndText, pVarValue) ;
               free (pVarName) ;
               free (pVarValue) ;
          }
          return 0 ;

     case WM_DESTROY :
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
