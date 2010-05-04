/*---------------------------------------
   MULTI1.C -- Multitasking Demo
               (c) Charles Petzold, 1998
  ---------------------------------------*/

#include <windows.h>
#include <math.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int cyChar ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("Multi1") ;
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
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Multitasking Demo"),
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

int CheckBottom (HWND hwnd, int cyClient, int iLine)
{
     if (iLine * cyChar + cyChar > cyClient)
     {
          InvalidateRect (hwnd, NULL, TRUE) ;
          UpdateWindow (hwnd) ;
          iLine = 0 ;
     }
     return iLine ;
}

// ------------------------------------------------
// Window 1: Display increasing sequence of numbers
// ------------------------------------------------

LRESULT APIENTRY WndProc1 (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static int iNum, iLine, cyClient ;
     HDC        hdc ;
     TCHAR      szBuffer[16] ;
     
     switch (message)
     {
     case WM_SIZE:
          cyClient = HIWORD (lParam) ;
          return 0 ;
          
     case WM_TIMER:
          if (iNum < 0)
               iNum = 0 ;
          
          iLine = CheckBottom (hwnd, cyClient, iLine) ;
          hdc = GetDC (hwnd) ;

          TextOut (hdc, 0, iLine * cyChar, szBuffer, 
                   wsprintf (szBuffer, TEXT ("%d"), iNum++)) ;

          ReleaseDC (hwnd, hdc) ;
          iLine++ ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

// ------------------------------------------------------
// Window 2: Display increasing sequence of prime numbers
// ------------------------------------------------------

LRESULT APIENTRY WndProc2 (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static int iNum = 1, iLine, cyClient ;
     HDC        hdc ;
     int        i, iSqrt ;
     TCHAR      szBuffer[16] ;
     
     switch (message)
     {
     case WM_SIZE:
          cyClient = HIWORD (lParam) ;
          return 0 ;
          
     case WM_TIMER:
          do   {
               if (++iNum < 0)
                    iNum = 0 ;
               
               iSqrt = (int) sqrt (iNum) ;
               
               for (i = 2 ; i <= iSqrt ; i++)
                    if (iNum % i == 0)
                         break ;
          }
          while (i <= iSqrt) ;
          
          iLine = CheckBottom (hwnd, cyClient, iLine) ;
          hdc = GetDC (hwnd) ;

          TextOut (hdc, 0, iLine * cyChar, szBuffer, 
                   wsprintf (szBuffer, TEXT ("%d"), iNum)) ;

          ReleaseDC (hwnd, hdc) ;
          iLine++ ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

// ----------------------------------------------------------
// Window 3: Display increasing sequence of Fibonacci numbers
// ----------------------------------------------------------

LRESULT APIENTRY WndProc3 (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static int iNum = 0, iNext = 1, iLine, cyClient ;
     HDC        hdc ;
     int        iTemp ;
     TCHAR      szBuffer[16] ;
     
     switch (message)
     {
     case WM_SIZE:
          cyClient = HIWORD (lParam) ;
          return 0 ;
          
     case WM_TIMER:
          if (iNum < 0)
          {
               iNum  = 0 ;
               iNext = 1 ;
          }
          
          iLine = CheckBottom (hwnd, cyClient, iLine) ;
          hdc = GetDC (hwnd) ;

          TextOut (hdc, 0, iLine * cyChar, szBuffer, 
                   wsprintf (szBuffer, TEXT ("%d"), iNum)) ;

          ReleaseDC (hwnd, hdc) ;
          iTemp  = iNum ;
          iNum   = iNext ;
          iNext += iTemp ;
          iLine++ ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

// -----------------------------------------
// Window 4: Display circles of random radii
// -----------------------------------------

LRESULT APIENTRY WndProc4 (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static int cxClient, cyClient ;
     HDC        hdc ;
     int        iDiameter ;
     
     switch (message)
     {
     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;
          
     case WM_TIMER:
          InvalidateRect (hwnd, NULL, TRUE) ;
          UpdateWindow (hwnd) ;
          
          iDiameter = rand() % (max (1, min (cxClient, cyClient))) ;
          hdc = GetDC (hwnd) ;
          
          Ellipse (hdc, (cxClient - iDiameter) / 2,
                        (cyClient - iDiameter) / 2,
                        (cxClient + iDiameter) / 2,
                        (cyClient + iDiameter) / 2) ;
          
          ReleaseDC (hwnd, hdc) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

// -----------------------------------
// Main window to create child windows
// -----------------------------------

LRESULT APIENTRY WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static HWND    hwndChild[4] ;
     static TCHAR * szChildClass[] = { TEXT ("Child1"), TEXT ("Child2"),
                                       TEXT ("Child3"), TEXT ("Child4") } ;
     static WNDPROC ChildProc[] = { WndProc1, WndProc2, WndProc3, WndProc4 } ;
     HINSTANCE      hInstance ;
     int            i, cxClient, cyClient ;
     WNDCLASS       wndclass ;
     
     switch (message)
     {
     case WM_CREATE:
          hInstance = (HINSTANCE) GetWindowLong (hwnd, GWL_HINSTANCE) ;
          
          wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
          wndclass.cbClsExtra    = 0 ;
          wndclass.cbWndExtra    = 0 ;
          wndclass.hInstance     = hInstance ;
          wndclass.hIcon         = NULL ;
          wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
          wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH) ;
          wndclass.lpszMenuName  = NULL ;
          
          for (i = 0 ; i < 4 ; i++)
          {
               wndclass.lpfnWndProc   = ChildProc[i] ;
               wndclass.lpszClassName = szChildClass[i] ;
               
               RegisterClass (&wndclass) ;
               
               hwndChild[i] = CreateWindow (szChildClass[i], NULL,
                                   WS_CHILDWINDOW | WS_BORDER | WS_VISIBLE,
                                   0, 0, 0, 0, 
                                   hwnd, (HMENU) i, hInstance, NULL) ;
          }
          
          cyChar = HIWORD (GetDialogBaseUnits ()) ;
          SetTimer (hwnd, 1, 10, NULL) ;
          return 0 ;
          
     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          
          for (i = 0 ; i < 4 ; i++)
               MoveWindow (hwndChild[i], (i % 2) * cxClient / 2,
                                         (i > 1) * cyClient / 2,
                           cxClient / 2, cyClient / 2, TRUE) ;
          return 0 ;
          
     case WM_TIMER:
          for (i = 0 ; i < 4 ; i++)
               SendMessage (hwndChild[i], WM_TIMER, wParam, lParam) ;
          
          return 0 ;
          
     case WM_CHAR:
          if (wParam == '\x1B')
               DestroyWindow (hwnd) ;
          
          return 0 ;
          
     case WM_DESTROY:
          KillTimer (hwnd, 1) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
