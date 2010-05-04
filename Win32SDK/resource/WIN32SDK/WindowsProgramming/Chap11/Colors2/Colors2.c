/*------------------------------------------------
   COLORS2.C -- Version using Modeless Dialog Box
                (c) Charles Petzold, 1998
  ------------------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc     (HWND, UINT, WPARAM, LPARAM) ;
BOOL    CALLBACK ColorScrDlg (HWND, UINT, WPARAM, LPARAM) ;

HWND hDlgModeless ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("Colors2") ;
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
     wndclass.hbrBackground = CreateSolidBrush (0L) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Color Scroll"),
                          WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL, NULL, hInstance, NULL) ;
     
     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;
     
     hDlgModeless = CreateDialog (hInstance, TEXT ("ColorScrDlg"), 
                                  hwnd, ColorScrDlg) ;
     
     while (GetMessage (&msg, NULL, 0, 0))
     {
          if (hDlgModeless == 0 || !IsDialogMessage (hDlgModeless, &msg))
          {
               TranslateMessage (&msg) ;
               DispatchMessage  (&msg) ;
          }
     }
     return msg.wParam ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     switch (message)
     {
     case WM_DESTROY :
          DeleteObject ((HGDIOBJ) SetClassLong (hwnd, GCL_HBRBACKGROUND,
                              (LONG) GetStockObject (WHITE_BRUSH))) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

BOOL CALLBACK ColorScrDlg (HWND hDlg, UINT message, 
                           WPARAM wParam, LPARAM lParam)
{
     static int iColor[3] ;
     HWND       hwndParent, hCtrl ;
     int        iCtrlID, iIndex ;
     
     switch (message)
     {
     case WM_INITDIALOG :
          for (iCtrlID = 10 ; iCtrlID < 13 ; iCtrlID++)
          {
               hCtrl = GetDlgItem (hDlg, iCtrlID) ;
               SetScrollRange (hCtrl, SB_CTL, 0, 255, FALSE) ;
               SetScrollPos   (hCtrl, SB_CTL, 0, FALSE) ;
          }
          return TRUE ;
          
     case WM_VSCROLL :
          hCtrl   = (HWND) lParam ;
          iCtrlID = GetWindowLong (hCtrl, GWL_ID) ;
          iIndex  = iCtrlID - 10 ;
          hwndParent = GetParent (hDlg) ;
          
          switch (LOWORD (wParam))
          {
          case SB_PAGEDOWN :
               iColor[iIndex] += 15 ;        // fall through
          case SB_LINEDOWN :
               iColor[iIndex] = min (255, iColor[iIndex] + 1) ;
               break ;
          case SB_PAGEUP :
               iColor[iIndex] -= 15 ;        // fall through
          case SB_LINEUP :
               iColor[iIndex] = max (0, iColor[iIndex] - 1) ;
               break ;
          case SB_TOP :
               iColor[iIndex] = 0 ;
               break ;
          case SB_BOTTOM :
               iColor[iIndex] = 255 ;
               break ;
          case SB_THUMBPOSITION :
          case SB_THUMBTRACK :
               iColor[iIndex] = HIWORD (wParam) ;
               break ;
          default :
               return FALSE ;
          }
          SetScrollPos  (hCtrl, SB_CTL,      iColor[iIndex], TRUE) ;
          SetDlgItemInt (hDlg,  iCtrlID + 3, iColor[iIndex], FALSE) ;
          
          DeleteObject ((HGDIOBJ) SetClassLong (hwndParent, GCL_HBRBACKGROUND,
                              (LONG) CreateSolidBrush (
                                   RGB (iColor[0], iColor[1], iColor[2])))) ;
          
          InvalidateRect (hwndParent, NULL, TRUE) ;
          return TRUE ;
     }
     return FALSE ;
}
