/*--------------------------------------------------------
   STRPROG.C -- Program using STRLIB dynamic-link library
                (c) Charles Petzold, 1998
  --------------------------------------------------------*/

#include <windows.h>
#include "strlib.h"
#include "resource.h"

typedef struct
{
     HDC hdc ;
     int xText ;
     int yText ;
     int xStart ;
     int yStart ;
     int xIncr ;
     int yIncr ;
     int xMax ;
     int yMax ;
}
CBPARAM ;

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName [] = TEXT ("StrProg") ;
TCHAR szString [MAX_LENGTH + 1] ;

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
     wndclass.lpszMenuName  = szAppName ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }

     hwnd = CreateWindow (szAppName, TEXT ("DLL Demonstration Program"),
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

BOOL CALLBACK DlgProc (HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
     switch (message)
     {
     case WM_INITDIALOG:
          SendDlgItemMessage (hDlg, IDC_STRING, EM_LIMITTEXT, MAX_LENGTH, 0) ;
          return TRUE ;
          
     case WM_COMMAND:
          switch (wParam)
          {
          case IDOK:
               GetDlgItemText (hDlg, IDC_STRING, szString, MAX_LENGTH) ;
               EndDialog (hDlg, TRUE) ;
               return TRUE ;
               
          case IDCANCEL:
               EndDialog (hDlg, FALSE) ;
               return TRUE ;
          }
     }
     return FALSE ;
}

BOOL CALLBACK GetStrCallBack (PTSTR pString, CBPARAM * pcbp)
{
     TextOut (pcbp->hdc, pcbp->xText, pcbp->yText,
              pString, lstrlen (pString)) ;
     
     if ((pcbp->yText += pcbp->yIncr) > pcbp->yMax)
     {
          pcbp->yText = pcbp->yStart ;
          if ((pcbp->xText += pcbp->xIncr) > pcbp->xMax)
               return FALSE ;
     }
     return TRUE ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static HINSTANCE  hInst ;
     static int        cxChar, cyChar, cxClient, cyClient ;
     static UINT       iDataChangeMsg ;
     CBPARAM           cbparam ;
     HDC               hdc ;
     PAINTSTRUCT       ps ;
     TEXTMETRIC        tm ;
     
     switch (message)
     {
     case WM_CREATE:
          hInst = ((LPCREATESTRUCT) lParam)->hInstance ;
          hdc   = GetDC (hwnd) ;
          GetTextMetrics (hdc, &tm) ;
          cxChar = (int) tm.tmAveCharWidth ;
          cyChar = (int) (tm.tmHeight + tm.tmExternalLeading) ;
          ReleaseDC (hwnd, hdc) ;

               // Register message for notifying instances of data changes

          iDataChangeMsg = RegisterWindowMessage (TEXT ("StrProgDataChange")) ;
          return 0 ;
          
     case WM_COMMAND:
          switch (wParam)
          {
          case IDM_ENTER:
               if (DialogBox (hInst, TEXT ("EnterDlg"), hwnd, &DlgProc))
               {
                    if (AddString (szString))
                         PostMessage (HWND_BROADCAST, iDataChangeMsg, 0, 0) ;
                    else
                         MessageBeep (0) ;
               }
               break ;
               
          case IDM_DELETE:
               if (DialogBox (hInst, TEXT ("DeleteDlg"), hwnd, &DlgProc))
               {
                    if (DeleteString (szString))
                         PostMessage (HWND_BROADCAST, iDataChangeMsg, 0, 0) ;
                    else
                         MessageBeep (0) ;
               }
               break ;
          }
          return 0 ;
          
     case WM_SIZE:
          cxClient = (int) LOWORD (lParam) ;
          cyClient = (int) HIWORD (lParam) ;
          return 0 ;
               
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
               
          cbparam.hdc   = hdc ;
          cbparam.xText = cbparam.xStart = cxChar ;
          cbparam.yText = cbparam.yStart = cyChar ;
          cbparam.xIncr = cxChar * MAX_LENGTH ;
          cbparam.yIncr = cyChar ;
          cbparam.xMax  = cbparam.xIncr * (1 + cxClient / cbparam.xIncr) ;
          cbparam.yMax  = cyChar * (cyClient / cyChar - 1) ;
               
          GetStrings ((GETSTRCB) GetStrCallBack, (PVOID) &cbparam) ;
              
          EndPaint (hwnd, &ps) ;
          return 0 ;
               
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;

     default:
          if (message == iDataChangeMsg)
               InvalidateRect (hwnd, NULL, TRUE) ;
          break ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
