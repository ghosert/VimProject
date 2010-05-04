/*-----------------------------------------------
   UNICHARS.C -- Displays 16-bit character codes
                 (c) Charles Petzold, 1998
  -----------------------------------------------*/

#include <windows.h>
#include "resource.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("UniChars") ;
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
     wndclass.lpszMenuName  = szAppName ;
     wndclass.lpszClassName = szAppName ;

     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requies Windows NT!"), 
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Unicode Characters"),
                          WS_OVERLAPPEDWINDOW | WS_VSCROLL,
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
     static CHOOSEFONT cf ;
     static int        iPage ;
     static LOGFONT    lf ;
     HDC               hdc ;
     int               cxChar, cyChar, x, y, i, cxLabels ;
     PAINTSTRUCT       ps ;
     SIZE              size ;
     TCHAR             szBuffer [8] ;
     TEXTMETRIC        tm ;
     WCHAR             ch ;

     switch (message)
     {
     case WM_CREATE:
          hdc = GetDC (hwnd) ;
          lf.lfHeight = - GetDeviceCaps (hdc, LOGPIXELSY) / 6 ;  // 12 points
          lstrcpy (lf.lfFaceName, TEXT ("Lucida Sans Unicode")) ;
          ReleaseDC (hwnd, hdc) ;

          cf.lStructSize = sizeof (CHOOSEFONT) ;
          cf.hwndOwner   = hwnd ;
          cf.lpLogFont   = &lf ;
          cf.Flags       = CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS ;

          SetScrollRange (hwnd, SB_VERT, 0, 255, FALSE) ;
          SetScrollPos   (hwnd, SB_VERT, iPage,  TRUE ) ;
          return 0 ;

     case WM_COMMAND:
          switch (LOWORD (wParam))
          {
          case IDM_FONT:
               if (ChooseFont (&cf))
                    InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;
          }
          return 0 ;

     case WM_VSCROLL:
          switch (LOWORD (wParam))
               {
               case SB_LINEUP:         iPage -=  1 ;  break ;
               case SB_LINEDOWN:       iPage +=  1 ;  break ;
               case SB_PAGEUP:         iPage -= 16 ;  break ;
               case SB_PAGEDOWN:       iPage += 16 ;  break ;
               case SB_THUMBPOSITION:  iPage = HIWORD (wParam) ;  break ;

               default:
                    return 0 ;
               }

          iPage = max (0, min (iPage, 255)) ;

          SetScrollPos (hwnd, SB_VERT, iPage, TRUE) ;
          InvalidateRect (hwnd, NULL, TRUE) ;
          return 0 ;

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;

          SelectObject (hdc, CreateFontIndirect (&lf)) ;

          GetTextMetrics (hdc, &tm) ;
          cxChar = tm.tmMaxCharWidth ;
          cyChar = tm.tmHeight + tm.tmExternalLeading ;

          cxLabels = 0 ;

          for (i = 0 ; i < 16 ; i++)
          {
               wsprintf (szBuffer, TEXT (" 000%1X: "), i) ;
               GetTextExtentPoint (hdc, szBuffer, 7, &size) ;

               cxLabels = max (cxLabels, size.cx) ;
          }

          for (y = 0 ; y < 16 ; y++)
          {
               wsprintf (szBuffer, TEXT (" %03X_: "), 16 * iPage + y) ;
               TextOut (hdc, 0, y * cyChar, szBuffer, 7) ;

               for (x = 0 ; x < 16 ; x++)
               {
                    ch = (WCHAR) (256 * iPage + 16 * y + x) ;
                    TextOutW (hdc, x * cxChar + cxLabels,
                                   y * cyChar, &ch, 1);
               }
          }

          DeleteObject (SelectObject (hdc, GetStockObject (SYSTEM_FONT)));
          EndPaint (hwnd, &ps) ;
          return 0 ;

     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
