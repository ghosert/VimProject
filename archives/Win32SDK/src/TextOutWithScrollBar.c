#include <windows.h>
#include "sysmets.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("SysMets3") ;
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
          MessageBox (NULL, TEXT ("Program requires Windows NT!"), 
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }

     hwnd = CreateWindow (szAppName, TEXT ("Get System Metrics No. 3"),
                          WS_OVERLAPPEDWINDOW | WS_VSCROLL | WS_HSCROLL, // add vertical scroll and ho horizontal scroll
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
     static int  cxChar, cxCaps, cyChar, cxClient, cyClient, iMaxWidth ;
     HDC         hdc ;
     int         i, x, y, iVertPos, iHorzPos, iPaintBeg, iPaintEnd ;
     PAINTSTRUCT ps ;
     SCROLLINFO  si ;
     TCHAR       szBuffer[10] ;
     TEXTMETRIC  tm ;
     
     switch (message)
     {
     case WM_CREATE:
          hdc = GetDC (hwnd) ; // Like BeginPaint, it will get hdc. The difference is that it is not for paint purpose.
                               // BeginPaint: get hdc with invalid area. GetDC: get hdc with the whole client area.
                               // BeginPaint: make invalid area valid. GetDC: will not make invalid area valid.
                               // GetDC(NULL) means get the desktop area.
                               // GetWindowDC means get hdc with current window area. GetWindowDC(NULL) to get desktop area.
                               // HDC hdc = CreateDC(TEXT("DISPLAY"), NULL, NULL, NULL) to get desktop area. invoking ReleaseDC(hdc) to release dc;
                               // Use ClientToScreen and ScreenToClient to convert them each other.
          
          GetTextMetrics (hdc, &tm) ; // Get the text metric
          cxChar = tm.tmAveCharWidth ;
          cxCaps = (tm.tmPitchAndFamily & 1 ? 3 : 2) * cxChar / 2 ; // for the capitalized character, it can be 1.5 wider than normal one if m.tmPitchAndFamily equals 1.
          cyChar = tm.tmHeight + tm.tmExternalLeading ; // tm.tmExternalLeading means add a extra space line under each line
          
          ReleaseDC (hwnd, hdc) ;

               // Save the width of the three columns
          
          iMaxWidth = 40 * cxChar + 22 * cxCaps ;
          return 0 ;

     case WM_SIZE:
          cxClient = LOWORD (lParam) ; // get the width of Client
          cyClient = HIWORD (lParam) ; // get the heigth of Client

               // Set vertical scroll bar range and page size

          si.cbSize = sizeof (si) ; // must set size of si to cbSize here before using Set/GetScrollInfo
          si.fMask  = SIF_RANGE | SIF_PAGE ; // set SIF_*** here. set SIF_RANGE means si.Min and si.Max will work, set SIF_PAGE means si.nPage will work.
                                             // and SIF_POS can be used to set position si.nPos will work.
          si.nMin   = 0 ;
          si.nMax   = NUMLINES - 1 ;
          si.nPage  = cyClient / cyChar ; // set si.nPage, then the real max value wil be si.nMax - si.nPage + 1 instead of si.nMax
                                          // in this program set si.nPage will make sure the last line will be shown at the bottom of the windows instead of at the top of the windows.
          SetScrollInfo (hwnd, SB_VERT, &si, TRUE) ;

               // Set horizontal scroll bar range and page size

          si.cbSize = sizeof (si) ;
          si.fMask  = SIF_RANGE | SIF_PAGE ;
          si.nMin   = 0 ;
          si.nMax   = 2 + iMaxWidth / cxChar ;
          si.nPage  = cxClient / cxChar ;
          SetScrollInfo (hwnd, SB_HORZ, &si, TRUE) ;
          return 0 ;
          
     case WM_VSCROLL: // will receive this message when the user click or drag the scroll bar.
               // Get all the vertical scroll bar information

          si.cbSize = sizeof (si) ;
          si.fMask  = SIF_ALL ;
          GetScrollInfo (hwnd, SB_VERT, &si) ;

               // Save the position for comparison later on

          iVertPos = si.nPos ;

          switch (LOWORD (wParam)) // ignore the lParam and just parse the wParam to get the action from user on scroll bar.
                                   // SB_*** variable.
          {
          case SB_TOP:  // if thumb arrive top
               si.nPos = si.nMin ;
               break ;
               
          case SB_BOTTOM: // if thumb arrive bottom
               si.nPos = si.nMax ;
               break ;
               
          case SB_LINEUP:
               si.nPos -= 1 ;
               break ;
               
          case SB_LINEDOWN:
               si.nPos += 1 ;
               break ;
               
          case SB_PAGEUP:
               si.nPos -= si.nPage ;
               break ;

          case SB_PAGEDOWN:
               si.nPos += si.nPage ;
               break ;
               
          case SB_THUMBTRACK: // got this, when the user drag scroll bar. see the SB_THUMBPOSITION below, that means when user stop dragging.
        	                  // you can drag the vertical bar and horizontal bar to see the difference.
               si.nPos = si.nTrackPos ;
               break ;
               
          default:
               break ;         
          }
               // Set the position and then retrieve it.  Due to adjustments
               //   by Windows it may not be the same as the value set.

          si.fMask = SIF_POS ; // use SIF_POS here to set position.
          SetScrollInfo (hwnd, SB_VERT, &si, TRUE) ; // call this to set the value, or the thumb will be back to original position before user move thumb
          GetScrollInfo (hwnd, SB_VERT, &si) ;

               // If the position has changed, scroll the window and update it

          if (si.nPos != iVertPos)
          {                    
        	  // we can also use InvalidateRect(hwnd, NULL, TRUE); here to make all client area invalid, then windows will sent a WM_PAINT message
        	  // to redraw. but this way is inefficient. TRUE means ease background by wndclass.hbrBackground;
        	  
               ScrollWindow (hwnd, 0, cyChar * (iVertPos - si.nPos), // call this to scroll not redraw the client
                                   NULL, NULL) ; // the last two parameters are set to NULL means scroll the whole client area.
                                                 // All the area which is not scrolled by this API will be set to invalid area and then be redraw in WM_PAINT
               UpdateWindow (hwnd) ; // Use UpdateWindow to redraw the invalid client immediately.
                                     // Windows will call WinProc with WM_PAINT message directly instead of send WM_PAINT message to message queue.
          }
          return 0 ;
          
     case WM_HSCROLL:
               // Get all the vertical scroll bar information

          si.cbSize = sizeof (si) ;
          si.fMask  = SIF_ALL ;

               // Save the position for comparison later on

          GetScrollInfo (hwnd, SB_HORZ, &si) ;
          iHorzPos = si.nPos ;

          switch (LOWORD (wParam))
          {
          case SB_LINELEFT:
               si.nPos -= 1 ;
               break ;
               
          case SB_LINERIGHT:
               si.nPos += 1 ;
               break ;
               
          case SB_PAGELEFT:
               si.nPos -= si.nPage ;
               break ;
               
          case SB_PAGERIGHT:
               si.nPos += si.nPage ;
               break ;
               
          case SB_THUMBPOSITION:
               si.nPos = si.nTrackPos ;
               break ;
               
          default :
               break ;
          }
               // Set the position and then retrieve it.  Due to adjustments
               //   by Windows it may not be the same as the value set.

          si.fMask = SIF_POS ;
          SetScrollInfo (hwnd, SB_HORZ, &si, TRUE) ;
          GetScrollInfo (hwnd, SB_HORZ, &si) ;
          
               // If the position has changed, scroll the window 

          if (si.nPos != iHorzPos)
          {
               ScrollWindow (hwnd, cxChar * (iHorzPos - si.nPos), 0, 
                             NULL, NULL) ;
          }
          return 0 ;

     case WM_PAINT :
    	 // return 0; This sentence should cause a problem because invalid area have no chance to be valid, so WM_PAINT message will be sent permanently.
    	 // Because no BeginPaint function is called, also DefWindowProc have no chance to handle WM_PAINT message.
          hdc = BeginPaint (hwnd, &ps) ; // This will make invalid area valid after calling BeginPaint
                                         // and this function will use  wndclass.hbrBackground defined above to ease the background fist.
                                         // return hdc with invalid area
                                         // ps.rcPaint will be filled with unvalid area which will be redraw if programmer use it, or they can ignore it redraw the whole client area.

               // Get vertical scroll bar position

          si.cbSize = sizeof (si) ;
          si.fMask  = SIF_POS ;
          GetScrollInfo (hwnd, SB_VERT, &si) ;
          iVertPos = si.nPos ;

               // Get horizontal scroll bar position
          GetScrollInfo (hwnd, SB_HORZ, &si) ;
          iHorzPos = si.nPos ;

               // Find painting limits

          // Only redraw the invalid client area because by using ps.rcpaint - this invalid client area.
          // and using max, min to make sure the value will be between 0 to NUMLINES - 1
          iPaintBeg = max (0, iVertPos + ps.rcPaint.top / cyChar) ;
          iPaintEnd = min (NUMLINES - 1,
                           iVertPos + ps.rcPaint.bottom / cyChar) ;
          
          for (i = iPaintBeg ; i <= iPaintEnd ; i++)
          {
               x = cxChar * (1 - iHorzPos) ;
               y = cyChar * (i - iVertPos) ;
               
               TextOut (hdc, x, y,
                        sysmetrics[i].szLabel,
                        lstrlen (sysmetrics[i].szLabel)) ;
               
               TextOut (hdc, x + 22 * cxCaps, y,
                        sysmetrics[i].szDesc,
                        lstrlen (sysmetrics[i].szDesc)) ;
               
               SetTextAlign (hdc, TA_RIGHT | TA_TOP) ;
               
               TextOut (hdc, x + 22 * cxCaps + 40 * cxChar, y, szBuffer,
                        wsprintf (szBuffer, TEXT ("%5d"),              // format the text using wsprintf and then textout it.
                             GetSystemMetrics (sysmetrics[i].iIndex))) ;

               SetTextAlign (hdc, TA_LEFT | TA_TOP) ;
          }

          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY :
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
