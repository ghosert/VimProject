/*-----------------------------------------
   CHOSFONT.C -- ChooseFont Demo
                 (c) Charles Petzold, 1998
  -----------------------------------------*/

#include <windows.h>
#include "resource.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("ChosFont") ;
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
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
               szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("ChooseFont"),
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
     static CHOOSEFONT cf ;
     static int        cyChar ;
     static LOGFONT    lf ;
     static TCHAR      szText[] = TEXT ("\x41\x42\x43\x44\x45 ")
                                  TEXT ("\x61\x62\x63\x64\x65 ")

                                  TEXT ("\xC0\xC1\xC2\xC3\xC4\xC5 ")
                                  TEXT ("\xE0\xE1\xE2\xE3\xE4\xE5 ") 
#ifdef UNICODE
                                  TEXT ("\x0390\x0391\x0392\x0393\x0394\x0395 ")
                                  TEXT ("\x03B0\x03B1\x03B2\x03B3\x03B4\x03B5 ")

                                  TEXT ("\x0410\x0411\x0412\x0413\x0414\x0415 ")
                                  TEXT ("\x0430\x0431\x0432\x0433\x0434\x0435 ")

                                  TEXT ("\x5000\x5001\x5002\x5003\x5004") 
#endif
                                 ;
     HDC               hdc ;
     int               y ;
     PAINTSTRUCT       ps ;
     TCHAR             szBuffer [64] ;
     TEXTMETRIC        tm ;
     
     switch (message)
     {
     case WM_CREATE:

               // Get text height

          cyChar = HIWORD (GetDialogBaseUnits ()) ;

               // Initialize the LOGFONT structure

          GetObject (GetStockObject (SYSTEM_FONT), sizeof (lf), &lf) ;

               // Initialize the CHOOSEFONT structure

          cf.lStructSize    = sizeof (CHOOSEFONT) ;
          cf.hwndOwner      = hwnd ;
          cf.hDC            = NULL ;
          cf.lpLogFont      = &lf ;
          cf.iPointSize     = 0 ;
          cf.Flags          = CF_INITTOLOGFONTSTRUCT |
                              CF_SCREENFONTS | CF_EFFECTS ;
          cf.rgbColors      = 0 ;
          cf.lCustData      = 0 ;
          cf.lpfnHook       = NULL ;
          cf.lpTemplateName = NULL ;
          cf.hInstance      = NULL ;
          cf.lpszStyle      = NULL ;
          cf.nFontType      = 0 ;      
          cf.nSizeMin       = 0 ;
          cf.nSizeMax       = 0 ;
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

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;

               // Display sample text using selected font

          SelectObject (hdc, CreateFontIndirect (&lf)) ;
          GetTextMetrics (hdc, &tm) ;
          SetTextColor (hdc, cf.rgbColors) ;
          TextOut (hdc, 0, y = tm.tmExternalLeading, szText, lstrlen (szText)) ;

               // Display LOGFONT structure fields using system font

          DeleteObject (SelectObject (hdc, GetStockObject (SYSTEM_FONT))) ;
          SetTextColor (hdc, 0) ;
          
          TextOut (hdc, 0, y += tm.tmHeight, szBuffer,
               wsprintf (szBuffer, TEXT ("lfHeight = %i"), lf.lfHeight)) ;
          
          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfWidth = %i"), lf.lfWidth)) ;
          
          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfEscapement = %i"), 
                         lf.lfEscapement)) ;
          
          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfOrientation = %i"), 
                         lf.lfOrientation)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfWeight = %i"), lf.lfWeight)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfItalic = %i"), lf.lfItalic)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfUnderline = %i"), lf.lfUnderline)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfStrikeOut = %i"), lf.lfStrikeOut)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfCharSet = %i"), lf.lfCharSet)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfOutPrecision = %i"), 
                         lf.lfOutPrecision)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfClipPrecision = %i"), 
                         lf.lfClipPrecision)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfQuality = %i"), lf.lfQuality)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfPitchAndFamily = 0x%02X"), 
                         lf.lfPitchAndFamily)) ;

          TextOut (hdc, 0, y += cyChar, szBuffer,
               wsprintf (szBuffer, TEXT ("lfFaceName = %s"), lf.lfFaceName)) ;

          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

