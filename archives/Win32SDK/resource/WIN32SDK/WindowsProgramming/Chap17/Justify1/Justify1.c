/*-----------------------------------------
   JUSTIFY1.C -- Justified Type Program #1
                 (c) Charles Petzold, 1998
  -----------------------------------------*/

#include <windows.h>
#include "resource.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName[] = TEXT ("Justify1") ;

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
     
     hwnd = CreateWindow (szAppName, TEXT ("Justified Type #1"),
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

void DrawRuler (HDC hdc, RECT * prc)
{
     static int iRuleSize [16] = { 360, 72, 144, 72, 216, 72, 144, 72,
                                   288, 72, 144, 72, 216, 72, 144, 72 } ;
     int        i, j ;
     POINT      ptClient ;
     
     SaveDC (hdc) ;
     
          // Set Logical Twips mapping mode
     
     SetMapMode (hdc, MM_ANISOTROPIC) ;
     SetWindowExtEx (hdc, 1440, 1440, NULL) ;
     SetViewportExtEx (hdc, GetDeviceCaps (hdc, LOGPIXELSX),
                            GetDeviceCaps (hdc, LOGPIXELSY), NULL) ;
     
          // Move the origin to a half inch from upper left
     
     SetWindowOrgEx (hdc, -720, -720, NULL) ;
     
          // Find the right margin (quarter inch from right)
     
     ptClient.x = prc->right ;
     ptClient.y = prc->bottom ;
     DPtoLP (hdc, &ptClient, 1) ;
     ptClient.x -= 360 ;
     
          // Draw the rulers
     
     MoveToEx (hdc, 0,          -360, NULL) ;
     LineTo   (hdc, ptClient.x, -360) ;
     MoveToEx (hdc, -360,          0, NULL) ;
     LineTo   (hdc, -360, ptClient.y) ;
     
     for (i = 0, j = 0 ; i <= ptClient.x ; i += 1440 / 16, j++)
     {
          MoveToEx (hdc, i, -360, NULL) ;
          LineTo   (hdc, i, -360 - iRuleSize [j % 16]) ;
     }
     
     for (i = 0, j = 0 ; i <= ptClient.y ; i += 1440 / 16, j++)
     {
          MoveToEx (hdc, -360, i, NULL) ;
          LineTo   (hdc, -360 - iRuleSize [j % 16], i) ;
     }
     
     RestoreDC (hdc, -1) ;
}

void Justify (HDC hdc, PTSTR pText, RECT * prc, int iAlign)
{
     int   xStart, yStart, cSpaceChars ;
     PTSTR pBegin, pEnd ;
     SIZE  size ;
     
     yStart = prc->top ;
     do                            // for each text line
     {
          cSpaceChars = 0 ;        // initialize number of spaces in line

          while (*pText == ' ')    // skip over leading spaces
               pText++ ;

          pBegin = pText ;         // set pointer to char at beginning of line
          
          do                       // until the line is known
          {
               pEnd = pText ;      // set pointer to char at end of line

                    // skip to next space 
               
               while (*pText != '\0' && *pText++ != ' ') ;

               if (*pText == '\0')
                    break ;

                    // after each space encountered, calculate extents

               cSpaceChars++ ;
               GetTextExtentPoint32(hdc, pBegin, pText - pBegin - 1, &size) ;
          }
          while (size.cx < (prc->right - prc->left)) ;
          
          cSpaceChars-- ;               // discount last space at end of line
          
          while (*(pEnd - 1) == ' ')    // eliminate trailing spaces
          {
               pEnd-- ;
               cSpaceChars-- ;
          }

               // if end of text and no space characters, set pEnd to end
          
          if (*pText == '\0' || cSpaceChars <= 0)
               pEnd = pText ;
          
          GetTextExtentPoint32 (hdc, pBegin, pEnd - pBegin, &size) ;
          
          switch (iAlign)               // use alignment for xStart
          {
          case IDM_ALIGN_LEFT:
               xStart = prc->left ;
               break ;
               
          case IDM_ALIGN_RIGHT:
               xStart = prc->right - size.cx ;
               break ;
               
          case IDM_ALIGN_CENTER:
               xStart = (prc->right + prc->left - size.cx) / 2 ;
               break ;
               
          case IDM_ALIGN_JUSTIFIED:
               if (*pText != '\0' && cSpaceChars > 0)
                    SetTextJustification (hdc,
                                          prc->right - prc->left - size.cx,
                                          cSpaceChars) ;
               xStart = prc->left ;
               break ;
          }
               // display the text
          
          TextOut (hdc, xStart, yStart, pBegin, pEnd - pBegin) ;

               // prepare for next line

          SetTextJustification (hdc, 0, 0) ;
          yStart += size.cy ;
          pText = pEnd ;
     }
     while (*pText && yStart < prc->bottom - size.cy) ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static CHOOSEFONT cf ;
     static DOCINFO    di = { sizeof (DOCINFO), TEXT ("Justify1: Printing") } ;
     static int        iAlign = IDM_ALIGN_LEFT ;
     static LOGFONT    lf ;
     static PRINTDLG   pd ;
     static TCHAR      szText[] = { 
                              TEXT ("You don't know about me, without you ")
                              TEXT ("have read a book by the name of \"The ")
                              TEXT ("Adventures of Tom Sawyer,\" but that ")
                              TEXT ("ain't no matter. That book was made by ")
                              TEXT ("Mr. Mark Twain, and he told the truth, ")
                              TEXT ("mainly. There was things which he ")
                              TEXT ("stretched, but mainly he told the truth. ")
                              TEXT ("That is nothing. I never seen anybody ")
                              TEXT ("but lied, one time or another, without ")
                              TEXT ("it was Aunt Polly, or the widow, or ")
                              TEXT ("maybe Mary. Aunt Polly -- Tom's Aunt ")
                              TEXT ("Polly, she is -- and Mary, and the Widow ")
                              TEXT ("Douglas, is all told about in that book ")
                              TEXT ("-- which is mostly a true book; with ")
                              TEXT ("some stretchers, as I said before.") } ;
     BOOL              fSuccess ;
     HDC               hdc, hdcPrn ;
     HMENU             hMenu ;
     int               iSavePointSize ;
     PAINTSTRUCT       ps ;
     RECT              rect ;
     
     switch (message)
     {
     case WM_CREATE:
               // Initialize the CHOOSEFONT structure

          GetObject (GetStockObject (SYSTEM_FONT), sizeof (lf), &lf) ;

          cf.lStructSize    = sizeof (CHOOSEFONT) ;
          cf.hwndOwner      = hwnd ;
          cf.hDC            = NULL ;
          cf.lpLogFont      = &lf ;
          cf.iPointSize     = 0 ;
          cf.Flags          = CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS | 
                              CF_EFFECTS ;
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
          hMenu = GetMenu (hwnd) ;
          
          switch (LOWORD (wParam))
          {
          case IDM_FILE_PRINT:
                                   // Get printer DC

               pd.lStructSize = sizeof (PRINTDLG) ;
               pd.hwndOwner   = hwnd ;
          	pd.Flags       = PD_RETURNDC | PD_NOPAGENUMS | PD_NOSELECTION ;

	          if (!PrintDlg (&pd))
                    return 0 ;

               if (NULL == (hdcPrn = pd.hDC))
               {
                    MessageBox (hwnd, TEXT ("Cannot obtain Printer DC"),
                                szAppName, MB_ICONEXCLAMATION | MB_OK) ;
                    return 0 ;
               }
                    // Set margins of 1 inch

               rect.left   = GetDeviceCaps (hdcPrn, LOGPIXELSX) -
                             GetDeviceCaps (hdcPrn, PHYSICALOFFSETX) ;

               rect.top    = GetDeviceCaps (hdcPrn, LOGPIXELSY) -
                             GetDeviceCaps (hdcPrn, PHYSICALOFFSETY) ;

               rect.right  = GetDeviceCaps (hdcPrn, PHYSICALWIDTH) -
                             GetDeviceCaps (hdcPrn, LOGPIXELSX) -
                             GetDeviceCaps (hdcPrn, PHYSICALOFFSETX) ;

               rect.bottom = GetDeviceCaps (hdcPrn, PHYSICALHEIGHT) - 
                             GetDeviceCaps (hdcPrn, LOGPIXELSY) -
                             GetDeviceCaps (hdcPrn, PHYSICALOFFSETY) ;

                    // Display text on printer

               SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
               ShowCursor (TRUE) ;

               fSuccess = FALSE ;

               if ((StartDoc (hdcPrn, &di) > 0) && (StartPage (hdcPrn) > 0))
               {
                         // Select font using adjusted lfHeight

                    iSavePointSize = lf.lfHeight ;
                    lf.lfHeight = -(GetDeviceCaps (hdcPrn, LOGPIXELSY) *
                                         cf.iPointSize) / 720 ;

                    SelectObject (hdcPrn, CreateFontIndirect (&lf)) ;
                    lf.lfHeight = iSavePointSize ;

                         // Set text color 

                    SetTextColor (hdcPrn, cf.rgbColors) ;
               
                         // Display text

                    Justify (hdcPrn, szText, &rect, iAlign) ;

                    if (EndPage (hdcPrn) > 0)
                    {
                         fSuccess = TRUE ;
                         EndDoc (hdcPrn) ;
                    }
               }
               ShowCursor (FALSE) ;
               SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

               DeleteDC (hdcPrn) ;

               if (!fSuccess)
                    MessageBox (hwnd, TEXT ("Could not print text"),
                                szAppName, MB_ICONEXCLAMATION | MB_OK) ;
               return 0 ;

          case IDM_FONT:
               if (ChooseFont (&cf))
                    InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;
               
          case IDM_ALIGN_LEFT:
          case IDM_ALIGN_RIGHT:
          case IDM_ALIGN_CENTER:
          case IDM_ALIGN_JUSTIFIED:
               CheckMenuItem (hMenu, iAlign, MF_UNCHECKED) ;
               iAlign = LOWORD (wParam) ;
               CheckMenuItem (hMenu, iAlign, MF_CHECKED) ;
               InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;
          }
          return 0 ;

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          GetClientRect (hwnd, &rect) ;
          DrawRuler (hdc, &rect) ;
          
          rect.left  += GetDeviceCaps (hdc, LOGPIXELSX) / 2 ;
          rect.top   += GetDeviceCaps (hdc, LOGPIXELSY) / 2 ;
          rect.right -= GetDeviceCaps (hdc, LOGPIXELSX) / 4 ;

          SelectObject (hdc, CreateFontIndirect (&lf)) ;
          SetTextColor (hdc, cf.rgbColors) ;
          
          Justify (hdc, szText, &rect, iAlign) ;
          
          DeleteObject (SelectObject (hdc, GetStockObject (SYSTEM_FONT)));
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}