/*----------------------------------------
   DIBBLE.C -- Bitmap and Palette Program
               (c) Charles Petzold, 1998
  ----------------------------------------*/

#include <windows.h>
#include "dibhelp.h"
#include "dibpal.h"
#include "dibconv.h"
#include "resource.h"

#define WM_USER_SETSCROLLS    (WM_USER + 1)
#define WM_USER_DELETEDIB     (WM_USER + 2)
#define WM_USER_DELETEPAL     (WM_USER + 3)
#define WM_USER_CREATEPAL     (WM_USER + 4)

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName[] = TEXT ("Dibble") ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     HACCEL   hAccel ;
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

     hwnd = CreateWindow (szAppName, szAppName,
                          WS_OVERLAPPEDWINDOW | WM_VSCROLL | WM_HSCROLL,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT, 
                          NULL, NULL, hInstance, NULL) ;

     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;

     hAccel = LoadAccelerators (hInstance, szAppName) ;

     while (GetMessage (&msg, NULL, 0, 0))
     {
          if (!TranslateAccelerator (hwnd, hAccel, &msg))
          {
               TranslateMessage (&msg) ;
               DispatchMessage (&msg) ;
          }
     }
     return msg.wParam ;
}

/*-------------------------------------------------------------
   DisplayDib: Displays or prints DIB actual size or stretched 
               depending on menu selection
  -------------------------------------------------------------*/

int DisplayDib (HDC hdc, HBITMAP hBitmap, int x, int y, 
                int cxClient, int cyClient, 
                WORD wShow, BOOL fHalftonePalette)
{
     BITMAP bitmap ;
     HDC    hdcMem ; 
     int    cxBitmap, cyBitmap, iReturn ;

     GetObject (hBitmap, sizeof (BITMAP), &bitmap) ;
     cxBitmap = bitmap.bmWidth ;
     cyBitmap = bitmap.bmHeight ;

     SaveDC (hdc) ;

     if (fHalftonePalette)
          SetStretchBltMode (hdc, HALFTONE) ;
     else
          SetStretchBltMode (hdc, COLORONCOLOR) ;

     hdcMem = CreateCompatibleDC (hdc) ;
     SelectObject (hdcMem, hBitmap) ;

     switch (wShow)
     {
     case IDM_SHOW_NORMAL:
          if (fHalftonePalette)
               iReturn = StretchBlt (hdc,    0, 0, 
                                             min (cxClient, cxBitmap - x), 
                                             min (cyClient, cyBitmap - y), 
                                     hdcMem, x, y, 
                                             min (cxClient, cxBitmap - x), 
                                             min (cyClient, cyBitmap - y), 
                                     SRCCOPY);
          else
               iReturn = BitBlt (hdc,    0, 0, 
                                         min (cxClient, cxBitmap - x), 
                                         min (cyClient, cyBitmap - y),
                                 hdcMem, x, y, SRCCOPY) ;
          break ;
               
     case IDM_SHOW_CENTER:
          if (fHalftonePalette)
               iReturn = StretchBlt (hdc, (cxClient - cxBitmap) / 2,
                                          (cyClient - cyBitmap) / 2, 
                                          cxBitmap, cyBitmap,
                                     hdcMem, 0, 0, cxBitmap, cyBitmap, SRCCOPY);
          else
               iReturn = BitBlt (hdc, (cxClient - cxBitmap) / 2,
                                      (cyClient - cyBitmap) / 2, 
                                      cxBitmap, cyBitmap,
                                 hdcMem, 0, 0, SRCCOPY) ;
          break ;

     case IDM_SHOW_STRETCH:
          iReturn = StretchBlt (hdc,    0, 0, cxClient, cyClient, 
                                hdcMem, 0, 0, cxBitmap, cyBitmap, SRCCOPY) ;
          break ;

     case IDM_SHOW_ISOSTRETCH:
          SetMapMode (hdc, MM_ISOTROPIC) ;
          SetWindowExtEx (hdc, cxBitmap, cyBitmap, NULL) ;
          SetViewportExtEx (hdc, cxClient, cyClient, NULL) ;
          SetWindowOrgEx (hdc, cxBitmap / 2, cyBitmap / 2, NULL) ;
          SetViewportOrgEx (hdc, cxClient / 2, cyClient / 2, NULL) ;

          iReturn = StretchBlt (hdc,    0, 0, cxBitmap, cyBitmap, 
                                hdcMem, 0, 0, cxBitmap, cyBitmap, SRCCOPY) ;
          break ;
     }
     DeleteDC (hdcMem) ;
     RestoreDC (hdc, -1) ;
     return iReturn ;
}

/*--------------------------------------------------------------------
   DibFlipHorizontal: Calls non-optimized DibSetPixel and DibGetPixel
  --------------------------------------------------------------------*/

HDIB DibFlipHorizontal (HDIB hdibSrc)
{
     HDIB hdibDst ;
     int  cx, cy, x, y ;

     if (!DibIsAddressable (hdibSrc))
          return NULL ;

     if (NULL == (hdibDst = DibCopy (hdibSrc, FALSE)))
          return NULL ;

     cx = DibWidth  (hdibSrc) ;
     cy = DibHeight (hdibSrc) ;
     
     for (x = 0 ; x < cx ; x++)
     for (y = 0 ; y < cy ; y++)
     {
          DibSetPixel (hdibDst, x, cy - 1 - y, DibGetPixel (hdibSrc, x, y)) ;
     }
     return hdibDst ;
}

/*---------------------------------------------------------------
   DibRotateRight: Calls optimized DibSetPixelx and DibGetPixelx
  ---------------------------------------------------------------*/

HDIB DibRotateRight (HDIB hdibSrc)
{
     HDIB hdibDst ;
     int  cx, cy, x, y ;

     if (!DibIsAddressable (hdibSrc))
          return NULL ;

     if (NULL == (hdibDst = DibCopy (hdibSrc, TRUE)))
          return NULL ;

     cx = DibWidth (hdibSrc) ;
     cy = DibHeight (hdibSrc) ;

     switch (DibBitCount (hdibSrc))
     {
     case  1:  
          for (x = 0 ; x < cx ; x++)
          for (y = 0 ; y < cy ; y++)
               DibSetPixel1 (hdibDst, cy - y - 1, x, 
                    DibGetPixel1 (hdibSrc, x, y)) ;
          break ;

     case  4:  
          for (x = 0 ; x < cx ; x++)
          for (y = 0 ; y < cy ; y++)
               DibSetPixel4 (hdibDst, cy - y - 1, x, 
                    DibGetPixel4 (hdibSrc, x, y)) ;
          break ;

     case  8:
          for (x = 0 ; x < cx ; x++)
          for (y = 0 ; y < cy ; y++)
               DibSetPixel8 (hdibDst, cy - y - 1, x, 
                    DibGetPixel8 (hdibSrc, x, y)) ;
          break ;

     case 16:  
          for (x = 0 ; x < cx ; x++)
          for (y = 0 ; y < cy ; y++)
               DibSetPixel16 (hdibDst, cy - y - 1, x, 
                    DibGetPixel16 (hdibSrc, x, y)) ;
          break ;

     case 24:
          for (x = 0 ; x < cx ; x++)
          for (y = 0 ; y < cy ; y++)
               DibSetPixel24 (hdibDst, cy - y - 1, x, 
                    DibGetPixel24 (hdibSrc, x, y)) ;
          break ;

     case 32:  
          for (x = 0 ; x < cx ; x++)
          for (y = 0 ; y < cy ; y++)
               DibSetPixel32 (hdibDst, cy - y - 1, x, 
                    DibGetPixel32 (hdibSrc, x, y)) ;
          break ;
     }
     return hdibDst ;
}

/*----------------------------------------------------------
   PaletteMenu: Uncheck and check menu item on palette menu
  ----------------------------------------------------------*/

void PaletteMenu (HMENU hMenu, WORD wItemNew)
{
     static WORD wItem = IDM_PAL_NONE ;

     CheckMenuItem (hMenu, wItem, MF_UNCHECKED) ;
     wItem = wItemNew ;
     CheckMenuItem (hMenu, wItem, MF_CHECKED) ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static BOOL         fHalftonePalette ;
     static DOCINFO      di = { sizeof (DOCINFO), TEXT ("Dibble: Printing") } ;
     static HBITMAP      hBitmap ;
     static HDIB         hdib ;
     static HMENU        hMenu ;
     static HPALETTE     hPalette ;
     static int          cxClient, cyClient, iVscroll, iHscroll ;
     static OPENFILENAME ofn ;
     static PRINTDLG     printdlg = { sizeof (PRINTDLG) } ;
     static TCHAR        szFileName [MAX_PATH], szTitleName [MAX_PATH] ;
     static TCHAR        szFilter[] = TEXT ("Bitmap Files (*.BMP)\0*.bmp\0")
                                      TEXT ("All Files (*.*)\0*.*\0\0") ;
     static TCHAR      * szCompression[] = { 
                           TEXT ("BI_RGB"), TEXT ("BI_RLE8"), TEXT ("BI_RLE4"), 
                           TEXT ("BI_BITFIELDS"), TEXT ("Unknown") } ;
     static WORD         wShow = IDM_SHOW_NORMAL ;
     BOOL                fSuccess ;
     BYTE              * pGlobal ;
     HDC                 hdc, hdcPrn ;
     HGLOBAL             hGlobal ;
     HDIB                hdibNew ;
     int                 iEnable, cxPage, cyPage, iConvert ;
     PAINTSTRUCT         ps ;
     SCROLLINFO          si ;
     TCHAR               szBuffer [256] ;

     switch (message)
     {
     case WM_CREATE:
         
               // Save the menu handle in a static variable

          hMenu = GetMenu (hwnd) ;

               // Initialize the OPENFILENAME structure for the File Open
               //   and File Save dialog boxes.

          ofn.lStructSize       = sizeof (OPENFILENAME) ;
          ofn.hwndOwner         = hwnd ;
          ofn.hInstance         = NULL ;
          ofn.lpstrFilter       = szFilter ;
          ofn.lpstrCustomFilter = NULL ;
          ofn.nMaxCustFilter    = 0 ;
          ofn.nFilterIndex      = 0 ;
          ofn.lpstrFile         = szFileName ;
          ofn.nMaxFile          = MAX_PATH ;
          ofn.lpstrFileTitle    = szTitleName ;
          ofn.nMaxFileTitle     = MAX_PATH ;
          ofn.lpstrInitialDir   = NULL ;
          ofn.lpstrTitle        = NULL ;
          ofn.Flags             = OFN_OVERWRITEPROMPT ;
          ofn.nFileOffset       = 0 ;
          ofn.nFileExtension    = 0 ;
          ofn.lpstrDefExt       = TEXT ("bmp") ;
          ofn.lCustData         = 0 ;
          ofn.lpfnHook          = NULL ;
          ofn.lpTemplateName    = NULL ;
          return 0 ;

     case WM_DISPLAYCHANGE:
          SendMessage (hwnd, WM_USER_DELETEPAL, 0, 0) ;
          SendMessage (hwnd, WM_USER_CREATEPAL, TRUE, 0) ;
          return 0 ;

     case WM_SIZE:
               // Save the client area width and height in static variables.

          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;

          wParam = FALSE ;
                                             // fall through

               // WM_USER_SETSCROLLS:  Programmer-defined Message!
               // Set the scroll bars. If the display mode is not normal,
               //   make them invisible. If wParam is TRUE, reset the 
               //   scroll bar position.

     case WM_USER_SETSCROLLS:
          if (hdib == NULL || wShow != IDM_SHOW_NORMAL)
          {
               si.cbSize = sizeof (SCROLLINFO) ;
               si.fMask  = SIF_RANGE ;
               si.nMin   = 0 ;
               si.nMax   = 0 ;
               SetScrollInfo (hwnd, SB_VERT, &si, TRUE) ;
               SetScrollInfo (hwnd, SB_HORZ, &si, TRUE) ;
          }
          else
          {
                    // First the vertical scroll

               si.cbSize = sizeof (SCROLLINFO) ;
               si.fMask  = SIF_ALL ;

               GetScrollInfo (hwnd, SB_VERT, &si) ;
               si.nMin  = 0 ;
               si.nMax  = DibHeight (hdib) ;
               si.nPage = cyClient ;
          
               if ((BOOL) wParam)
                    si.nPos = 0 ;

               SetScrollInfo (hwnd, SB_VERT, &si, TRUE) ;
               GetScrollInfo (hwnd, SB_VERT, &si) ;

               iVscroll = si.nPos ;

                    // Then the horizontal scroll

               GetScrollInfo (hwnd, SB_HORZ, &si) ;
               si.nMin  = 0 ;
               si.nMax  = DibWidth (hdib) ;
               si.nPage = cxClient ;
          
               if ((BOOL) wParam)
                    si.nPos = 0 ;

               SetScrollInfo (hwnd, SB_HORZ, &si, TRUE) ;
               GetScrollInfo (hwnd, SB_HORZ, &si) ;

               iHscroll = si.nPos ;
          }
          return 0 ;

          // WM_VSCROLL: Vertically scroll the DIB

     case WM_VSCROLL:
          si.cbSize = sizeof (SCROLLINFO) ;
          si.fMask  = SIF_ALL ;
          GetScrollInfo (hwnd, SB_VERT, &si) ;
          
          iVscroll = si.nPos ;

          switch (LOWORD (wParam))
          {
          case SB_LINEUP:      si.nPos -= 1 ;             break ;
          case SB_LINEDOWN:    si.nPos += 1 ;             break ;
          case SB_PAGEUP:      si.nPos -= si.nPage ;      break ;
          case SB_PAGEDOWN:    si.nPos += si.nPage ;      break ;
          case SB_THUMBTRACK:  si.nPos  = si.nTrackPos ;  break ;
          default:                                        break ;
          }

          si.fMask = SIF_POS ;
          SetScrollInfo (hwnd, SB_VERT, &si, TRUE) ;
          GetScrollInfo (hwnd, SB_VERT, &si) ;

          if (si.nPos != iVscroll)
          {
               ScrollWindow (hwnd, 0, iVscroll - si.nPos, NULL, NULL) ;
               iVscroll = si.nPos ;
               UpdateWindow (hwnd) ;
          }
          return 0 ;

          // WM_HSCROLL: Horizontally scroll the DIB

     case WM_HSCROLL:
          si.cbSize = sizeof (SCROLLINFO) ;
          si.fMask  = SIF_ALL ;
          GetScrollInfo (hwnd, SB_HORZ, &si) ;
          
          iHscroll = si.nPos ;

          switch (LOWORD (wParam))
          {
          case SB_LINELEFT:    si.nPos -= 1 ;             break ;
          case SB_LINERIGHT:   si.nPos += 1 ;             break ;
          case SB_PAGELEFT:    si.nPos -= si.nPage ;      break ;
          case SB_PAGERIGHT:   si.nPos += si.nPage ;      break ;
          case SB_THUMBTRACK:  si.nPos  = si.nTrackPos ;  break ;
          default:                                        break ;
          }

          si.fMask = SIF_POS ;
          SetScrollInfo (hwnd, SB_HORZ, &si, TRUE) ;
          GetScrollInfo (hwnd, SB_HORZ, &si) ;

          if (si.nPos != iHscroll)
          {
               ScrollWindow (hwnd, iHscroll - si.nPos, 0, NULL, NULL) ;
               iHscroll = si.nPos ;
               UpdateWindow (hwnd) ;
          }
          return 0 ;

          // WM_INITMENUPOPUP:  Enable or Gray menu items

     case WM_INITMENUPOPUP:
          if (hdib)
               iEnable = MF_ENABLED ;
          else
               iEnable = MF_GRAYED ;

          EnableMenuItem (hMenu, IDM_FILE_SAVE,       iEnable) ;
          EnableMenuItem (hMenu, IDM_FILE_PRINT,      iEnable) ;
          EnableMenuItem (hMenu, IDM_FILE_PROPERTIES, iEnable) ;
          EnableMenuItem (hMenu, IDM_EDIT_CUT,        iEnable) ;
          EnableMenuItem (hMenu, IDM_EDIT_COPY,       iEnable) ;
          EnableMenuItem (hMenu, IDM_EDIT_DELETE,     iEnable) ;

          if (DibIsAddressable (hdib))
               iEnable = MF_ENABLED ;
          else
               iEnable = MF_GRAYED ;

          EnableMenuItem (hMenu, IDM_EDIT_ROTATE,    iEnable) ;
          EnableMenuItem (hMenu, IDM_EDIT_FLIP,      iEnable) ;
          EnableMenuItem (hMenu, IDM_CONVERT_01,     iEnable) ;
          EnableMenuItem (hMenu, IDM_CONVERT_04,     iEnable) ;
          EnableMenuItem (hMenu, IDM_CONVERT_08,     iEnable) ;
          EnableMenuItem (hMenu, IDM_CONVERT_16,     iEnable) ;
          EnableMenuItem (hMenu, IDM_CONVERT_24,     iEnable) ;
          EnableMenuItem (hMenu, IDM_CONVERT_32,     iEnable) ;

          switch (DibBitCount (hdib))
          {
          case  1: EnableMenuItem (hMenu, IDM_CONVERT_01, MF_GRAYED) ; break ;
          case  4: EnableMenuItem (hMenu, IDM_CONVERT_04, MF_GRAYED) ; break ;
          case  8: EnableMenuItem (hMenu, IDM_CONVERT_08, MF_GRAYED) ; break ;
          case 16: EnableMenuItem (hMenu, IDM_CONVERT_16, MF_GRAYED) ; break ;
          case 24: EnableMenuItem (hMenu, IDM_CONVERT_24, MF_GRAYED) ; break ;
          case 32: EnableMenuItem (hMenu, IDM_CONVERT_32, MF_GRAYED) ; break ;
          }

          if (hdib && DibColorSize (hdib) > 0)
               iEnable = MF_ENABLED ;
          else
               iEnable = MF_GRAYED ;

          EnableMenuItem (hMenu, IDM_PAL_DIBTABLE,    iEnable) ;

          if (DibIsAddressable (hdib) && DibBitCount (hdib) > 8)
               iEnable = MF_ENABLED ;
          else
               iEnable = MF_GRAYED ;

          EnableMenuItem (hMenu, IDM_PAL_OPT_POP4,   iEnable) ;
          EnableMenuItem (hMenu, IDM_PAL_OPT_POP5,   iEnable) ;
          EnableMenuItem (hMenu, IDM_PAL_OPT_POP6,   iEnable) ;
          EnableMenuItem (hMenu, IDM_PAL_OPT_MEDCUT, iEnable) ;

          EnableMenuItem (hMenu, IDM_EDIT_PASTE, 
               IsClipboardFormatAvailable (CF_DIB) ? MF_ENABLED : MF_GRAYED) ;

          return 0 ;

          // WM_COMMAND:  Process all menu commands.

     case WM_COMMAND:
          iConvert = 0 ;

          switch (LOWORD (wParam))
          {
          case IDM_FILE_OPEN:

                    // Show the File Open dialog box

               if (!GetOpenFileName (&ofn))
                    return 0 ;
               
                    // If there's an existing DIB and palette, delete them

               SendMessage (hwnd, WM_USER_DELETEDIB, 0, 0) ;
               
                    // Load the DIB into memory

               SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
               ShowCursor (TRUE) ;

               hdib = DibFileLoad (szFileName) ;

               ShowCursor (FALSE) ;
               SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

                    // Reset the scroll bars

               SendMessage (hwnd, WM_USER_SETSCROLLS, TRUE, 0) ;

                    // Create the palette and DDB

               SendMessage (hwnd, WM_USER_CREATEPAL, TRUE, 0) ;

               if (!hdib)
               {
                    MessageBox (hwnd, TEXT ("Cannot load DIB file!"), 
                                szAppName, MB_OK | MB_ICONEXCLAMATION) ;
               }
               InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;

          case IDM_FILE_SAVE:

                    // Show the File Save dialog box

               if (!GetSaveFileName (&ofn))
                    return 0 ;

                    // Save the DIB to memory

               SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
               ShowCursor (TRUE) ;

               fSuccess = DibFileSave (hdib, szFileName) ;

               ShowCursor (FALSE) ;
               SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

               if (!fSuccess)
                    MessageBox (hwnd, TEXT ("Cannot save DIB file!"),
                                szAppName, MB_OK | MB_ICONEXCLAMATION) ;
               return 0 ;

          case IDM_FILE_PRINT:
               if (!hdib)
                    return 0 ;

                    // Get printer DC

          	printdlg.Flags = PD_RETURNDC | PD_NOPAGENUMS | PD_NOSELECTION ;

	          if (!PrintDlg (&printdlg))
                    return 0 ;

               if (NULL == (hdcPrn = printdlg.hDC))
               {
                    MessageBox (hwnd, TEXT ("Cannot obtain Printer DC"),
                                szAppName, MB_ICONEXCLAMATION | MB_OK) ;
                    return 0 ;
               }
                    // Check if the printer can print bitmaps

               if (!(RC_BITBLT & GetDeviceCaps (hdcPrn, RASTERCAPS)))
               {
                    DeleteDC (hdcPrn) ;
                    MessageBox (hwnd, TEXT ("Printer cannot print bitmaps"),
                                szAppName, MB_ICONEXCLAMATION | MB_OK) ;
                    return 0 ;
               }
                    // Get size of printable area of page

               cxPage = GetDeviceCaps (hdcPrn, HORZRES) ;
               cyPage = GetDeviceCaps (hdcPrn, VERTRES) ;

               fSuccess = FALSE ;

                    // Send the DIB to the printer

               SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
               ShowCursor (TRUE) ;

               if ((StartDoc (hdcPrn, &di) > 0) && (StartPage (hdcPrn) > 0))
               {
                    DisplayDib (hdcPrn, DibBitmapHandle (hdib), 0, 0, 
                                cxPage, cyPage, wShow, FALSE) ;
                    
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
                    MessageBox (hwnd, TEXT ("Could not print bitmap"),
                                szAppName, MB_ICONEXCLAMATION | MB_OK) ;
               return 0 ;

          case IDM_FILE_PROPERTIES:
               if (!hdib)
                    return 0 ;

               wsprintf (szBuffer, TEXT ("Pixel width:\t%i\n")
                                   TEXT ("Pixel height:\t%i\n")
                                   TEXT ("Bits per pixel:\t%i\n")
                                   TEXT ("Number of colors:\t%i\n")
                                   TEXT ("Compression:\t%s\n"),
                         DibWidth (hdib), DibHeight (hdib),
                         DibBitCount (hdib), DibNumColors (hdib),
                         szCompression [min (3, DibCompression (hdib))]) ;

               MessageBox (hwnd, szBuffer, szAppName, 
                           MB_ICONEXCLAMATION | MB_OK) ;
               return 0 ;

          case IDM_APP_EXIT:
               SendMessage (hwnd, WM_CLOSE, 0, 0) ;
               return 0 ;

          case IDM_EDIT_COPY:
          case IDM_EDIT_CUT:
               if (!(hGlobal = DibCopyToPackedDib (hdib, TRUE)))
                    return 0 ;

               OpenClipboard (hwnd) ;
               EmptyClipboard () ;
               SetClipboardData (CF_DIB, hGlobal) ;
               CloseClipboard () ;

               if (LOWORD (wParam) == IDM_EDIT_COPY)
                    return 0 ;
                                   // fall through for IDM_EDIT_CUT
          case IDM_EDIT_DELETE:
               SendMessage (hwnd, WM_USER_DELETEDIB, 0, 0) ;
               InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;

          case IDM_EDIT_PASTE:
               OpenClipboard (hwnd) ;

               hGlobal = GetClipboardData (CF_DIB) ;
               pGlobal = GlobalLock (hGlobal) ;

                    // If there's an existing DIB and palette, delete them.
                    // Then convert the packed DIB to an HDIB.

               if (pGlobal)
               {
                    SendMessage (hwnd, WM_USER_DELETEDIB, 0, 0) ;
                    hdib = DibCopyFromPackedDib ((BITMAPINFO *) pGlobal) ;
                    SendMessage (hwnd, WM_USER_CREATEPAL, TRUE, 0) ;
               }

               GlobalUnlock (hGlobal) ;
               CloseClipboard () ;

                    // Reset the scroll bars

               SendMessage (hwnd, WM_USER_SETSCROLLS, TRUE, 0) ;
               InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;

          case IDM_EDIT_ROTATE:
               if (hdibNew = DibRotateRight (hdib))
               {
                    DibDelete (hdib) ;
                    DeleteObject (hBitmap) ;
                    hdib = hdibNew ;
                    hBitmap = DibCopyToDdb (hdib, hwnd, hPalette) ;
                    SendMessage (hwnd, WM_USER_SETSCROLLS, TRUE, 0) ;
                    InvalidateRect (hwnd, NULL, TRUE) ;
               }
               else
               {
                    MessageBox (hwnd, TEXT ("Not enough memory"),
                                szAppName, MB_OK | MB_ICONEXCLAMATION) ;
               }
               return 0 ;

          case IDM_EDIT_FLIP:
               if (hdibNew = DibFlipHorizontal (hdib))
               {
                    DibDelete (hdib) ;
                    DeleteObject (hBitmap) ;
                    hdib = hdibNew ;
                    hBitmap = DibCopyToDdb (hdib, hwnd, hPalette) ;
                    InvalidateRect (hwnd, NULL, TRUE) ;
               }
               else
               {
                    MessageBox (hwnd, TEXT ("Not enough memory"),
                                szAppName, MB_OK | MB_ICONEXCLAMATION) ;
               }
               return 0 ;

          case IDM_SHOW_NORMAL:
          case IDM_SHOW_CENTER:
          case IDM_SHOW_STRETCH:
          case IDM_SHOW_ISOSTRETCH:
               CheckMenuItem (hMenu, wShow, MF_UNCHECKED) ;
               wShow = LOWORD (wParam) ;
               CheckMenuItem (hMenu, wShow, MF_CHECKED) ;

               SendMessage (hwnd, WM_USER_SETSCROLLS, FALSE, 0) ;

               InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;

          case IDM_CONVERT_32:  iConvert += 8 ;
          case IDM_CONVERT_24:  iConvert += 8 ;   
          case IDM_CONVERT_16:  iConvert += 8 ;
          case IDM_CONVERT_08:  iConvert += 4 ;
          case IDM_CONVERT_04:  iConvert += 3 ;
          case IDM_CONVERT_01:  iConvert += 1 ;
               SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
               ShowCursor (TRUE) ;

               hdibNew = DibConvert (hdib, iConvert) ;

               ShowCursor (FALSE) ;
               SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

               if (hdibNew)
               {
                    SendMessage (hwnd, WM_USER_DELETEDIB, 0, 0) ;
                    hdib = hdibNew ;
                    SendMessage (hwnd, WM_USER_CREATEPAL, TRUE, 0) ;
                    InvalidateRect (hwnd, NULL, TRUE) ;
               }
               else
               {
                    MessageBox (hwnd, TEXT ("Not enough memory"),
                                szAppName, MB_OK | MB_ICONEXCLAMATION) ;
               }
               return 0 ;

          case IDM_APP_ABOUT:
               MessageBox (hwnd, TEXT ("Dibble (c) Charles Petzold, 1998"),
                           szAppName, MB_OK | MB_ICONEXCLAMATION) ;
               return 0 ;
          }
     
               // All the other WM_COMMAND messages are from the palette
               //   items. Any existing palette is deleted, and the cursor
               //   is set to the hourglass.

          SendMessage (hwnd, WM_USER_DELETEPAL, 0, 0) ;
          SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
          ShowCursor (TRUE) ;

               // Notice that all messages for palette items are ended
               //   with break rather than return. This is to allow 
               //   additional processing later on.

          switch (LOWORD (wParam))
          {
          case IDM_PAL_DIBTABLE: 
               hPalette = DibPalDibTable (hdib) ; 
               break ;

          case IDM_PAL_HALFTONE: 
               hdc = GetDC (hwnd) ;

               if (hPalette = CreateHalftonePalette (hdc))
                    fHalftonePalette = TRUE ;

               ReleaseDC (hwnd, hdc) ;
               break ;

          case IDM_PAL_ALLPURPOSE: 
               hPalette = DibPalAllPurpose () ; 
               break ;

          case IDM_PAL_GRAY2:   hPalette = DibPalUniformGrays (  2) ; break ;
          case IDM_PAL_GRAY3:   hPalette = DibPalUniformGrays (  3) ; break ;
          case IDM_PAL_GRAY4:   hPalette = DibPalUniformGrays (  4) ; break ;
          case IDM_PAL_GRAY8:   hPalette = DibPalUniformGrays (  8) ; break ;
          case IDM_PAL_GRAY16:  hPalette = DibPalUniformGrays ( 16) ; break ;
          case IDM_PAL_GRAY32:  hPalette = DibPalUniformGrays ( 32) ; break ;
          case IDM_PAL_GRAY64:  hPalette = DibPalUniformGrays ( 64) ; break ;
          case IDM_PAL_GRAY128: hPalette = DibPalUniformGrays (128) ; break ;
          case IDM_PAL_GRAY256: hPalette = DibPalUniformGrays (256) ; break ;

          case IDM_PAL_RGB222: hPalette = DibPalUniformColors (2,2,2); break;
          case IDM_PAL_RGB333: hPalette = DibPalUniformColors (3,3,3); break;
          case IDM_PAL_RGB444: hPalette = DibPalUniformColors (4,4,4); break;
          case IDM_PAL_RGB555: hPalette = DibPalUniformColors (5,5,5); break;
          case IDM_PAL_RGB666: hPalette = DibPalUniformColors (6,6,6); break;
          case IDM_PAL_RGB775: hPalette = DibPalUniformColors (7,7,5); break;
          case IDM_PAL_RGB757: hPalette = DibPalUniformColors (7,5,7); break;
          case IDM_PAL_RGB577: hPalette = DibPalUniformColors (5,7,7); break;
          case IDM_PAL_RGB884: hPalette = DibPalUniformColors (8,8,4); break;
          case IDM_PAL_RGB848: hPalette = DibPalUniformColors (8,4,8); break;
          case IDM_PAL_RGB488: hPalette = DibPalUniformColors (4,8,8); break;

          case IDM_PAL_OPT_POP4:  
               hPalette = DibPalPopularity (hdib, 4) ;
               break ;

          case IDM_PAL_OPT_POP5:  
               hPalette = DibPalPopularity (hdib, 5) ;
               break ;

          case IDM_PAL_OPT_POP6:  
               hPalette = DibPalPopularity (hdib, 6) ;
               break ;                   

          case IDM_PAL_OPT_MEDCUT:
               hPalette = DibPalMedianCut (hdib, 6) ;
               break ;
          }

               // After processing Palette items from the menu, the cursor
               //   is restored to an arrow, the menu item is checked, and
               //   the window is invalidated.

          hBitmap = DibCopyToDdb (hdib, hwnd, hPalette) ;

          ShowCursor (FALSE) ;
          SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

          if (hPalette)
               PaletteMenu (hMenu, (LOWORD (wParam))) ;

          InvalidateRect (hwnd, NULL, TRUE) ;
          return 0 ;

          // This programmer-defined message deletes an existing DIB 
          //   in preparation for getting a new one.  Invoked during 
          //   File Open command, Edit Paste command, and others.

     case WM_USER_DELETEDIB:
          if (hdib)
          {
               DibDelete (hdib) ;
               hdib = NULL ;
          }
          SendMessage (hwnd, WM_USER_DELETEPAL, 0, 0) ;
          return 0 ;

          // This programmer-defined message deletes an existing palette
          //   in preparation for defining a new one.

     case WM_USER_DELETEPAL:
          if (hPalette)
          {
               DeleteObject (hPalette) ;
               hPalette = NULL ;
               fHalftonePalette = FALSE ;
               PaletteMenu (hMenu, IDM_PAL_NONE) ;
          }
          if (hBitmap)
               DeleteObject (hBitmap) ;

          return 0 ;

          // Programmers-defined message to create a new palette based on 
          //   a new DIB.  If wParam == TRUE, create a DDB as well.

     case WM_USER_CREATEPAL:
          if (hdib)
          {
               hdc = GetDC (hwnd) ;

               if (!(RC_PALETTE & GetDeviceCaps (hdc, RASTERCAPS)))
               {
                    PaletteMenu (hMenu, IDM_PAL_NONE) ;
               }
               else if (hPalette = DibPalDibTable (hdib))
               {
                    PaletteMenu (hMenu, IDM_PAL_DIBTABLE) ;
               }
               else if (hPalette = CreateHalftonePalette (hdc))
               {
                    fHalftonePalette = TRUE ;
                    PaletteMenu (hMenu, IDM_PAL_HALFTONE) ;
               }
               ReleaseDC (hwnd, hdc) ;

               if ((BOOL) wParam)
                    hBitmap = DibCopyToDdb (hdib, hwnd, hPalette) ;
          }
          return 0 ;

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;

          if (hPalette)
          {
               SelectPalette (hdc, hPalette, FALSE) ;
               RealizePalette (hdc) ;
          }
          if (hBitmap)
          {
               DisplayDib (hdc, 
                           fHalftonePalette ? DibBitmapHandle (hdib) : hBitmap, 
                           iHscroll, iVscroll, 
                           cxClient, cyClient, 
                           wShow, fHalftonePalette) ;
          }
          EndPaint (hwnd, &ps) ;
          return 0 ;

     case WM_QUERYNEWPALETTE:
          if (!hPalette)
               return FALSE ;

          hdc = GetDC (hwnd) ;
          SelectPalette (hdc, hPalette, FALSE) ;
          RealizePalette (hdc) ;
          InvalidateRect (hwnd, NULL, TRUE) ;

          ReleaseDC (hwnd, hdc) ;
          return TRUE ;

     case WM_PALETTECHANGED:
          if (!hPalette || (HWND) wParam == hwnd)
               break ;

          hdc = GetDC (hwnd) ;
          SelectPalette (hdc, hPalette, FALSE) ;
          RealizePalette (hdc) ;
          UpdateColors (hdc) ;

          ReleaseDC (hwnd, hdc) ;
          break ;

     case WM_DESTROY:
          if (hdib)
               DibDelete (hdib) ;

          if (hBitmap)
               DeleteObject (hBitmap) ;

          if (hPalette)
               DeleteObject (hPalette) ;

          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

