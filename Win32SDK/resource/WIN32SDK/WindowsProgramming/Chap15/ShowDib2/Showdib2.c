/*----------------------------------------------
   SHOWDIB2.C -- Shows a DIB in the client area
                 (c) Charles Petzold, 1998
  ----------------------------------------------*/

#include <windows.h>
#include "dibfile.h"
#include "resource.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName[] = TEXT ("ShowDib2") ;

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

     hwnd = CreateWindow (szAppName, TEXT ("Show DIB #2"),
                          WS_OVERLAPPEDWINDOW,
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

int ShowDib (HDC hdc, BITMAPINFO * pbmi, BYTE * pBits, int cxDib, int cyDib, 
             int cxClient, int cyClient, WORD wShow)
{
     switch (wShow)
     {
     case IDM_SHOW_NORMAL:
          return SetDIBitsToDevice (hdc, 0, 0, cxDib, cyDib, 0, 0, 
                                    0, cyDib, pBits, pbmi, DIB_RGB_COLORS) ;
               
     case IDM_SHOW_CENTER:
          return SetDIBitsToDevice (hdc, (cxClient - cxDib) / 2,
                                         (cyClient - cyDib) / 2, 
                                    cxDib, cyDib, 0, 0, 
                                    0, cyDib, pBits, pbmi, DIB_RGB_COLORS) ;

     case IDM_SHOW_STRETCH:
          SetStretchBltMode (hdc, COLORONCOLOR) ;

          return StretchDIBits (hdc, 0, 0, cxClient, cyClient, 
                                     0, 0, cxDib, cyDib,
                                pBits, pbmi, DIB_RGB_COLORS, SRCCOPY) ;

     case IDM_SHOW_ISOSTRETCH:
          SetStretchBltMode (hdc, COLORONCOLOR) ;
          SetMapMode (hdc, MM_ISOTROPIC) ;
          SetWindowExtEx (hdc, cxDib, cyDib, NULL) ;
          SetViewportExtEx (hdc, cxClient, cyClient, NULL) ;
          SetWindowOrgEx (hdc, cxDib / 2, cyDib / 2, NULL) ;
          SetViewportOrgEx (hdc, cxClient / 2, cyClient / 2, NULL) ;

          return StretchDIBits (hdc, 0, 0, cxDib, cyDib, 
                                     0, 0, cxDib, cyDib,
                                pBits, pbmi, DIB_RGB_COLORS, SRCCOPY) ;
     }
     return 0 ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static BITMAPFILEHEADER * pbmfh ;
     static BITMAPINFO       * pbmi ;
     static BYTE             * pBits ;
     static DOCINFO            di = { sizeof (DOCINFO), 
                                      TEXT ("ShowDib2: Printing") } ;
     static int                cxClient, cyClient, cxDib, cyDib ;
     static PRINTDLG           printdlg = { sizeof (PRINTDLG) } ;
     static TCHAR              szFileName [MAX_PATH], szTitleName [MAX_PATH] ;
     static WORD               wShow = IDM_SHOW_NORMAL ;
     BOOL                      bSuccess ;
     HDC                       hdc, hdcPrn ;
     HGLOBAL                   hGlobal ;
     HMENU                     hMenu ;
     int                       cxPage, cyPage, iEnable ;
     PAINTSTRUCT               ps ;
     BYTE                    * pGlobal ;

     switch (message)
     {
     case WM_CREATE:
          DibFileInitialize (hwnd) ;
          return 0 ;

     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;

     case WM_INITMENUPOPUP:
          hMenu = GetMenu (hwnd) ;

          if (pbmfh)
               iEnable = MF_ENABLED ;
          else
               iEnable = MF_GRAYED ;

          EnableMenuItem (hMenu, IDM_FILE_SAVE,   iEnable) ;
          EnableMenuItem (hMenu, IDM_FILE_PRINT,  iEnable) ;
          EnableMenuItem (hMenu, IDM_EDIT_CUT,    iEnable) ;
          EnableMenuItem (hMenu, IDM_EDIT_COPY,   iEnable) ;
          EnableMenuItem (hMenu, IDM_EDIT_DELETE, iEnable) ;
          return 0 ;

     case WM_COMMAND:
          hMenu = GetMenu (hwnd) ;

          switch (LOWORD (wParam))
          {
          case IDM_FILE_OPEN:
                    // Show the File Open dialog box

               if (!DibFileOpenDlg (hwnd, szFileName, szTitleName))
                    return 0 ;
               
                    // If there's an existing DIB, free the memory

               if (pbmfh)
               {
                    free (pbmfh) ;
                    pbmfh = NULL ;
               }
                    // Load the entire DIB into memory

               SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
               ShowCursor (TRUE) ;

               pbmfh = DibLoadImage (szFileName) ;

               ShowCursor (FALSE) ;
               SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

                    // Invalidate the client area for later update

               InvalidateRect (hwnd, NULL, TRUE) ;

               if (pbmfh == NULL)
               {
                    MessageBox (hwnd, TEXT ("Cannot load DIB file"), 
                                szAppName, MB_ICONEXCLAMATION | MB_OK) ;
                    return 0 ;
               }
                    // Get pointers to the info structure & the bits

               pbmi  = (BITMAPINFO *) (pbmfh + 1) ;
               pBits = (BYTE *) pbmfh + pbmfh->bfOffBits ;

                    // Get the DIB width and height

               if (pbmi->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
               {
                    cxDib = ((BITMAPCOREHEADER *) pbmi)->bcWidth ;
                    cyDib = ((BITMAPCOREHEADER *) pbmi)->bcHeight ;
               }
               else
               {
                    cxDib =      pbmi->bmiHeader.biWidth ;
                    cyDib = abs (pbmi->bmiHeader.biHeight) ;
               }
               return 0 ;

          case IDM_FILE_SAVE:
                    // Show the File Save dialog box

               if (!DibFileSaveDlg (hwnd, szFileName, szTitleName))
                    return 0 ;
               
                    // Save the DIB to a disk file

               SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
               ShowCursor (TRUE) ;

               bSuccess = DibSaveImage (szFileName, pbmfh) ;

               ShowCursor (FALSE) ;
               SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

               if (!bSuccess)
                    MessageBox (hwnd, TEXT ("Cannot save DIB file"), 
                                szAppName, MB_ICONEXCLAMATION | MB_OK) ;
               return 0 ;

          case IDM_FILE_PRINT:
               if (!pbmfh)
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

               bSuccess = FALSE ;

                    // Send the DIB to the printer

               SetCursor (LoadCursor (NULL, IDC_WAIT)) ;
               ShowCursor (TRUE) ;

               if ((StartDoc (hdcPrn, &di) > 0) && (StartPage (hdcPrn) > 0))
               {
                    ShowDib (hdcPrn, pbmi, pBits, cxDib, cyDib,
                             cxPage, cyPage, wShow) ;
                    
                    if (EndPage (hdcPrn) > 0)
                    {
                         bSuccess = TRUE ;
                         EndDoc (hdcPrn) ;
                    }
               }
               ShowCursor (FALSE) ;
               SetCursor (LoadCursor (NULL, IDC_ARROW)) ;

               DeleteDC (hdcPrn) ;

               if (!bSuccess)
                    MessageBox (hwnd, TEXT ("Could not print bitmap"),
                                szAppName, MB_ICONEXCLAMATION | MB_OK) ;
               return 0 ;

          case IDM_EDIT_COPY:
          case IDM_EDIT_CUT:
               if (!pbmfh)
                    return 0 ;

                    // Make a copy of the packed DIB

               hGlobal = GlobalAlloc (GHND | GMEM_SHARE, pbmfh->bfSize -
                                        sizeof (BITMAPFILEHEADER)) ;

               pGlobal = GlobalLock (hGlobal) ;

               CopyMemory (pGlobal, (BYTE *) pbmfh + sizeof (BITMAPFILEHEADER),
                           pbmfh->bfSize - sizeof (BITMAPFILEHEADER)) ;

               GlobalUnlock (hGlobal) ;

                    // Transfer it to the clipboard

               OpenClipboard (hwnd) ;
               EmptyClipboard () ;
               SetClipboardData (CF_DIB, hGlobal) ;
               CloseClipboard () ;

               if (LOWORD (wParam) == IDM_EDIT_COPY)
                    return 0 ;
                                        // fall through if IDM_EDIT_CUT 
          case IDM_EDIT_DELETE:
               if (pbmfh)
               {
                    free (pbmfh) ;
                    pbmfh = NULL ;
                    InvalidateRect (hwnd, NULL, TRUE) ;
               }
               return 0 ;

          case IDM_SHOW_NORMAL:
          case IDM_SHOW_CENTER:
          case IDM_SHOW_STRETCH:
          case IDM_SHOW_ISOSTRETCH:
               CheckMenuItem (hMenu, wShow, MF_UNCHECKED) ;
               wShow = LOWORD (wParam) ;
               CheckMenuItem (hMenu, wShow, MF_CHECKED) ;
               InvalidateRect (hwnd, NULL, TRUE) ;
               return 0 ;
          }
          break ;
         
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;

          if (pbmfh)
               ShowDib (hdc, pbmi, pBits, cxDib, cyDib, 
                        cxClient, cyClient, wShow) ;

          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          if (pbmfh)
               free (pbmfh) ;

          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}