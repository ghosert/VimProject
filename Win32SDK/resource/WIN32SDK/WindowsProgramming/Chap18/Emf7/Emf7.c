/*-------------------------------------
   EMF7.C -- Enhanced Metafile Demo #7
             (c) Charles Petzold, 1998
  -------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR lpszCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("EMF7") ;
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
     wndclass.hbrBackground = GetStockObject (WHITE_BRUSH) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Enhanced Metafile Demo #7"),
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

int CALLBACK EnhMetaFileProc (HDC hdc, HANDLETABLE * pHandleTable,
                              CONST ENHMETARECORD * pEmfRecord, 
                              int iHandles, LPARAM pData)
{
     HBRUSH   hBrush ;
     HPEN     hPen ;
     LOGBRUSH lb ;
     
     if (pEmfRecord->iType != EMR_HEADER && pEmfRecord->iType != EMR_EOF)
          
          PlayEnhMetaFileRecord (hdc, pHandleTable, pEmfRecord, iHandles) ;
     
     if (pEmfRecord->iType == EMR_RECTANGLE)
     {
          hBrush = SelectObject (hdc, GetStockObject (NULL_BRUSH)) ;
          
          lb.lbStyle = BS_SOLID ;
          lb.lbColor = RGB (0, 255, 0) ;
          lb.lbHatch = 0 ;
          
          hPen = SelectObject (hdc,
               ExtCreatePen (PS_SOLID | PS_GEOMETRIC, 5, &lb, 0, NULL)) ;
          
          Ellipse (hdc, 100, 100, 200, 200) ;
          
          DeleteObject (SelectObject (hdc, hPen)) ;
          SelectObject (hdc, hBrush) ;
     }
     return TRUE ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     ENHMETAHEADER emh ;
     HDC           hdc, hdcEMF ;
     HENHMETAFILE  hemfOld, hemf ;
     PAINTSTRUCT   ps ;
     RECT          rect ;
     
     switch (message)
     {
     case WM_CREATE:
          
               // Retrieve existing metafile and header
          
          hemfOld = GetEnhMetaFile (TEXT ("..\\emf3\\emf3.emf")) ;
          
          GetEnhMetaFileHeader (hemfOld, sizeof (ENHMETAHEADER), &emh) ;
          
               // Create a new metafile DC
          
          hdcEMF = CreateEnhMetaFile (NULL, TEXT ("emf7.emf"), NULL,
                                      TEXT ("EMF7\0EMF Demo #7\0")) ;
          
               // Enumerate the existing metafile
          
          EnumEnhMetaFile (hdcEMF, hemfOld, EnhMetaFileProc, NULL,
                           (RECT *) & emh.rclBounds) ;
          
               // Clean up
          
          hemf = CloseEnhMetaFile (hdcEMF) ;
          
          DeleteEnhMetaFile (hemfOld) ;
          DeleteEnhMetaFile (hemf) ;
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          GetClientRect (hwnd, &rect) ;
          
          rect.left   =     rect.right  / 4 ;
          rect.right  = 3 * rect.right  / 4 ;
          rect.top    =     rect.bottom / 4 ;
          rect.bottom = 3 * rect.bottom / 4 ;
          
          hemf = GetEnhMetaFile (TEXT ("emf7.emf")) ;
          
          PlayEnhMetaFile (hdc, hemf, &rect) ;
          DeleteEnhMetaFile (hemf) ;
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
