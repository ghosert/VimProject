/*-------------------------------------
   EMF6.C -- Enhanced Metafile Demo #6
             (c) Charles Petzold, 1998
  -------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR lpszCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("EMF6") ;
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
     
     hwnd = CreateWindow (szAppName, TEXT ("Enhanced Metafile Demo #6"),
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
     ENHMETARECORD * pEmfr ;
     
     pEmfr = (ENHMETARECORD *) malloc (pEmfRecord->nSize) ;
     
     CopyMemory (pEmfr, pEmfRecord, pEmfRecord->nSize) ;
     
     if (pEmfr->iType == EMR_RECTANGLE)
          pEmfr->iType = EMR_ELLIPSE ;

     PlayEnhMetaFileRecord (hdc, pHandleTable, pEmfr, iHandles) ;
     
     free (pEmfr) ;

     return TRUE ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     HDC          hdc ;
     HENHMETAFILE hemf ;
     PAINTSTRUCT  ps ;
     RECT         rect ;
     
     switch (message)
     {
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          GetClientRect (hwnd, &rect) ;
          
          rect.left   =     rect.right  / 4 ;
          rect.right  = 3 * rect.right  / 4 ;
          rect.top    =     rect.bottom / 4 ;
          rect.bottom = 3 * rect.bottom / 4 ;
          
          hemf = GetEnhMetaFile (TEXT ("..\\emf3\\emf3.emf")) ;
          
          EnumEnhMetaFile (hdc, hemf, EnhMetaFileProc, NULL, &rect) ;
          DeleteEnhMetaFile (hemf) ;
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
