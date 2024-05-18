/*-----------------------------------------------------------
   SHOWBIT.C -- Shows bitmaps in BITLIB dynamic-link library
                (c) Charles Petzold, 1998

  -----------------------------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName [] = TEXT ("ShowBit") ;

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
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }

     hwnd = CreateWindow (szAppName, 
                          TEXT ("Show Bitmaps from BITLIB (Press Key)"),
                          WS_OVERLAPPEDWINDOW,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          CW_USEDEFAULT, CW_USEDEFAULT,
                          NULL, NULL, hInstance, NULL) ;

     if (!hwnd)
          return 0 ;
     
     ShowWindow (hwnd, iCmdShow) ;
     UpdateWindow (hwnd) ;
     
     while (GetMessage (&msg, NULL, 0, 0))
     {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ;
     }
     return msg.wParam ;
}

void DrawBitmap (HDC hdc, int xStart, int yStart, HBITMAP hBitmap)
{
     BITMAP bm ;
     HDC    hMemDC ;
     POINT  pt ;

     hMemDC = CreateCompatibleDC (hdc) ;
     SelectObject (hMemDC, hBitmap) ;
     GetObject (hBitmap, sizeof (BITMAP), &bm) ;
     pt.x = bm.bmWidth ;
     pt.y = bm.bmHeight ;
     
     BitBlt (hdc, xStart, yStart, pt.x, pt.y, hMemDC, 0, 0, SRCCOPY) ;
     
     DeleteDC (hMemDC) ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static HINSTANCE hLibrary ;
     static int       iCurrent = 1 ;
     HBITMAP          hBitmap ;
     HDC              hdc ;
     PAINTSTRUCT      ps ;

     switch (message)
     {
     case WM_CREATE:
    	  // Load a pure resource dll here and get HINSTANCE variable.
    	  // More about LoadLibrary function:
    	  // Because of GDI32.LIB import lib file has been linked to our exe, so we can use Rectangle(hdc, xLeft, yTop, xRight, yBottom) function directly,
    	  // if you do not have .lib file, you can load dll at runtime, or the .lib file you have is older one but you need new functions in current dll. this is why we need this technology.
    	  // Below are the steps about this technology.
//    	 typedef BOOL (WINAPI * PFNRECT) (HDC, int, int, int, int) ;
//
//    	 You then define two variables: 
//
//    	 HANDLE  hLibrary ;
//    	 PFNRECT pfnRectangle ;
//
//    	 Now you set hLibrary to the handle of the library and lpfnRectangle to the address of the Rectangle function: 
//
//    	 hLibrary = LoadLibrary (TEXT ("GDI32.DLL"))
//    	 pfnRectangle = (PFNPRECT) GetProcAddress (hLibrary, TEXT ("Rectangle"))
//
//    	 The LoadLibrary function returns NULL if the library file can't be found or if some other error occurs. Now you can call the function and then free the library: 
//
//    	 pfnRectangle (hdc, xLeft, yTop, xRight, yBottom) ;
//    	 FreeLibrary (hLibrary) ;

          if ((hLibrary = LoadLibrary (TEXT ("BITLIB.DLL"))) == NULL)
          {
               MessageBox (hwnd, TEXT ("Can't load BITLIB.DLL."),
                           szAppName, 0) ;
               return -1 ;
          }
          return 0 ;
          
     case WM_CHAR:
          if (hLibrary)
          {
               iCurrent ++ ;
               InvalidateRect (hwnd, NULL, TRUE) ;
          }
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          if (hLibrary)
          {
        	   // Load resource from pure resource dll with hinstance and id here.
        	   // MAKEINTRESOURCE here means we are using id in bitlib.rc file just as one line I picked up in bitlib.rc file below:
        	   // 1                       BITMAP  DISCARDABLE     "src/bitmap1.bmp"
        	   // See more about MAKEINTRESOURCE in the book windows programming Figure 10-4.
               hBitmap = LoadBitmap (hLibrary, MAKEINTRESOURCE (iCurrent)) ;

               if (!hBitmap) 
               {
                    iCurrent = 1 ;
                    hBitmap = LoadBitmap (hLibrary, 
                                          MAKEINTRESOURCE (iCurrent)) ;
               }
               if (hBitmap)
               {
                    DrawBitmap (hdc, 0, 0, hBitmap) ;
                    DeleteObject (hBitmap) ;
               }
          }
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
    	  // free the loaded hinstance.
          if (hLibrary)
               FreeLibrary (hLibrary) ;

          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

