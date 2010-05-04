/*------------------------------------------
   RANDRECT.C -- Displays Random Rectangles
                 (c) Charles Petzold, 1998
  ------------------------------------------*/

// #include "stdafx.h"
// #include "win32.h"
#include <windows.h>
#include "CustomFunctionLib.h"

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("RandRect") ;
     HWND         hwnd ;
     MSG          msg ;
     WNDCLASS     wndclass ;
     
     //wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
	 wndclass.style         = 0 ;
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
     
     hwnd = CreateWindow (szAppName, TEXT ("Random Rectangles"),
		                  WS_POPUP | WS_SYSMENU,
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
     return (int) msg.wParam ;
}

int callback(void) {
	MessageBox(NULL, TEXT("I am mail."), TEXT("I am No. 1"), MB_OK);
}

int callbackHorn(void) {
	MessageBox(NULL, TEXT("I am Horn."), TEXT("I am No. 2"), MB_OK);
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
	 static HBITMAP hBitmap;
	 
     HDC hdc;
	 HDC hdcMemImag;
	 HINSTANCE hInstance;
	 BITMAP bitmap;
	 PAINTSTRUCT ps;
	 RECT rect;

     switch (iMsg) {
	     case WM_CREATE:
	    	 hInstance = ((LPCREATESTRUCT) lParam)->hInstance;
			 // hBitmap = LoadBitmap(hInstance, MAKEINTRESOURCE(IDB_BITMAP1));
			 hBitmap = LoadBitmap(hInstance, TEXT("CustomWindow"));
	    	 
	    	 // Get bitmap from hBitmap
		     GetObject(hBitmap, sizeof(BITMAP), &bitmap);

			 // Expand the window.
			 MoveWindow(hwnd, 300, 300, bitmap.bmWidth, bitmap.bmHeight, FALSE);
		     
		     // Select hdcMemImage to set or get pixel.
			 hdc = GetDC(hwnd);
			 hdcMemImag = CreateCompatibleDC(hdc);
	    	 ReleaseDC(hwnd, hdc);
	    	 
	    	 SelectObject(hdcMemImag, hBitmap);
	    	 
	    	 // Create Region from Bitmap and draw bitmap to window.
	    	 SetWindowRgnFromBitMap(hwnd, hdcMemImag, bitmap, RGB(255, 0, 255), TRUE);
	   	     
	    	 DeleteDC(hdcMemImag);
	    	 
	    	 // Create Custom Button here:
	    	 CUSTOMBUTTONPARAM cmbtpm;
	    	 cmbtpm.ID_CHILDWINDOW = 1;
	    	 cmbtpm.leftPosition = 89;
	    	 cmbtpm.topPosition = 12;
	    	 cmbtpm.lpfnCallBack = callback;
	    	 cmbtpm.lpszNormalBitmapResource = TEXT("CustomButton");
	    	 cmbtpm.lpszMouseMoveBitmapResource = TEXT("CustomButton_MoveIn");
	    	 cmbtpm.lpszPressedBitmapResource = TEXT("CustomButton_Down");
	    	 cmbtpm.pHwnd = hwnd;
	    	 
	    	 CreateCustomButton(&cmbtpm);
	    	 
	    	 cmbtpm.ID_CHILDWINDOW = 2;
	    	 cmbtpm.leftPosition = 140;
	    	 cmbtpm.topPosition = 11;
	    	 cmbtpm.lpfnCallBack = callbackHorn;
	    	 cmbtpm.lpszNormalBitmapResource = TEXT("horn");
	    	 cmbtpm.lpszMouseMoveBitmapResource = TEXT("horn_movein");
	    	 cmbtpm.lpszPressedBitmapResource = TEXT("horn_down");
	    	 cmbtpm.pHwnd = hwnd;
	    	 
	    	 CreateCustomButton(&cmbtpm);
	    	 
    	     CreateTransParentWindows(hwnd, RGB(255, 0, 0), 192, LWA_ALPHA);
    	     InvalidateRect(hwnd, NULL, TRUE); 
	    	 return 0;
		 case WM_PAINT:
			 hdc = BeginPaint(hwnd, &ps);
			 
             // Select bitmaps into memory DCs
			 hdcMemImag = CreateCompatibleDC (hdc);
			 SelectObject (hdcMemImag, hBitmap);
			 
			 rect = ps.rcPaint;
			 BitBlt(hdc, rect.left, rect.top, rect.right, rect.bottom, hdcMemImag, rect.left, rect.top, SRCCOPY);
			 
			 DeleteDC(hdcMemImag);
			 EndPaint(hwnd, &ps);

			 return 0;
		 case WM_LBUTTONDOWN:
			 // To simulate that we are click on the capital title and this will cause Windows to move the current window even there is no capital title there.
			 // We can also simulate the minimize, maximize, and close button here.
			 SendMessage(hwnd, WM_NCLBUTTONDOWN, HTCAPTION, 0);
			 return 0;
	     case WM_DESTROY:
	    	  // Delete hBitmap
	    	  DeleteObject(hBitmap);
	          PostQuitMessage (0) ;
	          return 0 ;
     }
     return DefWindowProc (hwnd, iMsg, wParam, lParam) ;
}
