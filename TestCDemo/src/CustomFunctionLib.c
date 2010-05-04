#include <windows.h>
#include "CustomFunctionLib.h"

// Create Region From BitMap which is set mask area with color specified here. And the return value HRGN should be DeleteObject(HRGN) later.
// the number of iMaxNumRect below may be can be reduced to iMaxNumRect / 2, try it later.
HRGN CreateRegionFromBitMap(HDC hdcMem, BITMAP bitmap, COLORREF color) {
	
	// The code below means one pixel one rect.
	int iMaxNumRect = bitmap.bmWidth * bitmap.bmHeight;
	
	// This size is ready for RGNDATA struct and equals size of RGNDATAHEADER & number of rect.
	int size = sizeof(RGNDATAHEADER) + iMaxNumRect * sizeof(RECT);
	
	// alloc the memory, return NULL if there is no enough memory.
	char * buffer = (char *) malloc(size);
	if (buffer == NULL) return NULL;

	memset(buffer, 0, size);
	
	PRGNDATA pRgnData = (PRGNDATA) buffer;
	
	// Set RGNDATAHEADER
	pRgnData->rdh.dwSize = sizeof(RGNDATAHEADER);
	pRgnData->rdh.iType = RDH_RECTANGLES;
	pRgnData->rdh.nCount = 0; // this value will be increased later.
	pRgnData->rdh.nRgnSize = iMaxNumRect * sizeof(RECT); // means number of rect, you can set 0 here means unknown;
	// Define the maximum rect for the region.
	pRgnData->rdh.rcBound.left = 0;
	pRgnData->rdh.rcBound.top = 0;
	pRgnData->rdh.rcBound.right = bitmap.bmWidth;
	pRgnData->rdh.rcBound.bottom = bitmap.bmHeight;
	// Get the pointer of rect to visit the rects from RGNDATA
	PRECT pRect = (PRECT)pRgnData->Buffer;
	
	
	int x, y, iStartPoint;
	BOOL isRectStart = FALSE;
	for (y = 0; y < bitmap.bmHeight; y++) {
	    for (x = 0; x < bitmap.bmWidth; x++) {
	    	if (GetPixel(hdcMem, x, y) != color) {
	    		if (isRectStart) {
	    			continue;
	    		} else {
	    			isRectStart = TRUE;
	    			iStartPoint = x;
	    		}
	    	} else {
	    		if (isRectStart) {
	    			pRect->left = iStartPoint;
	    			pRect->right = x;
	    			pRect->top = y;
	    			pRect->bottom = y + 1;
	    			pRect++;
					pRgnData->rdh.nCount++;
	    			isRectStart = FALSE;
	    		} else {
	    			continue;
	    		}
	    	}
	    }
	    if (isRectStart) {
			pRect->left = iStartPoint;
			pRect->right = x;
			pRect->top = y;
			pRect->bottom = y + 1;
			pRect++;
			pRgnData->rdh.nCount++;
			isRectStart = FALSE;
	    }
	}
	
	HRGN hgrn = ExtCreateRegion(NULL, size, pRgnData);
	
	free(buffer);
	
	return hgrn;
}

void SetWindowRgnFromBitMap(HWND hwnd, HDC hdcMem, BITMAP bitmap, COLORREF color, BOOL redraw) {
	HRGN hrgn = CreateRegionFromBitMap(hdcMem, bitmap, color);
	SetWindowRgn(hwnd, hrgn, redraw);
	DeleteObject(hrgn);
}

BOOL IsPtInWindowsRegion(HWND hwnd, int x, int y) {
	 HRGN hrgn = CreateRectRgn(0,0,0,0);
	 GetWindowRgn(hwnd, hrgn);
	 BOOL isPtInWindowsRegion = FALSE;
	 if (PtInRegion(hrgn, x, y)) isPtInWindowsRegion = TRUE;
	 DeleteObject(hrgn); /* finished with region */
	 return isPtInWindowsRegion;
}

void CreateAnimateWindows(HWND hwnd, DWORD dwTime, DWORD dwFlags, BOOL isCreateOrClose) {
	
     // Here we import the function from USER32.DLL   
     HMODULE   hUser32   =   GetModuleHandle(TEXT("USER32.DLL"));   
     
	 // Preparation for the function we want to import from USER32.DLL   
	 typedef BOOL (WINAPI *lpfnAnimateWindow)(HWND hwnd, DWORD dwTime, DWORD dwFlags);
	 lpfnAnimateWindow AnimateWindow = (lpfnAnimateWindow) GetProcAddress(hUser32, "AnimateWindow");
	 
	 // The animate window effect.
	 if (!isCreateOrClose) dwFlags = dwFlags | 0x10000; // 0x10000 means AW_HIDE here
	 AnimateWindow(hwnd, dwTime, dwFlags); 
}
void CreateTransParentWindows(HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags) {
	
     // Here we import the function from USER32.DLL   
     HMODULE   hUser32   =   GetModuleHandle(TEXT("USER32.DLL"));   
     
	 // Preparation for the function we want to import from USER32.DLL   
	 typedef   BOOL   (WINAPI   *lpfnSetLayeredWindowAttributes)(HWND   hWnd,   COLORREF   crKey,   BYTE   bAlpha,   DWORD   dwFlags);   
	 lpfnSetLayeredWindowAttributes SetLayeredWindowAttributes = (lpfnSetLayeredWindowAttributes) GetProcAddress(hUser32, "SetLayeredWindowAttributes");   
	 
	 // The transparence effect.
	 SetWindowLong(hwnd,GWL_EXSTYLE,GetWindowLong(hwnd,GWL_EXSTYLE)^WS_EX_LAYERED);   
	 SetLayeredWindowAttributes(hwnd, crKey, bAlpha, dwFlags);   
}

static LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

typedef struct _CUSTOMBUTTONINTERNPARAM {
	HBITMAP hNormalBitmap;
	HBITMAP hMouseMoveBitmap;
	HBITMAP hPressedBitmap;
	BTCALLBACK lpfnCallBack;
} CUSTOMBUTTONINTERNPARAM, *PCUSTOMBUTTONINTERNPARAM;

void CreateCustomButton(PCUSTOMBUTTONPARAM pCmbtpm) {
	
	HINSTANCE hInstance = (HINSTANCE) GetWindowLong(pCmbtpm->pHwnd, GWL_HINSTANCE);
	
	LPCSTR lpszClassName = TEXT("custombutton");
	
	WNDCLASS wndclass;
	wndclass.style         = 0 ;
    wndclass.lpfnWndProc   = WndProc;
    wndclass.cbClsExtra    = 0 ;
    wndclass.hInstance     = hInstance ;
    wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
    wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
    wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH) ;
    wndclass.lpszMenuName  = NULL ;
    wndclass.lpszClassName = lpszClassName;

    // IMPORTANT: initialize this parameter for it can preserve the data which is related to a certain hwnd by using SetWindowLong(hwnd, 0, data);
    // The data's size is defined here.
    wndclass.cbWndExtra    = sizeof(PCUSTOMBUTTONINTERNPARAM) ;  
    
    RegisterClass (&wndclass);
    
    // Create the owner-draw pushbuttons
    HWND hwnd = CreateWindow (lpszClassName, TEXT (""), WS_CHILD | WS_VISIBLE, 0, 0, 0, 0,
    		                  pCmbtpm->pHwnd, pCmbtpm->ID_CHILDWINDOW, hInstance, pCmbtpm);
}

#define RESOURCE_NORMAL    0
#define RESOURCE_MOUSEMOVE 1 
#define RESOURCE_PRESSED   2 

static LRESULT CALLBACK WndProc (HWND hwnd, UINT iMsg, WPARAM wParam, LPARAM lParam)
{
	 static int iCurrentBitmap = RESOURCE_NORMAL;
	 static BOOL isMoveIn = FALSE;
	 static BOOL isPressed = FALSE;
	 
	 HINSTANCE hInstance;
     HDC hdc;
	 HDC hdcMemImag;
	 PAINTSTRUCT ps;
	 
	 PCUSTOMBUTTONINTERNPARAM pCmbtinpm;

     switch (iMsg) {
	     case WM_CREATE:
	    	 
	    	 hInstance = ((LPCREATESTRUCT) lParam)->hInstance;
	    	 
	    	 PCUSTOMBUTTONPARAM pCmbtpm = (PCUSTOMBUTTONPARAM)((LPCREATESTRUCT) lParam)->lpCreateParams;
	    	 
	    	 // initialize CUSTOMBUTTONINTERNPARAM from CUSTOMBUTTONPARAM
	    	 pCmbtinpm = malloc(sizeof(CUSTOMBUTTONINTERNPARAM));
	    	 pCmbtinpm->lpfnCallBack = pCmbtpm->lpfnCallBack;
			 // hBitmap = LoadBitmap(hInstance, MAKEINTRESOURCE(IDB_BITMAP1));
	    	 pCmbtinpm->hNormalBitmap = LoadBitmap(hInstance, pCmbtpm->lpszNormalBitmapResource);
	    	 pCmbtinpm->hMouseMoveBitmap = LoadBitmap(hInstance, pCmbtpm->lpszMouseMoveBitmapResource);
	    	 pCmbtinpm->hPressedBitmap = LoadBitmap(hInstance, pCmbtpm->lpszPressedBitmapResource);
			 
			 // Set it to hwnd related cbWndExtra which is initialized above for further using.
			 SetWindowLong(hwnd, 0, (long) (pCmbtinpm));
	    	 
	    	 // Get bitmap from hBitmap
	         BITMAP bitmap;
		     GetObject(pCmbtinpm->hNormalBitmap, sizeof(BITMAP), &bitmap);

			 // Expand the window.
			 MoveWindow(hwnd, pCmbtpm->leftPosition, pCmbtpm->topPosition, bitmap.bmWidth, bitmap.bmHeight, FALSE);
		     
		     // Select hdcMemImage to set or get pixel.
			 hdc = GetDC(hwnd);
			 hdcMemImag = CreateCompatibleDC(hdc);
	    	 ReleaseDC(hwnd, hdc);
	    	 
	    	 SelectObject(hdcMemImag, pCmbtinpm->hNormalBitmap);
	    	 
	    	 // Create Region from Bitmap and draw bitmap to window.
	    	 SetWindowRgnFromBitMap(hwnd, hdcMemImag, bitmap, RGB(255, 0, 255), TRUE);
	   	     
	    	 DeleteDC(hdcMemImag);
	    	 
	    	 return 0;
		 case WM_MOUSEMOVE:
			 if (isMoveIn == FALSE) {
			     // Begin to trace WM_MOUSELEAVE message when the mouse is moving on the current client.
				 // Here is the only place where we can begin to trace the WM_MOUSELEAVE message.
     	    	 TRACKMOUSEEVENT ts;
     	    	 ts.cbSize = sizeof(TRACKMOUSEEVENT);
     	    	 ts.dwFlags = TME_LEAVE;
     	    	 ts.hwndTrack = hwnd;
     	    	 TrackMouseEvent(&ts);
     	    	 
     	    	 // Redraw the client when it is now in.
		         iCurrentBitmap = RESOURCE_MOUSEMOVE;
			     InvalidateRgn(hwnd, NULL, TRUE);
			     
     	    	 isMoveIn = TRUE;
			 } else {
				 if (isPressed) {
					 int x = LOWORD(lParam);
					 int y = HIWORD(lParam);
					 BOOL isPtInWindowsRegion = IsPtInWindowsRegion(hwnd, x, y);
					 if (isPtInWindowsRegion) {
						 if (iCurrentBitmap != RESOURCE_PRESSED) {
					         iCurrentBitmap = RESOURCE_PRESSED;
					         InvalidateRgn(hwnd, NULL, TRUE);
						 }
					 } else {
						 if (iCurrentBitmap != RESOURCE_NORMAL) {
					         iCurrentBitmap = RESOURCE_NORMAL;
					         InvalidateRgn(hwnd, NULL, TRUE);
						 }
					 }
				 }
			 }
			 
			 return 0;
		 case WM_MOUSELEAVE:
			 
			 // If invoking SetCapture, WM_MOUSELEAVE message will never be sent by windows until invoking ReleaseCature.
   	    	 isMoveIn = FALSE;
   	    	 
   	    	 // Redraw the client when it is now out.
			 iCurrentBitmap = RESOURCE_NORMAL;
			 InvalidateRgn(hwnd, NULL, TRUE);
			 
			 return 0;
		 case WM_LBUTTONDOWN:
			 
			 SetCapture(hwnd);
			 isPressed = TRUE;
			 
   	    	 // Redraw the client when it is pressed.
			 iCurrentBitmap = RESOURCE_PRESSED;
			 InvalidateRgn(hwnd, NULL, TRUE);

			 return 0;
		 case WM_LBUTTONUP:
			 
			 if (isPressed) {
				 // after invoking ReleaseCapture, if current point is outside the window rgn, windows will sent a WM_MOUSELEAVE right now.
				 ReleaseCapture(); 
				 isPressed = FALSE;
				 
				 int x = LOWORD(lParam);
				 int y = HIWORD(lParam);
				 BOOL isPtInWindowsRegion = IsPtInWindowsRegion(hwnd, x, y);
				 if (isPtInWindowsRegion) {
		   	    	 // Redraw the client when it is pressed and the current point is in window region.
					 iCurrentBitmap = RESOURCE_MOUSEMOVE;
					 InvalidateRgn(hwnd, NULL, TRUE);
					 
					 // And then execute the real action here.
			         pCmbtinpm = (PCUSTOMBUTTONINTERNPARAM) GetWindowLong(hwnd, 0);
			         pCmbtinpm->lpfnCallBack();
				 }
				 
				 // ignore the action and redraw if current point is out side of the window region. 
				 // In this case, ReleaseCapture will cause windows sent a WM_MOUSELEAVE message, we don't need to redraw here.
			 }

			 return 0;
		 case WM_PAINT:
			 hdc = BeginPaint(hwnd, &ps);
			 
             // Select bitmaps into memory DCs
			 hdcMemImag = CreateCompatibleDC (hdc);
			 
			 pCmbtinpm = (PCUSTOMBUTTONINTERNPARAM) GetWindowLong(hwnd, 0);
			 switch (iCurrentBitmap) {
			     case RESOURCE_NORMAL:
			         SelectObject (hdcMemImag, pCmbtinpm->hNormalBitmap);
			         break;
			     case RESOURCE_MOUSEMOVE:
			         SelectObject (hdcMemImag, pCmbtinpm->hMouseMoveBitmap);
			         break;
			     case RESOURCE_PRESSED:
			         SelectObject (hdcMemImag, pCmbtinpm->hPressedBitmap);
			 }
			 
			 RECT rect = ps.rcPaint;
			 BitBlt(hdc, rect.left, rect.top, rect.right, rect.bottom, hdcMemImag, rect.left, rect.top, SRCCOPY);
			 
			 DeleteDC(hdcMemImag);
			 EndPaint(hwnd, &ps);

			 return 0;
	     case WM_DESTROY:
	    	  // Delete hBitmap
	          pCmbtinpm = (PCUSTOMBUTTONINTERNPARAM) GetWindowLong(hwnd, 0);
	    	  DeleteObject(pCmbtinpm->hNormalBitmap);
	    	  DeleteObject(pCmbtinpm->hMouseMoveBitmap);
	    	  DeleteObject(pCmbtinpm->hPressedBitmap);
	    	  free(pCmbtinpm);
	          PostQuitMessage (0) ;
	          return 0 ;
     }
     return DefWindowProc (hwnd, iMsg, wParam, lParam) ;
}


