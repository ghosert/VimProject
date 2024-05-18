/*--------------------------------------
   CLOCK.C -- Analog Clock Program

              (c) Charles Petzold, 1998
  --------------------------------------*/

#include <windows.h>
#include <math.h>

#define ID_TIMER    1
#define TWOPI       (2 * 3.14159)

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("Clock") ;
     HWND         hwnd;
     MSG          msg;
     WNDCLASS     wndclass ;
     
     wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
     wndclass.lpfnWndProc   = WndProc ;
     wndclass.cbClsExtra    = 0 ;
     wndclass.cbWndExtra    = 0 ;
     wndclass.hInstance     = hInstance ;
     wndclass.hIcon         = NULL ;
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

     hwnd = CreateWindow (szAppName, TEXT ("Analog Clock"),
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

// Set di car er coordinate with origin and 1000 x size, 1000 y size as logical coordinate.
void SetIsotropic (HDC hdc, int cxClient, int cyClient)
{
     SetMapMode (hdc, MM_ISOTROPIC) ;
     SetWindowExtEx (hdc, 1000, 1000, NULL) ;
     SetViewportExtEx (hdc, cxClient / 2, -cyClient / 2, NULL) ;
     SetViewportOrgEx (hdc, cxClient / 2,  cyClient / 2, NULL) ;
}

// Rotate the points array with numbers and Angle(360 degree).
void RotatePoint (POINT pt[], int iNum, int iAngle)
{
     int   i ;
     POINT ptTemp ;
     
     // x2 = x1 * cos(a) + y1 * sin(a);
     // y2 = y1 * cos(a) - x1 * sin(a);
     for (i = 0 ; i < iNum ; i++)
     {
          ptTemp.x = (int) (pt[i].x * cos (TWOPI * iAngle / 360) +
               pt[i].y * sin (TWOPI * iAngle / 360)) ;
          
          ptTemp.y = (int) (pt[i].y * cos (TWOPI * iAngle / 360) -
               pt[i].x * sin (TWOPI * iAngle / 360)) ;
          
          pt[i] = ptTemp ;
     }
}

void DrawClock (HDC hdc)
{
     int   iAngle ;
     POINT pt[3] ;

     for (iAngle = 0 ; iAngle < 360 ; iAngle += 6)
     {
          pt[0].x =   0 ;
          pt[0].y = 900 ;
          
          // draw a round every 6 degree.
          RotatePoint (pt, 1, iAngle) ;
          
          // draw a big round instead of small one every 5
          pt[2].x = pt[2].y = iAngle % 5 ? 33 : 100 ;
          
          // get the left-top point
          pt[0].x -= pt[2].x / 2 ;
          pt[0].y -= pt[2].y / 2 ;
          
          // get the right-bottom point
          pt[1].x  = pt[0].x + pt[2].x ;
          pt[1].y  = pt[0].y + pt[2].y ;
          
          SelectObject (hdc, GetStockObject (BLACK_BRUSH)) ;
          
          Ellipse (hdc, pt[0].x, pt[0].y, pt[1].x, pt[1].y) ;
     }
}

void DrawHands (HDC hdc, SYSTEMTIME * pst, BOOL fChange)
{
     static POINT pt[3][5] = { 0, -150, 100, 0, 0, 600, -100, 0, 0, -150,
                               0, -200,  50, 0, 0, 800,  -50, 0, 0, -200,
                               0,    0,   0, 0, 0,   0,    0, 0, 0,  800 } ;
     int          i, iAngle[3] ;
     POINT        ptTemp[3][5] ;
     
     // rotate 30 degree + wMinute * 6 / (60 / 5)
     iAngle[0] = (pst->wHour * 30) % 360 + pst->wMinute / 2 ;
     // rotate 6 degree every time for minute hand
     iAngle[1] =  pst->wMinute  *  6 ;
     // rotate 6 degree every time for second hand
     iAngle[2] =  pst->wSecond  *  6 ;
     
     memcpy (ptTemp, pt, sizeof (pt)) ;
     
     // fChange here means whether to draw hour hand and minute hand, second hand should be draw always.
     for (i = fChange ? 0 : 2 ; i < 3 ; i++)
     {
          RotatePoint (ptTemp[i], 5, iAngle[i]) ;
          
          Polyline (hdc, ptTemp[i], 5) ;
     }
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static int        cxClient, cyClient ;
     static SYSTEMTIME stPrevious ;
     BOOL              fChange ;
     HDC               hdc ;
     PAINTSTRUCT       ps ;
     SYSTEMTIME        st ;
          
     switch (message)
     {
     case WM_CREATE :
          SetTimer (hwnd, ID_TIMER, 1000, NULL) ;
          GetLocalTime (&st) ;
          stPrevious = st ;
          return 0 ;
          
     case WM_SIZE :
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;
          
     case WM_TIMER :
          GetLocalTime (&st) ;
                    
          fChange = st.wHour   != stPrevious.wHour ||
                    st.wMinute != stPrevious.wMinute ;
          
          hdc = GetDC (hwnd) ;
          
          SetIsotropic (hdc, cxClient, cyClient) ;
          
          // erase the hands before redraw it(by using white pen the same color to the background)
          SelectObject (hdc, GetStockObject (WHITE_PEN)) ;
          DrawHands (hdc, &stPrevious, fChange) ;
          
          SelectObject (hdc, GetStockObject (BLACK_PEN)) ;
          DrawHands (hdc, &st, TRUE) ;
          
          ReleaseDC (hwnd, hdc) ;
          
          stPrevious = st ;
          return 0 ;
          
     case WM_PAINT :
          hdc = BeginPaint (hwnd, &ps) ;
          
          SetIsotropic (hdc, cxClient, cyClient) ;

          DrawClock    (hdc) ;
          DrawHands    (hdc, &stPrevious, TRUE) ;
          
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY :
          KillTimer (hwnd, ID_TIMER) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

