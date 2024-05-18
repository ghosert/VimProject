/*--------------------------------------------------
   LINEDEMO.C -- Line-Drawing Demonstration Program
                 (c) Charles Petzold, 1998
  --------------------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("LineDemo") ;
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
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("Program requires Windows NT!"), 
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Line Demonstration"),
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
     static int  cxClient, cyClient ;
     HDC         hdc ;
     PAINTSTRUCT ps ;
     
     switch (message)
     {
     case WM_SIZE:
          cxClient = LOWORD (lParam) ;
          cyClient = HIWORD (lParam) ;
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
// For the GetStockObject you can use three pen white, black and null, other color & style pen should be created by yourself. BLACK_PEN by default
          HPEN hPen = GetStockObject(WHITE_PEN);
          hPen = SelectObject(hdc, hPen); // select white pen to object and save the former pen to hPen.
          SelectObject(hdc, hPen); // Reselect black pen to object.
          
          // Create a red solid pen with 1 pixel width.
          hPen = CreatePen(PS_SOLID, 0, RGB(255, 0, 0)); // Other style including PS_DASH, PS_DOT, PS_****
          hPen = SelectObject(hdc, hPen); // select the red pen and save the former pen to hPen (black pen);
          
          Rectangle (hdc,     cxClient / 8,     cyClient / 8,
                     7 * cxClient / 8, 7 * cyClient / 8) ;
          
          DeleteObject(SelectObject(hdc, hPen));// select the black pen and delete the former pen.(red pen);
          
          // if the width is larger than 1. the dash will be turned into solid. The rule is the same to other style.
          hPen = CreatePen(PS_DASH, 0, RGB(0, 255, 0)); 
          hPen = SelectObject(hdc, hPen);
          
          // SetBkColor to change the color in the space of the dash line.
          SetBkColor(hdc, RGB(0, 0, 255));
          // Windows will not fill the color in the space of the dash line, after calling the function below.
          // SetBkMode(hdc, TRANSPARENT);
          
          MoveToEx  (hdc,        0,        0, NULL) ;
          LineTo    (hdc, cxClient, cyClient) ;
          
          DeleteObject(SelectObject(hdc, hPen));

// Use the combination of pen color/ brush color and background color to decide the final color.
// R2_NOT means use the opposite color of backgroud as the pen color and don't care the current pen color.
          SetROP2(hdc, R2_NOT); // R2_COPYPEN is default pen, means copy the pen color to background
          
          MoveToEx  (hdc,        0, cyClient, NULL) ;
          LineTo    (hdc, cxClient,        0) ;
          
          SetROP2(hdc, R2_COPYPEN);
          
// NULL_PEN means no border, NULL_BRUSH means no brush
          HBRUSH hBrush = GetStockObject(GRAY_BRUSH);
          hBrush = SelectObject(hdc, hBrush);
          
          Ellipse   (hdc,     cxClient / 8,     cyClient / 8,
                     7 * cxClient / 8, 7 * cyClient / 8) ;
          
          DeleteObject(SelectObject(hdc, hBrush));
          
          hBrush = CreateSolidBrush(RGB(255, 0, 0));
          hBrush = SelectObject(hdc, hBrush);
          
          RoundRect (hdc,     cxClient / 4,     cyClient / 4,
                     3 * cxClient / 4, 3 * cyClient / 4,
                         cxClient / 4,     cyClient / 4) ;
          
          DeleteObject(SelectObject(hdc, hBrush));
          
          // This will display a cross with red line and black color filled.
          // Other style including HS_***
          SetBkColor(hdc, RGB(0, 0, 0));
          hBrush = CreateHatchBrush(HS_CROSS, RGB(255, 0, 0));
          hBrush = SelectObject(hdc, hBrush);
          
          Pie(hdc, cxClient / 4, cyClient / 4, 3 * cxClient / 4, 3 * cxClient / 4, cxClient / 2, cyClient / 4, cxClient / 4, cyClient / 2);
          
          DeleteObject(SelectObject(hdc, hBrush));
          
          Chord(hdc, cxClient / 4, cyClient / 4, 3 * cxClient / 4, 3 * cxClient / 4, cxClient / 2, cyClient / 4, cxClient / 4, cyClient / 2);
          Arc(hdc, cxClient / 4, cyClient / 4, 3 * cxClient / 4, 3 * cxClient / 4, cxClient / 2, cyClient / 4, cxClient / 4, cyClient / 2);
          
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

