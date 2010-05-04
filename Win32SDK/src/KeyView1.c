/*--------------------------------------------------------
   KEYVIEW1.C -- Displays Keyboard and Character Messages
                 (c) Charles Petzold, 1998
  --------------------------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("KeyView1") ;
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
          MessageBox (NULL, TEXT ("This program requires Windows NT!"), 
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, TEXT ("Keyboard Message Viewer #1"),
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
     static int   cxClientMax, cyClientMax, cxClient, cyClient, cxChar, cyChar ;
     static int   cLinesMax, cLines ;
     static PMSG  pmsg ;
     static RECT  rectScroll ;
     static TCHAR szTop[] = TEXT ("Message        Key       Char     ")
                            TEXT ("Repeat Scan Ext ALT Prev Tran") ;
     static TCHAR szUnd[] = TEXT ("_______        ___       ____     ")
                            TEXT ("______ ____ ___ ___ ____ ____") ;

     static TCHAR * szFormat[2] = { 
          
               TEXT ("%-13s %3d %-15s%c%6u %4d %3s %3s %4s %4s"),
               TEXT ("%-13s            0x%04X%1s%c %6u %4d %3s %3s %4s %4s") } ;
     static TCHAR * szYes  = TEXT ("Yes") ;
     static TCHAR * szNo   = TEXT ("No") ;
     static TCHAR * szDown = TEXT ("Down") ;
     static TCHAR * szUp   = TEXT ("Up") ;

     static TCHAR * szMessage [] = { 
                         TEXT ("WM_KEYDOWN"),    TEXT ("WM_KEYUP"), 
                         TEXT ("WM_CHAR"),       TEXT ("WM_DEADCHAR"), 
                         TEXT ("WM_SYSKEYDOWN"), TEXT ("WM_SYSKEYUP"), 
                         TEXT ("WM_SYSCHAR"),    TEXT ("WM_SYSDEADCHAR") } ;
     HDC          hdc ;
     int          i, iType ;
     PAINTSTRUCT  ps ;
     TCHAR        szBuffer[128], szKeyName [32] ;
     TEXTMETRIC   tm ;
     
     switch (message)
     {
     case WM_CREATE:
     case WM_DISPLAYCHANGE: // if the display is changed. Eg. from 800*600 to 1024*768
     
               // Get maximum size of client area

          cxClientMax = GetSystemMetrics (SM_CXMAXIMIZED) ;
          cyClientMax = GetSystemMetrics (SM_CYMAXIMIZED) ;

              // Get character size for fixed-pitch font

          hdc = GetDC (hwnd) ;
          
//          GetStockObject标识符 字体名称 小字体档案 大字体档案 
//          SYSTEM_FONT System VGASYS.FON 8514SYS.FON 点阵字 适合阅读
//          SYSTEM_FIXED_FONT FixedSys VGAFIX.FON 8514FIX.FON 向量字 淘汰
//          OEM_FIXED_FONT Terminal VGAOEM.FON 8514OEM.FON truetype 真正所见即所得
          
          // Select the equals-width SYSTEM_FONT as the font, the default one is SYSTEM_FONT
          SelectObject (hdc, GetStockObject (SYSTEM_FIXED_FONT)) ;
          GetTextMetrics (hdc, &tm) ;
          cxChar = tm.tmAveCharWidth ;
          cyChar = tm.tmHeight ;

          ReleaseDC (hwnd, hdc) ;

               // Allocate memory for display lines

          if (pmsg) // if pmsg exists, free it for re-allocating it later.
               free (pmsg) ;

          cLinesMax = cyClientMax / cyChar ;
          // The sentence below is equals to calloc(sizeof (MSG), cLinesMax);
          // The only difference is that calloc will initialize the memory with 0;
          // malloc will return the pointer which point to the first element of the array.
          pmsg = malloc (cLinesMax * sizeof (MSG)) ;
          cLines = 0 ;
                                   // fall through
     case WM_SIZE:
          if (message == WM_SIZE)
          {
               cxClient = LOWORD (lParam) ;
               cyClient = HIWORD (lParam) ;
          }
               // Calculate scrolling rectangle

          rectScroll.left   = 0 ;
          rectScroll.right  = cxClient ;
          rectScroll.top    = cyChar ;
          rectScroll.bottom = cyChar * (cyClient / cyChar) ;

          InvalidateRect (hwnd, NULL, TRUE) ;
          return 0 ;
          
     // There are two type of windows message will be stored to pmsg array.
     // One is KEY message and the other is CHAR message.
     // They have the same meaningful lParam parameter and different wParam parameter.
     // lParam can be parsed by using the way below from line 196 to line 201.
     // See the explaination on the page 201 of the book: Windows Programming
     // For KEY message, wParam means virtual key code like 'A'(no matter you are typing 'A' or 'a'), 'LEFT WINDOWS'.
     // For CHAR message, wParam means ANSI or UNICODE code like 'A', 'a'.
     // Handle characters in CHAR message while handle direction, function, delete, insert, shift, ctrl, alt keys in KEY message.
     // tab, enter, backspace and escape keys should be handled in WM_CHAR message instead of virtual key in WM_KEYDOWN message.
     case WM_KEYDOWN:
     case WM_KEYUP:
     case WM_CHAR:
     case WM_DEADCHAR:
     case WM_SYSKEYDOWN:
     case WM_SYSKEYUP:
     case WM_SYSCHAR:
     case WM_SYSDEADCHAR: 

               // Rearrange storage array

          for (i = cLinesMax - 1 ; i > 0 ; i--)
          {
               pmsg[i] = pmsg[i - 1] ;
          }
               // Store new message

          pmsg[0].hwnd = hwnd ;
          pmsg[0].message = message ;
          pmsg[0].wParam = wParam ;
          pmsg[0].lParam = lParam ;

          cLines = min (cLines + 1, cLinesMax) ;

               // Scroll up the display

          ScrollWindow (hwnd, 0, -cyChar, &rectScroll, &rectScroll) ;

          break ;        // i.e., call DefWindowProc so Sys messages work

     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          // Select the equals-width SYSTEM_FONT as the font, the default one is SYSTEM_FONT
          SelectObject (hdc, GetStockObject (SYSTEM_FIXED_FONT)) ; 
          SetBkMode (hdc, TRANSPARENT) ; // try to comment out this sentence and run the exe once again.
          TextOut (hdc, 0, 0, szTop, lstrlen (szTop)) ;
          TextOut (hdc, 0, 0, szUnd, lstrlen (szUnd)) ;

          for (i = 0 ; i < min (cLines, cyClient / cyChar - 1) ; i++)
          {
        	  // iType will be used to distinguish the KEY message and CHAR message.
               iType = pmsg[i].message == WM_CHAR ||
                       pmsg[i].message == WM_SYSCHAR ||
                       pmsg[i].message == WM_DEADCHAR ||
                       pmsg[i].message == WM_SYSDEADCHAR ;

               GetKeyNameText (pmsg[i].lParam, szKeyName, // translate the lParam of KEY message to readable text such as: left windows.
                               sizeof (szKeyName) / sizeof (TCHAR)) ;

               TextOut (hdc, 0, (cyClient / cyChar - 1 - i) * cyChar, szBuffer,
                        wsprintf (szBuffer, szFormat [iType],
                             szMessage [pmsg[i].message - WM_KEYFIRST],
                             pmsg[i].wParam,  // for key message, it is virtual key code, for char message, it is ansi or unicode.
                             (PTSTR) (iType ? TEXT (" ") : szKeyName), // display key name if it is KEY message.
                             (TCHAR) (iType ? pmsg[i].wParam : ' '), // display ansi or unicode charater if it is CHAR message.
                             LOWORD (pmsg[i].lParam),
                             HIWORD (pmsg[i].lParam) & 0xFF,
                             0x01000000 & pmsg[i].lParam ? szYes  : szNo,
                             0x20000000 & pmsg[i].lParam ? szYes  : szNo,
                             0x40000000 & pmsg[i].lParam ? szDown : szUp,
                             0x80000000 & pmsg[i].lParam ? szUp   : szDown)) ;
          }
          EndPaint (hwnd, &ps) ;
          return 0 ;

     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

