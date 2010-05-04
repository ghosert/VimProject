/*------------------------------------------------------------
   HELLOWIN.C -- Displays "Hello, Windows 98!" in client area
                 (c) Charles Petzold, 1998
  ------------------------------------------------------------*/

#include <windows.h>

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("HelloWin") ;
     HWND         hwnd ;
     MSG          msg ;
     WNDCLASS     wndclass ;

     wndclass.style         = CS_HREDRAW | CS_VREDRAW ; // class style. these two means redraw window when the H or S direction changed.
     wndclass.lpfnWndProc   = WndProc ; // callback function address.
     wndclass.cbClsExtra    = 0 ;
     wndclass.cbWndExtra    = 0 ;
     wndclass.hInstance     = hInstance ;
     wndclass.hIcon         = LoadIcon (NULL, IDI_APPLICATION) ;
     wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
     wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH) ;
     wndclass.lpszMenuName  = NULL ; // no menu defined.
     wndclass.lpszClassName = szAppName ; // define the windows class name equals to application name.

     if (!RegisterClass (&wndclass)) // if compile the program by using _UNICODE, RegisterClass will be turned to RegisterClassW which is not availabe on win98
    	                             // so it's a chance to let people know, he is trying to compile a unicode version which is running on win98. that's impossible.
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"), 
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     hwnd = CreateWindow (szAppName,                  // window class name, this should be one of the windows class name that has been registered.
                          TEXT ("The Hello Program"), // window caption
                          WS_OVERLAPPEDWINDOW,        // window style, WS_OVERLAPPEDWINDOW means caption, sysmenu, minimizebox, maximizebox
                          CW_USEDEFAULT,              // initial x position
                          CW_USEDEFAULT,              // initial y position
                          CW_USEDEFAULT,              // initial x size
                          CW_USEDEFAULT,              // initial y size
                          NULL,                       // parent window handle, NULL means the parent window is desktop.
                          NULL,                       // window menu handle
                          hInstance,                  // program instance handle
                          NULL) ;                     // creation parameters
     
     ShowWindow (hwnd, iCmdShow) ; // iCmdShow means SW_SHOWNORMAL SW_HIDDEN etc.
     UpdateWindow (hwnd) ; // invoke this will let Windows send a WM_PAINT message to WinProc function.
     
     while (GetMessage (&msg, NULL, 0, 0)) // the 2, 3, 4 parameters are NULL, 0, 0 means program receive all the messages from all the windows which is created by program itself.
    	                                   // Get the message in message queue. Some messages will be sent to WinProc by Windows System directly, not in message queue.
    	                                   // But messages will always be sent to WndProc one by one(not at the same time) whatever it is in queue or not.
     {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ; // means let Windows System call WndProc callback function with this message and after finishing calling, return control to DispatchMessage.
                                   // and ready to get next message.
     }
     return msg.wParam ; // the value equals to the value which is passed to PostQuitMessage function, 0 is in common.
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     HDC         hdc ;
     PAINTSTRUCT ps ;
     RECT        rect ;
     
     switch (message)
     {
     case WM_CREATE:
          PlaySound (TEXT ("hellowin.wav"), NULL, SND_FILENAME | SND_ASYNC) ;
          return 0 ;

     case WM_PAINT: // move window, other window cover this window, minimize, maximize will force the client area to be valid, then windows sytem call WndProc with WM_PAINT message to redraw the window.
          hdc = BeginPaint (hwnd, &ps) ;
          
          GetClientRect (hwnd, &rect) ; // get client rect
          
          DrawText (hdc, TEXT ("Hello, Windows 98!"), -1, &rect, // -1 means the second parameter is a sz String means string end with '\0'
                    DT_SINGLELINE | DT_CENTER | DT_VCENTER) ; // DT_SINGLELINE means text should be shown in one line. DT_CENTER | DT_VCENTER means according to &rect to draw
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ; // post a WM_QUIT message equals 0 to stop GetMessage function above. GetMessage will return 0 if WM_QUIT message comes.
                                // the while sentence is stopped then, and the program is died.
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ; // this function handle all the messages that WndProc doesn't handle.
}

