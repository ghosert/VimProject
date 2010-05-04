/*----------------------------------------
   BIGJOB2.C -- Multithreading Demo
                (c) Charles Petzold, 1998
  ----------------------------------------*/

#include <windows.h>
#include <math.h>
#include <process.h>

#define REP              1000000

#define STATUS_READY     0
#define STATUS_WORKING   1
#define STATUS_DONE      2

#define WM_CALC_DONE     (WM_USER + 0)
#define WM_CALC_ABORTED  (WM_USER + 1)

typedef struct
{
     HWND   hwnd ;
     HANDLE hEvent ;
     BOOL   bContinue ;
}
PARAMS, *PPARAMS ;

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static TCHAR szAppName[] = TEXT ("BigJob2") ;
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
     
     hwnd = CreateWindow (szAppName, TEXT ("Multithreading Demo"),
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

// Compared with BigJob1, we always have one thread waiting for ui thread notify it.
void Thread (PVOID pvoid)
{
     double    A = 1.0 ;
     INT       i ;
     LONG      lTime ;
     volatile  PPARAMS pparams ;
     
     pparams = (PPARAMS) pvoid ;
     
     while (TRUE)
     {
    	  // If the second parameter(dwTimeOut) is INFINITE, if someone has already set event it will return immediately or the thread will wait until set event is called.
    	  // dwTimeOut can be a milli second means if there is no event, you don't have to wait forever, you can return when "time is out". Generally it is INFINITE.
    	  // WaitForSingleObject like wait in Java.
          WaitForSingleObject (pparams->hEvent, INFINITE) ;
          
          lTime = GetCurrentTime () ;
          
          for (i = 0 ; i < REP && pparams->bContinue ; i++)
               A = tan (atan (exp (log (sqrt (A * A))))) + 1.0 ;

          if (i == REP)
          {
               lTime = GetCurrentTime () - lTime ;

               PostMessage (pparams->hwnd, WM_CALC_DONE, 0, lTime) ;
          }
          else
               PostMessage (pparams->hwnd, WM_CALC_ABORTED, 0, 0) ;
     }
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static HANDLE  hEvent ;
     static INT     iStatus ;
     static LONG    lTime ;
     static PARAMS  params ;
     static TCHAR * szMessage[] = { TEXT ("Ready (left mouse button begins)"),
                                    TEXT ("Working (right mouse button ends)"),
                                    TEXT ("%d repetitions in %ld msec") } ;
     HDC            hdc ;
     PAINTSTRUCT    ps ;
     RECT           rect ;
     TCHAR          szBuffer[64] ;
     
     switch (message)
     {
     case WM_CREATE:
    	  // Here the first and the last parameter will be only used when there is multi-process shared data. Or they should be null.
    	  // The first parameter is the pointer of SECURITY_ATTRIBUTES, the last paramter is a event object name.
    	  // The second paramter is fManual, true means the event should be reset by manually, false means the event will be reset automaticly.
    	  // That means you dont't have to invoke ResetEvent function to reset event anymore. Generally, this parameter is false. (Java way)
    	  // If it is true, maybe we can implement Topic model like JMS.
    	  // The third paramter is fInitial, true means the event is initialized as "there is already a signal at beginning",
    	  // false means the event is initialized as "there is no signal at beginning." So the thread will be blocked if you firt call WaitForSingleObject function.
    	  // Generally, this parameter is false.
          hEvent = CreateEvent (NULL, FALSE, FALSE, NULL) ;
          
          params.hwnd = hwnd ;
          params.hEvent = hEvent ;
          params.bContinue = FALSE ;
          
          _beginthread (Thread, 0, &params) ;
          
          return 0 ;
          
     case WM_LBUTTONDOWN:
          if (iStatus == STATUS_WORKING)
          {
               MessageBeep (0) ;
               return 0 ;
          }
          iStatus = STATUS_WORKING ;
          params.bContinue = TRUE ;
          
          // Set event to notify WaitForSingleObject don't wait any more. Like notify in java.
          SetEvent (hEvent) ;
        
          InvalidateRect (hwnd, NULL, TRUE) ;
          return 0 ;
          
     case WM_RBUTTONDOWN:
          params.bContinue = FALSE ;
          return 0 ;
          
     case WM_CALC_DONE:
          lTime = lParam ;
          iStatus = STATUS_DONE ;
          InvalidateRect (hwnd, NULL, TRUE) ;
          return 0 ;
          
     case WM_CALC_ABORTED:
          iStatus = STATUS_READY ;
          InvalidateRect (hwnd, NULL, TRUE) ;
          return 0 ;
          
     case WM_PAINT:
          hdc = BeginPaint (hwnd, &ps) ;
          
          GetClientRect (hwnd, &rect) ;
          
          wsprintf (szBuffer, szMessage[iStatus], REP, lTime) ;
          DrawText (hdc, szBuffer, -1, &rect,
                    DT_SINGLELINE | DT_CENTER | DT_VCENTER) ;
          
          EndPaint (hwnd, &ps) ;
          return 0 ;
          
     case WM_DESTROY:
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}

