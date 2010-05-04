/*---------------------------------------------------
   BACHTOCC.C -- Bach Toccata in D Minor (First Bar)
                 (c) Charles Petzold, 1998
  ---------------------------------------------------*/

#include <windows.h>

#define ID_TIMER    1

LRESULT CALLBACK WndProc (HWND, UINT, WPARAM, LPARAM) ;

TCHAR szAppName[] = TEXT ("BachTocc") ;

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
     wndclass.hbrBackground = GetStockObject (WHITE_BRUSH) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     
     if (!RegisterClass (&wndclass))
     {
          MessageBox (NULL, TEXT ("This program requires Windows NT!"),
                      szAppName, MB_ICONERROR) ;
          return 0 ;
     }
     
     hwnd = CreateWindow (szAppName, 
                          TEXT ("Bach Toccata in D Minor (First Bar)"),
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

DWORD MidiOutMessage (HMIDIOUT hMidi, int iStatus, int iChannel,
                                      int iData1,  int iData2)
{
     DWORD dwMessage = iStatus | iChannel | (iData1 << 8) | (iData2 << 16) ;
     
     return midiOutShortMsg (hMidi, dwMessage) ;
}

LRESULT CALLBACK WndProc (HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam)
{
     static struct
     {
          int iDur ;
          int iNote [2] ;
     }
     noteseq [] = { 110, 69, 81,  110, 67, 79,  990, 69, 81,  220, -1, -1,
                    110, 67, 79,  110, 65, 77,  110, 64, 76,  110, 62, 74,
                    220, 61, 73,  440, 62, 74, 1980, -1, -1,  110, 57, 69,
                    110, 55, 67,  990, 57, 69,  220, -1, -1,  220, 52, 64,
                    220, 53, 65,  220, 49, 61,  440, 50, 62, 1980, -1, -1 } ;
     
     static HMIDIOUT hMidiOut ;
     static int      iIndex ;
     int             i ;
     
     switch (message)
     {
     case WM_CREATE:
               // Open MIDIMAPPER device
          
          if (midiOutOpen (&hMidiOut, MIDIMAPPER, 0, 0, 0))
          {
               MessageBeep (MB_ICONEXCLAMATION) ;
               MessageBox (hwnd, TEXT ("Cannot open MIDI output device!"),
                                 szAppName, MB_ICONEXCLAMATION | MB_OK) ;
               return -1 ;
          }
               // Send Program Change messages for "Church Organ"
          
          MidiOutMessage (hMidiOut, 0xC0,  0, 19, 0) ;

          SetTimer (hwnd, ID_TIMER, 1000, NULL) ;
          return 0 ;
          
     case WM_TIMER:
              // Loop for 2-note polyphony
          
          for (i = 0 ; i < 2 ; i++)
          {
                    // Note Off messages for previous note
               
               if (iIndex != 0 && noteseq[iIndex - 1].iNote[i] != -1)
               {
                    MidiOutMessage (hMidiOut, 0x80,  0,
                                    noteseq[iIndex - 1].iNote[i], 0) ;
               }
                    // Note On messages for new note
               
               if (iIndex != sizeof (noteseq) / sizeof (noteseq[0]) &&
                    noteseq[iIndex].iNote[i] != -1)
               {
                    MidiOutMessage (hMidiOut, 0x90,  0,
                                    noteseq[iIndex].iNote[i], 127) ;
               }
          }
          
          if (iIndex != sizeof (noteseq) / sizeof (noteseq[0]))
          {
               SetTimer (hwnd, ID_TIMER, noteseq[iIndex++].iDur - 1, NULL) ;
          }
          else
          {
               KillTimer (hwnd, ID_TIMER) ;
               DestroyWindow (hwnd) ;
          }
          return 0 ;
          
     case WM_DESTROY:
          midiOutReset (hMidiOut) ;
          midiOutClose (hMidiOut) ;
          PostQuitMessage (0) ;
          return 0 ;
     }
     return DefWindowProc (hwnd, message, wParam, lParam) ;
}
