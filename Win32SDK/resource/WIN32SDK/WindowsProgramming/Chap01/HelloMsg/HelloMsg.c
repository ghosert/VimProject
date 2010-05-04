/*--------------------------------------------------------------
   HelloMsg.c -- Displays "Hello, Windows 98!" in a message box
                 (c) Charles Petzold, 1998
  --------------------------------------------------------------*/

#include <windows.h>

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     MessageBox (NULL, TEXT ("Hello, Windows 98!"), TEXT ("HelloMsg"), 0) ;

     return 0 ;
}
