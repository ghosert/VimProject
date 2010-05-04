/*---------------------------------------------
   FORMFEED.C -- Advances printer to next page
                 (c) Charles Petzold, 1998
  ---------------------------------------------*/

#include <windows.h>

HDC GetPrinterDC (void) ;

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    LPSTR lpszCmdLine, int iCmdShow)
{
     static DOCINFO di = { sizeof (DOCINFO), TEXT ("FormFeed") } ;
     HDC            hdcPrint = GetPrinterDC () ;
     
     if (hdcPrint != NULL)
     {
          if (StartDoc (hdcPrint, &di) > 0)
               if (StartPage (hdcPrint) > 0 && EndPage (hdcPrint) > 0)
                    EndDoc (hdcPrint) ;
               
          DeleteDC (hdcPrint) ;
     }
     return 0 ;
}

