/*----------------------------------------
   FONTROT.C -- Rotated Fonts
                (c) Charles Petzold, 1998
  ----------------------------------------*/

#include <windows.h>
#include "..\\eztest\\ezfont.h"

TCHAR szAppName [] = TEXT ("FontRot") ;
TCHAR szTitle   [] = TEXT ("FontRot: Rotated Fonts") ;

void PaintRoutine (HWND hwnd, HDC hdc, int cxArea, int cyArea)
{
     static TCHAR szString [] = TEXT ("   Rotation") ;
     HFONT        hFont ;
     int          i ;
     LOGFONT      lf ;

     hFont = EzCreateFont (hdc, TEXT ("Times New Roman"), 540, 0, 0, TRUE) ;
     GetObject (hFont, sizeof (LOGFONT), &lf) ;
     DeleteObject (hFont) ;

     SetBkMode (hdc, TRANSPARENT) ;
     SetTextAlign (hdc, TA_BASELINE) ;
     SetViewportOrgEx (hdc, cxArea / 2, cyArea / 2, NULL) ;

     for (i = 0 ; i < 12 ; i ++)
     {
          lf.lfEscapement = lf.lfOrientation = i * 300 ;
          SelectObject (hdc, CreateFontIndirect (&lf)) ;

          TextOut (hdc, 0, 0, szString, lstrlen (szString)) ;

          DeleteObject (SelectObject (hdc, GetStockObject (SYSTEM_FONT))) ;
     }
}
