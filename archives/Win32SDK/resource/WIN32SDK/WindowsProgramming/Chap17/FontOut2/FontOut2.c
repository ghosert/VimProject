/*------------------------------------------
   FONTOUT2.C -- Using Path to Outline Font
                 (c) Charles Petzold, 1998
  ------------------------------------------*/

#include <windows.h>
#include "..\\eztest\\ezfont.h"

TCHAR szAppName [] = TEXT ("FontOut2") ;
TCHAR szTitle [] = TEXT ("FontOut2: Using Path to Outline Font") ;

void PaintRoutine (HWND hwnd, HDC hdc, int cxArea, int cyArea)
{
     static TCHAR szString [] = TEXT ("Outline") ;
     HFONT        hFont ;
     LOGBRUSH     lb ;
     SIZE         size ;

     hFont = EzCreateFont (hdc, TEXT ("Times New Roman"), 1440, 0, 0, TRUE) ;

     SelectObject (hdc, hFont) ;
     SetBkMode (hdc, TRANSPARENT) ;

     GetTextExtentPoint32 (hdc, szString, lstrlen (szString), &size) ;

     BeginPath (hdc) ;
     TextOut (hdc, (cxArea - size.cx) / 2, (cyArea - size.cy) / 2,
                    szString, lstrlen (szString)) ;
     EndPath (hdc) ;

     lb.lbStyle = BS_SOLID ;
     lb.lbColor = RGB (255, 0, 0) ;
     lb.lbHatch = 0 ;

     SelectObject (hdc, ExtCreatePen (PS_GEOMETRIC | PS_DOT,
                               GetDeviceCaps (hdc, LOGPIXELSX) / 24,
                                   &lb, 0, NULL)) ;
     StrokePath (hdc) ;

     DeleteObject (SelectObject (hdc, GetStockObject (BLACK_PEN))) ;
     SelectObject (hdc, GetStockObject (SYSTEM_FONT)) ;
     DeleteObject (hFont) ;
}
