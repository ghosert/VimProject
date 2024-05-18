/*--------------------------------------
   FADER.C -- Palette Animation Demo
              (c) Charles Petzold, 1998
  --------------------------------------*/

#include <windows.h>

#define ID_TIMER 1

TCHAR szAppName [] = TEXT ("Fader") ;
TCHAR szTitle   [] = TEXT ("Fader: Palette Animation Demo") ;

static LOGPALETTE lp ;

HPALETTE CreateRoutine (HWND hwnd)
{
     HPALETTE hPalette ;
     
     lp.palVersion             = 0x0300 ;
     lp.palNumEntries          = 1 ;
     lp.palPalEntry[0].peRed   = 255 ;
     lp.palPalEntry[0].peGreen = 255 ;
     lp.palPalEntry[0].peBlue  = 255 ;
     lp.palPalEntry[0].peFlags = PC_RESERVED ;
   
     hPalette = CreatePalette (&lp) ;
     
     SetTimer (hwnd, ID_TIMER, 50, NULL) ;
     return hPalette ;
}

void PaintRoutine (HDC hdc, int cxClient, int cyClient)
{
     static TCHAR szText [] = TEXT (" Fade In and Out ") ;
     int          x, y ;
     SIZE         sizeText ;

     SetTextColor (hdc, PALETTEINDEX (0)) ;
     GetTextExtentPoint32 (hdc, szText, lstrlen (szText), &sizeText) ;

     for (x = 0 ; x < cxClient ; x += sizeText.cx)
     for (y = 0 ; y < cyClient ; y += sizeText.cy)
     {
          TextOut (hdc, x, y, szText, lstrlen (szText)) ;
     }
     return ;
}

void TimerRoutine (HDC hdc, HPALETTE hPalette)
{
     static BOOL bFadeIn = TRUE ;

     if (bFadeIn)
     {
          lp.palPalEntry[0].peRed   -= 4 ;
          lp.palPalEntry[0].peGreen -= 4 ;

          if (lp.palPalEntry[0].peRed == 3)
               bFadeIn = FALSE ;
     }
     else
     {
          lp.palPalEntry[0].peRed   += 4 ;
          lp.palPalEntry[0].peGreen += 4 ;

          if (lp.palPalEntry[0].peRed == 255)
               bFadeIn = TRUE ;
     }

     AnimatePalette (hPalette, 0, 1, lp.palPalEntry) ;
     return ;
}

void DestroyRoutine (HWND hwnd, HPALETTE hPalette)
{
     KillTimer (hwnd, ID_TIMER) ;
     DeleteObject (hPalette) ;
     return ;
}
