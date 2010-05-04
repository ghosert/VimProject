/*---------------------------------------
   BOUNCE.C -- Palette Animation Demo
               (c) Charles Petzold, 1998
  ---------------------------------------*/

#include <windows.h>

#define ID_TIMER 1

TCHAR szAppName [] = TEXT ("Bounce") ;
TCHAR szTitle   [] = TEXT ("Bounce: Palette Animation Demo") ;

static LOGPALETTE * plp ;

HPALETTE CreateRoutine (HWND hwnd)
{
     HPALETTE hPalette ;
     int      i ;

     plp = malloc (sizeof (LOGPALETTE) + 33 * sizeof (PALETTEENTRY)) ;

     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = 34 ;

     for (i = 0 ; i < 34 ; i++)
     {
          plp->palPalEntry[i].peRed   = 255 ;
          plp->palPalEntry[i].peGreen = (i == 0 ? 0 : 255) ;
          plp->palPalEntry[i].peBlue  = (i == 0 ? 0 : 255) ;
          plp->palPalEntry[i].peFlags = (i == 33 ? 0 : PC_RESERVED) ;
     }
     hPalette = CreatePalette (plp) ;

     SetTimer (hwnd, ID_TIMER, 50, NULL) ;
     return hPalette ;
}

void PaintRoutine (HDC hdc, int cxClient, int cyClient)
{
     HBRUSH hBrush ;
     int    i, x1, x2, y1, y2 ;
     RECT   rect ;

          // Draw window background using palette index 33

     SetRect (&rect, 0, 0, cxClient, cyClient) ;
     hBrush = CreateSolidBrush (PALETTEINDEX (33)) ;
     FillRect (hdc, &rect, hBrush) ;
     DeleteObject (hBrush) ;

          // Draw the 33 balls

     SelectObject (hdc, GetStockObject (NULL_PEN)) ;

     for (i = 0 ; i < 33 ; i++)
     {
          x1 =  i      * cxClient / 33 ;
          x2 = (i + 1) * cxClient / 33 ;

          if (i < 9)
          {
               y1  = i      * cyClient / 9 ;
               y2 = (i + 1) * cyClient / 9 ;
          }
          else if (i < 17)
          {
               y1 = (16 - i) * cyClient / 9 ;
               y2 = (17 - i) * cyClient / 9 ;
          }
          else if (i < 25)
          {
               y1 = (i - 16) * cyClient / 9 ;
               y2 = (i - 15) * cyClient / 9 ;
          }
          else 
          {
               y1 = (32 - i) * cyClient / 9 ;
               y2 = (33 - i) * cyClient / 9 ;
          }

          hBrush = CreateSolidBrush (PALETTEINDEX (i)) ;
          SelectObject (hdc, hBrush) ;
          Ellipse (hdc, x1, y1, x2, y2) ;
          DeleteObject (SelectObject (hdc, GetStockObject (WHITE_BRUSH))) ;
     }
     return ;
}

void TimerRoutine (HDC hdc, HPALETTE hPalette)
{
     static BOOL bLeftToRight = TRUE ;
     static int  iBall ;

          // Set old ball to white

     plp->palPalEntry[iBall].peGreen = 255 ;
     plp->palPalEntry[iBall].peBlue  = 255 ;

     iBall += (bLeftToRight ? 1 : -1) ;

     if (iBall == (bLeftToRight ? 33 : -1))
     {
          iBall = (bLeftToRight ? 31 : 1) ;
          bLeftToRight ^= TRUE ;
     }

          // Set new ball to red

     plp->palPalEntry[iBall].peGreen = 0 ;
     plp->palPalEntry[iBall].peBlue  = 0 ;

          // Animate the palette

     AnimatePalette (hPalette, 0, 33, plp->palPalEntry) ;
     return ;
}

void DestroyRoutine (HWND hwnd, HPALETTE hPalette)
{
     KillTimer (hwnd, ID_TIMER) ;
     DeleteObject (hPalette) ;
     free (plp) ;
     return ;
}

