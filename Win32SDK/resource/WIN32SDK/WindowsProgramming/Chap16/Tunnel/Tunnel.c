/*---------------------------------------
   TUNNEL.C -- Palette Animation Demo
               (c) Charles Petzold, 1998
  ---------------------------------------*/

#include <windows.h>

#define ID_TIMER 1

TCHAR szAppName [] = TEXT ("Tunnel") ;
TCHAR szTitle   [] = TEXT ("Tunnel: Palette Animation Demo") ;

static LOGPALETTE * plp ;

HPALETTE CreateRoutine (HWND hwnd)
{
     BYTE     byGrayLevel ;
     HPALETTE hPalette ;
     int      i ;

     plp = malloc (sizeof (LOGPALETTE) + 255 * sizeof (PALETTEENTRY)) ;
     
          // Initialize the fields of the LOGPALETTE structure
     
     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = 128 ;
     
     for (i = 0 ; i < 128 ; i++)
     {
          if (i < 64)
               byGrayLevel = (BYTE) (4 * i) ;
          else
               byGrayLevel = (BYTE) min (255, 4 * (128 - i)) ;
          
          plp->palPalEntry[i].peRed   = byGrayLevel ;
          plp->palPalEntry[i].peGreen = byGrayLevel ;
          plp->palPalEntry[i].peBlue  = byGrayLevel ;
          plp->palPalEntry[i].peFlags = PC_RESERVED ;
          
          plp->palPalEntry[i + 128].peRed   = byGrayLevel ;
          plp->palPalEntry[i + 128].peGreen = byGrayLevel ;
          plp->palPalEntry[i + 128].peBlue  = byGrayLevel ;
          plp->palPalEntry[i + 128].peFlags = PC_RESERVED ;
     }
   
     hPalette = CreatePalette (plp) ;
     
     SetTimer (hwnd, ID_TIMER, 50, NULL) ;
     return hPalette ;
}

void PaintRoutine (HDC hdc, int cxClient, int cyClient)
{
     HBRUSH hBrush ;
     int    i ;
     RECT   rect ;
     
     for (i = 0 ; i < 127 ; i++)
     {
               // Use a RECT structure for each of 128 rectangles
          
          rect.left   =            i * cxClient / 255 ;
          rect.top    =            i * cyClient / 255 ;
          rect.right  = cxClient - i * cxClient / 255 ;
          rect.bottom = cyClient - i * cyClient / 255 ;
          
          hBrush = CreateSolidBrush (PALETTEINDEX (i)) ;
          
               // Fill the rectangle and delete the brush
          
          FillRect (hdc, &rect, hBrush) ;
          DeleteObject (hBrush) ;
     }
     return ;
}

void TimerRoutine (HDC hdc, HPALETTE hPalette)
{
     static int iLevel ;

     iLevel = (iLevel + 1) % 128 ;

     AnimatePalette (hPalette, 0, 128, plp->palPalEntry + iLevel) ;
     return ;
}

void DestroyRoutine (HWND hwnd, HPALETTE hPalette)
{
     KillTimer (hwnd, ID_TIMER) ;
     DeleteObject (hPalette) ;
     free (plp) ;
     return ;
}
