/*---------------------------------------
   EMF13.C -- Enhanced Metafile Demo #13
              (c) Charles Petzold, 1998
  ---------------------------------------*/

#include <windows.h>

TCHAR szClass [] = TEXT ("EMF13") ;
TCHAR szTitle [] = TEXT ("EMF13: Enhanced Metafile Demo #13") ;

void CreateRoutine (HWND hwnd)
{
}

void PaintRoutine (HWND hwnd, HDC hdc, int cxArea, int cyArea)
{
     ENHMETAHEADER emh ;
     HENHMETAFILE  hemf ;
     POINT         pt ;
     int           cxImage, cyImage ;
     RECT          rect ;
     
     SetMapMode (hdc, MM_HIMETRIC) ;
     
     SetViewportOrgEx (hdc, 0, cyArea, NULL) ;
     
     pt.x = cxArea ;
     pt.y = 0 ;
     
     DPtoLP (hdc, &pt, 1) ;
     
     hemf = GetEnhMetaFile (TEXT ("..\\emf11\\emf11.emf")) ;
     
     GetEnhMetaFileHeader (hemf, sizeof (emh), &emh) ;
     
     cxImage = emh.rclFrame.right - emh.rclFrame.left ;
     cyImage = emh.rclFrame.bottom - emh.rclFrame.top ;
     
     rect.left   = (pt.x - cxImage) / 2 ;
     rect.top    = (pt.y + cyImage) / 2 ;
     rect.right  = (pt.x + cxImage) / 2 ;
     rect.bottom = (pt.y - cyImage) / 2 ;
     
     PlayEnhMetaFile (hdc, hemf, &rect) ;
     
     DeleteEnhMetaFile (hemf) ;
}
