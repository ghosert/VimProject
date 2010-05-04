/*----------------------------------------
   DIBPAL.C -- Palette-Creation Functions
               (c) Charles Petzold, 1998
  ----------------------------------------*/

#include <windows.h>
#include "dibhelp.h"
#include "dibpal.h"

/*------------------------------------------------------------
   DibPalDibTable: Creates a palette from the DIB color table
  ------------------------------------------------------------*/

HPALETTE DibPalDibTable (HDIB hdib)
{
     HPALETTE     hPalette ;
     int          i, iNum ;
     LOGPALETTE * plp ;
     RGBQUAD      rgb ;

     if (0 == (iNum = DibNumColors (hdib)))
          return NULL ;

     plp = malloc (sizeof (LOGPALETTE) + (iNum - 1) * sizeof (PALETTEENTRY)) ;

     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = iNum ;

     for (i = 0 ; i < iNum ; i++)
     {
          DibGetColor (hdib, i, &rgb) ;

          plp->palPalEntry[i].peRed   = rgb.rgbRed ;
          plp->palPalEntry[i].peGreen = rgb.rgbGreen ;
          plp->palPalEntry[i].peBlue  = rgb.rgbBlue ;
          plp->palPalEntry[i].peFlags = 0 ;
     }
     hPalette = CreatePalette (plp) ;
     free (plp) ;
     return hPalette ;
}

/*------------------------------------------------------------------------
   DibPalAllPurpose: Creates a palette suitable for a wide variety
          of images; the palette has 247 entries, but 15 of them are 
          duplicates or match the standard 20 colors.
  ------------------------------------------------------------------------*/

HPALETTE DibPalAllPurpose (void)
{
     HPALETTE     hPalette ;
     int          i, incr, R, G, B ;
     LOGPALETTE * plp ;

     plp = malloc (sizeof (LOGPALETTE) + 246 * sizeof (PALETTEENTRY)) ;

     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = 247 ;

          // The following loop calculates 31 grays shades, but 3 of them
          //        will match the standard 20 colors

     for (i = 0, G = 0, incr = 8 ; G <= 0xFF ; i++, G += incr)
     {
          plp->palPalEntry[i].peRed   = (BYTE) G ;
          plp->palPalEntry[i].peGreen = (BYTE) G ;
          plp->palPalEntry[i].peBlue  = (BYTE) G ;
          plp->palPalEntry[i].peFlags = 0 ;

          incr = (incr == 9 ? 8 : 9) ;
     }

          // The following loop is responsible for 216 entries, but 8 of 
          //        them will match the standard 20 colors, and another
          //        4 of them will match the gray shades above.

     for (R = 0 ; R <= 0xFF ; R += 0x33)
     for (G = 0 ; G <= 0xFF ; G += 0x33)
     for (B = 0 ; B <= 0xFF ; B += 0x33)
     {
          plp->palPalEntry[i].peRed   = (BYTE) R ;
          plp->palPalEntry[i].peGreen = (BYTE) G ;
          plp->palPalEntry[i].peBlue  = (BYTE) B ;
          plp->palPalEntry[i].peFlags = 0 ;

          i++ ;
     }
     hPalette = CreatePalette (plp) ;

     free (plp) ;
     return hPalette ;
}

/*------------------------------------------------------------------------
   DibPalUniformGrays:  Creates a palette of iNum grays, uniformly spaced
  ------------------------------------------------------------------------*/

HPALETTE DibPalUniformGrays (int iNum)
{
     HPALETTE     hPalette ;
     int          i ;
     LOGPALETTE * plp ;

     plp = malloc (sizeof (LOGPALETTE) + (iNum - 1) * sizeof (PALETTEENTRY)) ;

     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = iNum ;

     for (i = 0 ; i < iNum ; i++)
     {
          plp->palPalEntry[i].peRed   =
          plp->palPalEntry[i].peGreen = 
          plp->palPalEntry[i].peBlue  = (BYTE) (i * 255 / (iNum - 1)) ;
          plp->palPalEntry[i].peFlags = 0 ;
     }
     hPalette = CreatePalette (plp) ;
     free (plp) ;
     return hPalette ;
}

/*------------------------------------------------------------------------
   DibPalUniformColors: Creates a palette of iNumR x iNumG x iNumB colors
  ------------------------------------------------------------------------*/

HPALETTE DibPalUniformColors (int iNumR, int iNumG, int iNumB)
{
     HPALETTE     hPalette ;
     int          i, iNum, R, G, B ;
     LOGPALETTE * plp ;

     iNum = iNumR * iNumG * iNumB ;

     plp = malloc (sizeof (LOGPALETTE) + (iNum - 1) * sizeof (PALETTEENTRY)) ;

     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = iNumR * iNumG * iNumB ;

     i = 0 ;
     for (R = 0 ; R < iNumR ; R++)
     for (G = 0 ; G < iNumG ; G++)
     for (B = 0 ; B < iNumB ; B++)
     {
          plp->palPalEntry[i].peRed   = (BYTE) (R * 255 / (iNumR - 1)) ;
          plp->palPalEntry[i].peGreen = (BYTE) (G * 255 / (iNumG - 1)) ;
          plp->palPalEntry[i].peBlue  = (BYTE) (B * 255 / (iNumB - 1)) ;
          plp->palPalEntry[i].peFlags = 0 ;

          i++ ;
     }
     hPalette = CreatePalette (plp) ;
     free (plp) ;
     return hPalette ;
}

/*---------------------------------------------------------------
   DibPalVga:  Creates a palette based on standard 16 VGA colors
  ---------------------------------------------------------------*/

HPALETTE DibPalVga (void)
{
     static RGBQUAD rgb [16] = { 0x00, 0x00, 0x00, 0x00,
                                 0x00, 0x00, 0x80, 0x00,
                                 0x00, 0x80, 0x00, 0x00,
                                 0x00, 0x80, 0x80, 0x00,
                                 0x80, 0x00, 0x00, 0x00,
                                 0x80, 0x00, 0x80, 0x00,
                                 0x80, 0x80, 0x00, 0x00,
                                 0x80, 0x80, 0x80, 0x00,
                                 0xC0, 0xC0, 0xC0, 0x00,
                                 0x00, 0x00, 0xFF, 0x00,
                                 0x00, 0xFF, 0x00, 0x00,
                                 0x00, 0xFF, 0xFF, 0x00,
                                 0xFF, 0x00, 0x00, 0x00,
                                 0xFF, 0x00, 0xFF, 0x00,
                                 0xFF, 0xFF, 0x00, 0x00,
                                 0xFF, 0xFF, 0xFF, 0x00 } ;
     HPALETTE       hPalette ;
     int            i ;
     LOGPALETTE   * plp ;

     plp = malloc (sizeof (LOGPALETTE) + 15 * sizeof (PALETTEENTRY)) ;

     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = 16 ;

     for (i = 0 ; i < 16 ; i++)
     {
          plp->palPalEntry[i].peRed   = rgb[i].rgbRed ;
          plp->palPalEntry[i].peGreen = rgb[i].rgbGreen ;
          plp->palPalEntry[i].peBlue  = rgb[i].rgbBlue ;
          plp->palPalEntry[i].peFlags = 0 ;
     }
     hPalette = CreatePalette (plp) ;
     free (plp) ;
     return hPalette ;
}

/*---------------------------------------------
   Macro used in palette optimization routines
  ---------------------------------------------*/

#define PACK_RGB(R,G,B,iRes) ((int) (R) | ((int) (G) <<  (iRes)) |       \
                                          ((int) (B) << ((iRes) + (iRes))))

/*--------------------------------------------------------------------
   AccumColorCounts: Fills up piCount (indexed by a packed RGB color)
     with counts of pixels of that color.
  --------------------------------------------------------------------*/

static void AccumColorCounts (HDIB hdib, int * piCount, int iRes)
{
     int     x, y, cx, cy ;
     RGBQUAD rgb ;

     cx = DibWidth (hdib) ;
     cy = DibHeight (hdib) ;

     for (y = 0 ; y < cy ; y++)
     for (x = 0 ; x < cx ; x++)
     {
          DibGetPixelColor (hdib, x, y, &rgb) ;

          rgb.rgbRed   >>= (8 - iRes) ;
          rgb.rgbGreen >>= (8 - iRes) ;
          rgb.rgbBlue  >>= (8 - iRes) ;

          ++piCount [PACK_RGB (rgb.rgbRed, rgb.rgbGreen, rgb.rgbBlue, iRes)] ;
     }
}

/*--------------------------------------------------------------
   DibPalPopularity:  Popularity algorithm for optimized colors
  --------------------------------------------------------------*/

HPALETTE DibPalPopularity (HDIB hdib, int iRes)
{
     HPALETTE     hPalette ;
     int          i, iArraySize, iEntry, iCount, iIndex, iMask, R, G, B ;
     int        * piCount ;
     LOGPALETTE * plp ;

          // Validity checks
    
     if (DibBitCount (hdib) < 16)
          return NULL ;

     if (iRes < 3 || iRes > 8)
          return NULL ;

          // Allocate array for counting pixel colors

     iArraySize = 1 << (3 * iRes) ;
     iMask = (1 << iRes) - 1 ;

     if (NULL == (piCount = calloc (iArraySize, sizeof (int))))
          return NULL ;

          // Get the color counts

     AccumColorCounts (hdib, piCount, iRes) ;

          // Set up a palette

     plp = malloc (sizeof (LOGPALETTE) + 235 * sizeof (PALETTEENTRY)) ;

     plp->palVersion = 0x0300 ;

     for (iEntry = 0 ; iEntry < 236 ; iEntry++)
     {
          for (i = 0, iCount = 0 ; i < iArraySize ; i++)
               if (piCount[i] > iCount)
               {
                    iCount = piCount[i] ;
                    iIndex = i ;
               }

          if (iCount == 0)
               break ;

          R = (iMask &  iIndex                  ) << (8 - iRes) ;
          G = (iMask & (iIndex >>         iRes )) << (8 - iRes) ;
          B = (iMask & (iIndex >> (iRes + iRes))) << (8 - iRes) ;

          plp->palPalEntry[iEntry].peRed   = (BYTE) R ; 
          plp->palPalEntry[iEntry].peGreen = (BYTE) G ; 
          plp->palPalEntry[iEntry].peBlue  = (BYTE) B ; 
          plp->palPalEntry[iEntry].peFlags = 0 ;

          piCount [iIndex] = 0 ;
     }
          // On exit from the loop iEntry will be the number of stored entries

     plp->palNumEntries = iEntry ;

          // Create the palette, clean up, and return the palette handle

     hPalette = CreatePalette (plp) ;

     free (piCount) ;
     free (plp) ;

     return hPalette ;
}

/*-------------------------------------------------------
   Structures used for implementing median cut algorithm
  -------------------------------------------------------*/

typedef struct           // defines dimension of a box
{
     int Rmin, Rmax, Gmin, Gmax, Bmin, Bmax ;
}
MINMAX ;

typedef struct           // for Compare routine for qsort
{
     int     iBoxCount ;
     RGBQUAD rgbBoxAv ;
}
BOXES ;

/*----------------------------
   FindAverageColor: In a box
  ----------------------------*/

static int FindAverageColor (int * piCount, MINMAX mm, 
                             int iRes, RGBQUAD * prgb)
{
     int R, G, B, iR, iG, iB, iTotal, iCount ;

          // Initialize some variables

     iTotal = iR = iG = iB = 0 ;
          
          // Loop through all colors in the box
          
     for (R = mm.Rmin ; R <= mm.Rmax ; R++)
     for (G = mm.Gmin ; G <= mm.Gmax ; G++)
     for (B = mm.Bmin ; B <= mm.Bmax ; B++)
     {
               // Get the number of pixels of that color

          iCount = piCount [PACK_RGB (R, G, B, iRes)] ;

               // Weight the pixel count by the color value

          iR += iCount * R ;
          iG += iCount * G ;
          iB += iCount * B ;

          iTotal += iCount ;
     }
          // Find the average color

     prgb->rgbRed   = (BYTE) ((iR / iTotal) << (8 - iRes)) ;
     prgb->rgbGreen = (BYTE) ((iG / iTotal) << (8 - iRes)) ;
     prgb->rgbBlue  = (BYTE) ((iB / iTotal) << (8 - iRes)) ;

          // Return the total number of pixels in the box
     
     return iTotal ;
}

/*------------------------------
   CutBox:  Divide a box in two
  ------------------------------*/

static void CutBox (int * piCount, int iBoxCount, MINMAX mm,
                    int iRes, int iLevel, BOXES * pboxes, int * piEntry)
{
     int    iCount, R, G, B ;
     MINMAX mmNew ;
     
          // If the box is empty, return 

     if (iBoxCount == 0)
          return ;

          // If the nesting level is 8, or the box is one pixel, we're ready
          //   to find the average color in the box and save it along with
          //   the number of pixels of that color

     if (iLevel == 8 || (mm.Rmin == mm.Rmax && 
                         mm.Gmin == mm.Gmax && 
                         mm.Bmin == mm.Bmax))
     {
          pboxes[*piEntry].iBoxCount = 
               FindAverageColor (piCount, mm, iRes, &pboxes[*piEntry].rgbBoxAv) ;

          (*piEntry) ++ ;
     }
          // Otherwise, if blue is the largest side, split it

     else if ((mm.Bmax - mm.Bmin > mm.Rmax - mm.Rmin) && 
              (mm.Bmax - mm.Bmin > mm.Gmax - mm.Gmin))
     {
               // Initialize a counter and loop through the blue side

          iCount = 0 ;

          for (B = mm.Bmin ; B < mm.Bmax ; B++)
          {
                    // Accumulate all the pixels for each successive blue value

               for (R = mm.Rmin ; R <= mm.Rmax ; R++)
               for (G = mm.Gmin ; G <= mm.Gmax ; G++)
                    iCount += piCount [PACK_RGB (R, G, B, iRes)] ;

                    // If it's more than half the box count, we're there

               if (iCount >= iBoxCount / 2)
                    break ;

                    // If the next blue value will be the max, we're there

               if (B == mm.Bmax - 1)
                    break ;
          }
               // Cut the two split boxes.
               //   The second argument to CutBox is the new box count.
               //   The third argument is the new min and max values.

          mmNew = mm ;
          mmNew.Bmin = mm.Bmin ;
          mmNew.Bmax = B ;

          CutBox (piCount, iCount, mmNew, iRes, iLevel + 1, 
                  pboxes, piEntry) ;

          mmNew.Bmin = B + 1 ;
          mmNew.Bmax = mm.Bmax ;

          CutBox (piCount, iBoxCount - iCount, mmNew, iRes, iLevel + 1,
                  pboxes, piEntry) ;
     }
          // Otherwise, if red is the largest side, split it (just like blue)

     else if (mm.Rmax - mm.Rmin > mm.Gmax - mm.Gmin)
     {
          iCount = 0 ;

          for (R = mm.Rmin ; R < mm.Rmax ; R++)
          {
               for (B = mm.Bmin ; B <= mm.Bmax ; B++)
               for (G = mm.Gmin ; G <= mm.Gmax ; G++)
                    iCount += piCount [PACK_RGB (R, G, B, iRes)] ;

               if (iCount >= iBoxCount / 2)
                    break ;

               if (R == mm.Rmax - 1)
                    break ;
          }
          mmNew = mm ;
          mmNew.Rmin = mm.Rmin ;
          mmNew.Rmax = R ;

          CutBox (piCount, iCount, mmNew, iRes, iLevel + 1, 
                  pboxes, piEntry) ;

          mmNew.Rmin = R + 1 ;
          mmNew.Rmax = mm.Rmax ;

          CutBox (piCount, iBoxCount - iCount, mmNew, iRes, iLevel + 1,
                  pboxes, piEntry) ;
     }
          // Otherwise, split along the green size
     else 
     {
          iCount = 0 ;

          for (G = mm.Gmin ; G < mm.Gmax ; G++)
          {
               for (B = mm.Bmin ; B <= mm.Bmax ; B++)
               for (R = mm.Rmin ; R <= mm.Rmax ; R++)
                    iCount += piCount [PACK_RGB (R, G, B, iRes)] ;

               if (iCount >= iBoxCount / 2)
                    break ;

               if (G == mm.Gmax - 1)
                    break ;
          }
          mmNew = mm ;
          mmNew.Gmin = mm.Gmin ;
          mmNew.Gmax = G ;

          CutBox (piCount, iCount, mmNew, iRes, iLevel + 1, 
                  pboxes, piEntry) ;

          mmNew.Gmin = G + 1 ;
          mmNew.Gmax = mm.Gmax ;

          CutBox (piCount, iBoxCount - iCount, mmNew, iRes, iLevel + 1,
                  pboxes, piEntry) ;
     }
}

/*---------------------------
   Compare routine for qsort
  ---------------------------*/

static int Compare (const BOXES * pbox1, const BOXES * pbox2)
{
     return pbox1->iBoxCount - pbox2->iBoxCount ;
}

/*-----------------------------------------------------------------
   DibPalMedianCut:  Creates palette based on median cut algorithm
  -----------------------------------------------------------------*/

HPALETTE DibPalMedianCut (HDIB hdib, int iRes)
{
     BOXES        boxes [256] ;
     HPALETTE     hPalette ;
     int          i, iArraySize, iCount, R, G, B, iTotCount, iDim, iEntry = 0 ;
     int        * piCount ;
     LOGPALETTE * plp ;
     MINMAX       mm ;

          // Validity checks
    
     if (DibBitCount (hdib) < 16)
          return NULL ;

     if (iRes < 3 || iRes > 8)
          return NULL ;

          // Accumulate counts of pixel colors

     iArraySize = 1 << (3 * iRes) ;

     if (NULL == (piCount = calloc (iArraySize, sizeof (int))))
          return NULL ;

     AccumColorCounts (hdib, piCount, iRes) ;

          // Find the dimensions of the total box

     iDim = 1 << iRes ;

     mm.Rmin = mm.Gmin = mm.Bmin = iDim - 1 ;
     mm.Rmax = mm.Gmax = mm.Bmax = 0 ;

     iTotCount = 0 ;

     for (R = 0 ; R < iDim ; R++)
     for (G = 0 ; G < iDim ; G++)
     for (B = 0 ; B < iDim ; B++)
          if ((iCount = piCount [PACK_RGB (R, G, B, iRes)]) > 0)
          {
               iTotCount += iCount ;

               if (R < mm.Rmin) mm.Rmin = R ;
               if (G < mm.Gmin) mm.Gmin = G ;
               if (B < mm.Bmin) mm.Bmin = B ;

               if (R > mm.Rmax) mm.Rmax = R ;
               if (G > mm.Gmax) mm.Gmax = G ;
               if (B > mm.Bmax) mm.Bmax = B ;
          }

          // Cut the first box (iterative function).
          //   On return, the boxes structure will have up to 256 RGB values, 
          //        one for each of the boxes, and the number of pixels in
          //        each box.
          //   The iEntry value will indicate the number of non-empty boxes.

     CutBox (piCount, iTotCount, mm, iRes, 0, boxes, &iEntry) ;
     free (piCount) ;

          // Sort the RGB table by the number of pixels for each color

     qsort (boxes, iEntry, sizeof (BOXES), Compare) ;

     plp = malloc (sizeof (LOGPALETTE) + (iEntry - 1) * sizeof (PALETTEENTRY)) ;

     if (plp == NULL)
          return NULL ;

     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = iEntry ;

     for (i = 0 ; i < iEntry ; i++)
     {
          plp->palPalEntry[i].peRed   = boxes[i].rgbBoxAv.rgbRed ;
          plp->palPalEntry[i].peGreen = boxes[i].rgbBoxAv.rgbGreen ;
          plp->palPalEntry[i].peBlue  = boxes[i].rgbBoxAv.rgbBlue ;
          plp->palPalEntry[i].peFlags = 0 ;
     }

     hPalette = CreatePalette (plp) ;

     free (plp) ;
     return hPalette ;
}