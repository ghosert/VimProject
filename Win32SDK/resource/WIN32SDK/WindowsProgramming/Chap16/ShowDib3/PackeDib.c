/*----------------------------------------------
   PACKEDIB.C -- Routines for using packed DIBs
                 (c) Charles Petzold, 1998
  ----------------------------------------------*/

#include <windows.h>

/*---------------------------------------------------------
   PackedDibLoad: Load DIB File as Packed-Dib Memory Block
  ---------------------------------------------------------*/

BITMAPINFO * PackedDibLoad (PTSTR szFileName)
{
     BITMAPFILEHEADER bmfh ;
     BITMAPINFO     * pbmi ;
     BOOL             bSuccess ;
     DWORD            dwPackedDibSize, dwBytesRead ;
     HANDLE           hFile ;

          // Open the file: read access, prohibit write access

     hFile = CreateFile (szFileName, GENERIC_READ, FILE_SHARE_READ, NULL, 
                         OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL) ;

     if (hFile == INVALID_HANDLE_VALUE)
          return NULL ;

          // Read in the BITMAPFILEHEADER

     bSuccess = ReadFile (hFile, &bmfh, sizeof (BITMAPFILEHEADER), 
                          &dwBytesRead, NULL) ;

     if (!bSuccess || (dwBytesRead != sizeof (BITMAPFILEHEADER))         
                   || (bmfh.bfType != * (WORD *) "BM"))
     {
          CloseHandle (hFile) ;
          return NULL ;
     }

          // Allocate memory for the packed-DIB & read it in

     dwPackedDibSize = bmfh.bfSize - sizeof (BITMAPFILEHEADER) ;

     pbmi = malloc (dwPackedDibSize) ;

     bSuccess = ReadFile (hFile, pbmi, dwPackedDibSize, &dwBytesRead, NULL) ;
     CloseHandle (hFile) ;

     if (!bSuccess || (dwBytesRead != dwPackedDibSize))
     {
          free (pbmi) ;
          return NULL ;
     }

     return pbmi ;
}

/*----------------------------------------------
   Functions to get information from Packed Dib
  ----------------------------------------------*/

int PackedDibGetWidth (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcWidth ;
     else
          return pPackedDib->bmiHeader.biWidth ;
}

int PackedDibGetHeight (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcHeight ;
     else
          return abs (pPackedDib->bmiHeader.biHeight) ;
}

int PackedDibGetBitCount (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcBitCount ;
     else
          return pPackedDib->bmiHeader.biBitCount ;
}

int PackedDibGetRowLength (BITMAPINFO * pPackedDib)
{
     return ((PackedDibGetWidth (pPackedDib) * 
              PackedDibGetBitCount (pPackedDib) + 31) & ~31) >> 3 ;
}

/*-----------------------------------------------------------
   PackedDibGetInfoHeaderSize includes possible color masks!
  -----------------------------------------------------------*/

int PackedDibGetInfoHeaderSize (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return ((PBITMAPCOREINFO)pPackedDib)->bmciHeader.bcSize ;

     else if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPINFOHEADER))
          return pPackedDib->bmiHeader.biSize + 
                    (pPackedDib->bmiHeader.biCompression == 
                                        BI_BITFIELDS ? 12 : 0) ;

     else return pPackedDib->bmiHeader.biSize ;
}

/*-------------------------------------------------------------
   PackedDibGetColorsUsed returns value in information header;
          could be 0 to indicate non-truncated color table!
  -------------------------------------------------------------*/

int PackedDibGetColorsUsed (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return 0 ;
     else
          return pPackedDib->bmiHeader.biClrUsed ;
}

/*------------------------------------------------------------------
   PackedDibGetNumColors is actual number of entries in color table
  ------------------------------------------------------------------*/

int PackedDibGetNumColors (BITMAPINFO * pPackedDib)
{
     int iNumColors ;

     iNumColors = PackedDibGetColorsUsed (pPackedDib) ;

     if (iNumColors == 0 && PackedDibGetBitCount (pPackedDib) < 16)
          iNumColors = 1 << PackedDibGetBitCount (pPackedDib) ;

     return iNumColors ;
}

int PackedDibGetColorTableSize (BITMAPINFO * pPackedDib)
{
     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return PackedDibGetNumColors (pPackedDib) * sizeof (RGBTRIPLE) ;
     else
          return PackedDibGetNumColors (pPackedDib) * sizeof (RGBQUAD) ;
}

RGBQUAD * PackedDibGetColorTablePtr (BITMAPINFO * pPackedDib)
{
     if (PackedDibGetNumColors (pPackedDib) == 0)
          return 0 ;

     return (RGBQUAD *) (((BYTE *) pPackedDib) + 
                                   PackedDibGetInfoHeaderSize (pPackedDib)) ;
}

RGBQUAD * PackedDibGetColorTableEntry (BITMAPINFO * pPackedDib, int i)
{
     if (PackedDibGetNumColors (pPackedDib) == 0)
          return 0 ;

     if (pPackedDib->bmiHeader.biSize == sizeof (BITMAPCOREHEADER))
          return (RGBQUAD *) 
               (((RGBTRIPLE *) PackedDibGetColorTablePtr (pPackedDib)) + i) ;
     else
          return PackedDibGetColorTablePtr (pPackedDib) + i ;
}

/*------------------------------
   PackedDibGetBitsPtr finally!
  ------------------------------*/

BYTE * PackedDibGetBitsPtr (BITMAPINFO * pPackedDib)
{
     return ((BYTE *) pPackedDib) + PackedDibGetInfoHeaderSize (pPackedDib) +
                                    PackedDibGetColorTableSize (pPackedDib) ;
}

/*----------------------------------------------------------------------- 
   PackedDibGetBitsSize can be calculated from the height and row length
          if it's not explicitly in the biSizeImage field
  -----------------------------------------------------------------------*/

int PackedDibGetBitsSize (BITMAPINFO * pPackedDib)
{
     if ((pPackedDib->bmiHeader.biSize != sizeof (BITMAPCOREHEADER)) &&
         (pPackedDib->bmiHeader.biSizeImage != 0))
         return pPackedDib->bmiHeader.biSizeImage ;

     return PackedDibGetHeight (pPackedDib) * 
            PackedDibGetRowLength (pPackedDib) ;
}

/*----------------------------------------------------------------
   PackedDibCreatePalette creates logical palette from Packed DIB
  ----------------------------------------------------------------*/

HPALETTE PackedDibCreatePalette (BITMAPINFO * pPackedDib)
{
     HPALETTE     hPalette ;
     int          i, iNumColors ;
     LOGPALETTE * plp ;
     RGBQUAD    * prgb ;

     if (0 == (iNumColors = PackedDibGetNumColors (pPackedDib)))
          return NULL ;

     plp = malloc (sizeof (LOGPALETTE) * 
                         (iNumColors - 1) * sizeof (PALETTEENTRY)) ;

     plp->palVersion    = 0x0300 ;
     plp->palNumEntries = iNumColors ;

     for (i = 0 ; i < iNumColors ; i++)
     {
          prgb = PackedDibGetColorTableEntry (pPackedDib, i) ;

          plp->palPalEntry[i].peRed   = prgb->rgbRed ;
          plp->palPalEntry[i].peGreen = prgb->rgbGreen ;
          plp->palPalEntry[i].peBlue  = prgb->rgbBlue ;
          plp->palPalEntry[i].peFlags = 0 ;
     }

     hPalette = CreatePalette (plp) ;
     free (plp) ;

     return hPalette ;
}
