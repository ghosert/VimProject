/**********************************************************************

  Audacity: A Digital Audio Editor

  ImageManipulation.cpp
  
  Dominic Mazzoni

**********************************************************************/  

#include <wx/image.h>

#include "ImageManipulation.h"

// This looks at the first pixel in the image, and shifts
// the entire image by the vector difference between that 
// pixel and the dstColour.  For better control, use
// ChangeImageColour(wxImage, wxColour*, wxColour*) below
wxImage *ChangeImageColour(wxImage * srcImage, wxColour & dstColour) 
{
   unsigned char *src = srcImage->GetData();
   wxColour c;
   c.Set(src[0], src[1], src[2]);
   return ChangeImageColour(srcImage, c, dstColour);
}

//This will explicitly shift the image color from
//srcColour to dstColour. 
wxImage *ChangeImageColour(wxImage * srcImage,
                           wxColour & srcColour,
                           wxColour & dstColour) 
{
   // This function takes a source image, which it assumes to
   // be grayscale, and smoothly changes the overall color
   // to the specified color, and returns the result as a
   // new image.  This works well for grayscale 3D images.
   // Audacity uses this routines to make the buttons
   // (skip-start, play, stop, record, skip-end) adapt to
   // the color scheme of the user.

   unsigned char *src = srcImage->GetData();
   int width = srcImage->GetWidth();
   int height = srcImage->GetHeight();

   wxImage * dstImage = new wxImage(width, height);
   unsigned char *dst = dstImage->GetData();


   //Get the source color
   int srcVal[3], srcOpp[3];
   srcVal[0] = srcColour.Red();
   srcVal[1] = srcColour.Green();
   srcVal[2] = srcColour.Blue();

   int dstVal[3], dstOpp[3];
   dstVal[0] = dstColour.Red();
   dstVal[1] = dstColour.Green();
   dstVal[2] = dstColour.Blue();

   int i;
   for (i = 0; i < 3; i++) {
      srcOpp[i] = 255 - srcVal[i];
      dstOpp[i] = 255 - dstVal[i];

   }

   int c = 0;
   for (i = 0; i < width * height * 3; i++) {
      int s = (int) *src;

      if (s > srcVal[c])
         *dst++ = dstVal[c] + dstOpp[c] * (s - srcVal[c]) / srcOpp[c];

      else
         *dst++ = dstVal[c] * s / srcVal[c];
      src++;
      c = (c + 1) % 3;
   }

   return dstImage;
}


wxImage *OverlayImage(wxImage * background, wxImage * foreground,
                      wxImage * mask, int xoff, int yoff) 
{
   // Takes a background image, foreground image, and mask
   // (i.e. the alpha channel for the foreground), and
   // returns an new image where the foreground has been
   // overlaid onto the background using alpha-blending,
   // at location (xoff, yoff).
   unsigned char *bg = background->GetData();
   unsigned char *fg = foreground->GetData();
   unsigned char *mk = mask->GetData();

   int bgWidth = background->GetWidth();
   int bgHeight = background->GetHeight();
   int fgWidth = foreground->GetWidth();
   int fgHeight = foreground->GetHeight();
   int mkWidth = mask->GetWidth();
   int mkHeight = mask->GetHeight();


   //Now, determine the dimensions of the images to be masked together
   //on top of the background.  This should be equal to the area of the
   //smaller of the foreground and the mask, as long as it is 
   //within the area of the background, given the offset.

   //Make sure the foreground size is no bigger than the mask
   int wCutoff = (fgWidth < mkWidth) ? fgWidth : mkWidth;
   int hCutoff = (fgHeight < mkHeight) ? fgHeight : mkHeight;


   // If the masked foreground + offset is bigger than the background, masking
   // should only occur within these bounds of the foreground image
   wCutoff = (bgWidth - xoff > wCutoff) ? wCutoff : bgWidth - xoff;
   hCutoff = (bgHeight - yoff > hCutoff) ? hCutoff : bgHeight - yoff;


   //Make a new image the size of the background
   wxImage * dstImage = new wxImage(bgWidth, bgHeight);
   unsigned char *dst = dstImage->GetData();
   memcpy(dst, bg, bgWidth * bgHeight * 3);


   // Go through the foreground image bit by bit and mask it on to the
   // background, at an offset of xoff,yoff.
   // BUT...Don't go beyond the size of the background image,
   // the foreground image, or the mask 
   int x, y;
   for (y = 0; y < hCutoff; y++) {

      unsigned char *bkp = bg + 3 * ((y + yoff) * bgWidth + xoff);
      unsigned char *dstp = dst + 3 * ((y + yoff) * bgWidth + xoff);

      for (x = 0; x < wCutoff; x++) {

         int value = mk[3 * (y * mkWidth + x)];
         int opp = 255 - value;

         for (int c = 0; c < 3; c++)
            dstp[x * 3 + c] = 
               ((bkp[x * 3 + c] * opp) + 
                (fg[3 * (y * fgWidth + x) + c] * value)) / 255;
      }
   } 
   return dstImage;
}

// Creates an image with a solid background color
wxImage *CreateBackground(int width, int height, wxColour colour)
{
   wxImage *i = new wxImage(width, height);
   unsigned char *ip;
   int srcVal[3];
   int x;

   srcVal[0] = colour.Red();
   srcVal[1] = colour.Green();
   srcVal[2] = colour.Blue();

   ip = i->GetData();
   for(x=0; x<width*height; x++) {
      *ip++ = srcVal[0];
      *ip++ = srcVal[1];
      *ip++ = srcVal[2];
   }

   return i;
}

// Creates an image with the Mac OS X Aqua stripes, to be used
// as a background
wxImage *CreateAquaBackground(int width, int height, int offset)
{
   wxImage *image = new wxImage(width, height);
   unsigned char *ip = image->GetData();
   unsigned char val[4] = {231, 239, 255, 239};
   unsigned char v;
   int x, y;

   for(y=0; y<height; y++) {
      v = val[(y+offset)%4];
      for(x=0; x<width*3; x++)
         *ip++ = v;
   }

   return image;
}

wxImage *CreateSysBackground(int width, int height, int offset,
                             wxColour colour)
{
   #ifdef __WXMAC__
   return CreateAquaBackground(width, height, offset);
   #else
   return CreateBackground(width, height, colour);
   #endif
}


