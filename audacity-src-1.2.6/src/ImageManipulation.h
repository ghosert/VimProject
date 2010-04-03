/**********************************************************************

  Audacity: A Digital Audio Editor

  ImageManipulation.h
  
  Dominic Mazzoni

**********************************************************************/  

#include <wx/defs.h>
#include <wx/colour.h>

class wxImage;

// This looks at the first pixel in the image, and shifts
// the entire image by the vector difference between that 
// pixel and the dstColour.  For better control, use
// ChangeImageColour(wxImage, wxColour*, wxColour*) below
wxImage *ChangeImageColour(wxImage * srcImage, wxColour & dstColour); 

// This function takes a source image, which it assumes to
// be grayscale, and smoothly changes the overall color
// to the specified color, and returns the result as a
// new image.  This works well for grayscale 3D images.
// Audacity uses this routines to make the buttons
// (skip-start, play, stop, record, skip-end) adapt to
// the color scheme of the user.
wxImage *ChangeImageColour(wxImage * srcImage,
                           wxColour & srcColour,
                           wxColour & dstColour);
     
// Takes a background image, foreground image, and mask
// (i.e. the alpha channel for the foreground), and
// returns an new image where the foreground has been
// overlaid onto the background using alpha-blending,
// at location (xoff, yoff).
wxImage *OverlayImage(wxImage * background, wxImage * foreground,
                      wxImage * mask, int xoff, int yoff);

// Creates an image with a solid background color
wxImage *CreateBackground(int width, int height, wxColour colour);

// Creates an image with the Mac OS X Aqua stripes, to be used
// as a background
wxImage *CreateAquaBackground(int width, int height, int offset);

// Uses color on all OS except Mac, uses Aqua
wxImage *CreateSysBackground(int width, int height, int offset,
                             wxColour colour);
