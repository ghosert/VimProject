/*--------------------------------*-C-*---------------------------------*
 * File:
 *	fftn.h
 * ---------------------------------------------------------------------*
 * Re[]:	real value array
 * Im[]:	imaginary value array
 * nTotal:	total number of complex values
 * nPass:	number of elements involved in this pass of transform
 * nSpan:	nspan/nPass = number of bytes to increment pointer
 *		in Re[] and Im[]
 * isign:	exponent: +1 = forward  -1 = reverse
 * scaling:	normalizing constant by which the final result is *divided*
 *	scaling == -1, normalize by total dimension of the transform
 *	scaling <  -1, normalize by the square-root of the total dimension
 *
 * ----------------------------------------------------------------------
 * See the comments in the code for correct usage!
 */

#ifndef _FFTN_H
#define _FFTN_H

extern void fft_free (void);

/* double precision routine */
extern int fftn (int ndim, const int dims[], double Re[], double Im[],
                 int isign, double scaling);

/* float precision routine */
extern int fftnf (int ndim, const int dims[], float Re[], float Im[],
                  int isign, double scaling);
#endif	/* _FFTN_H */
