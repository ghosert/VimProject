/*
 * FILE: sine.c
 *   BY: Christopher Lee Fraley (cf0v@spice.cs.cmu.edu)
 * DESC: Creates sine waves of given period, amplitude, and length.
 *
 * 1.0 ( 1-JUN-88) - created. (cf0v)
 * 2.0 ( 5-JUL-88) - converted to binary sound file format. (cf0v)
 */

/*
 * sine <amplitude> <period> <length> [-outfile]
 *    Outputs integer sine wave of maximum <amplitude>, with <period>
 * samples per period, and <length> number of periods to stdout. Illegal
 * arguments cause the defaults 32767, 20, and 3 to be used, respectively.
 * If the flag "-outfile" is present, the output is in binary format to the
 * named file.  If the flag is ommitted, the output is to stdout, and is in
 * ascii format, one number per line.
 */

#include <stdio.h>
#include <math.h>
#include "stdefs.h"

#define PERMS 0644      /* -rw-r--r-- */

fails(s, s2)
char *s, *s2;
{
   fprintf(stderr, s, s2);
   exit(1);
}


main (argc,argv)
int argc;
char *argv[];
{
   double Amp, Samps, Perds;
   int i;

   if (argc!=4  &&  argc!=5)
      fails("format is 'sine %s'\n",
         "<amplitude> <samples-per-period> <#-of-periods> [-outfile]");
   if (!sscanf(*++argv, "%lf", &Amp))
      Amp = 32767.0;
   if (!sscanf(*++argv, "%lf", &Samps))
      Samps = 20.0;
   if (!sscanf(*++argv, "%lf", &Perds))
      Perds = 3.0;
   if (argc==5)
      {
      int fout;
      HWORD *Data, *ptr;
      unsigned int outLen, len;

      if (NULL > (fout = creat(*++argv+1, PERMS)))
        fails("could not create <outfile> '%s'\n", *argv+1);
      outLen = Samps*Perds*sizeof(HWORD);
      ptr = Data = (HWORD *)malloc(outLen);
      if (NULL == Data)
         fails("could not allocate enough memory for output", "");
      for (i=0; i<(Samps*Perds); i++)
         *ptr++ = (HWORD) (sin(2*PI*i/Samps) * Amp);
      len = write(fout, (char *)Data, outLen);
      if (len != outLen)
         fails("incorrect number of bytes written", "");
      }
   else
     {
     for (i=0; i<(Samps*Perds); i++)
        printf("%d\n", (int)(sin(2*PI*i/Samps)*Amp));
     }
}
