/*
 * FILE: plot.c
 *   BY: Christopher Lee Fraley (cf0v@spice.cs.cmu.edu)
 * DESC: graphs file of numbers on terminal
 *
 * 1.1 ( 1-JUN-88) - added lines from 0 to data points (cf0v)
 * 1.2 ( 3-JUN-88) - added skip parameter (cf0v)
 * 1.3 (23-JUN-88) - added -x and -y options (cf0v)
 * 1.4 (30-JUN-88) - clean-up. (cf0v)
 * 2.0 ( 5-JUL-88) - added binary sound file format option.  Changed scaling
 *			for "-n" option to only consider those values being
 *			plotted, instead of whole file.
 */

char plotVERSION[] = "2.0  (5-JUL-88, 11:40am)";

/*
 * plot [<file> [-nxyab]]
 *    Accepts input stream of numbers from <file> (or stdin, if no <file>
 * argument is present), drawing a graph to sdout.  File is prescanned for
 * min and max values, and the graph is scaled accordingly.  If making the
 * file's min non-zero delta equal to one char allows the entire graph to
 * fit on the screen, then this scalar is used.  If the -n<num> option is
 * used, then only every <num>th number from the input stream is plotted.
 * The -x option enables printing the line number in the file on the screen
 * every 10 lines.  The -y option enables printing the y value on the screen
 * every line.  Note -x and -y are NOT mutually exclusive.  The -a option
 * indicates the input file is in ascii format, while the -b option indicates
 * the input file is in the binary sound file format.  When neither of these
 * is present, binary is assumed unless input is from stdin.
 */

#include <stdio.h>
#include "stdefs.h"

#define MAXNUMINPUT 16*1024
#define BINARY 0
#define ASCII 1


fail(s, arg)
char *s, *arg;
{
   fprintf(stderr,"\nplot: ");
   fprintf(stderr, s, arg);
   fprintf(stderr,"\n\n");
   exit(1);
}



FILE *fain;    /* Pointer to input file for ascii format */
int fbin;      /* Input file for binary format */
int ftype;     /* Type of input file */


prescan(X, Num, Max, Min, Delta)
HWORD X[];
int *Num, *Max, *Min, *Delta;
{
   int last, i, len;

   *Min = *Delta = 32767;
   *Max = -32768;
   *Num = last = 0;
   if (ftype == ASCII)
      while (fscanf(fain,"%d",&i) != EOF)
         {
         *Min = MIN(i, *Min);
         *Max = MAX(i, *Max);
         if (i != last)
            *Delta = MIN(ABS(i-last), *Delta);
         *Num += 1;
         last = i;
         *X++ = i;
         }
   else
      {
      printf("| binary prescan, fbin:%d\n", fbin);
      len = read(fbin, (char *)X, MAXNUMINPUT*sizeof(HWORD)) / sizeof(HWORD);
      if (len >= MAXNUMINPUT)
         fprintf(stderr, "plot: input truncated to %d samples\n",
            MAXNUMINPUT);
      for (i=0; i<len; i++)
         {
         *Min = MIN(*X, *Min);
         *Max = MAX(*X, *Max);
         if (*X != last)
            *Delta = MIN(ABS(*X-last), *Delta);
         last = *X++;
         }
      *Num = len;
      }
}



getargs(argc, argv, skip, xPrint, yPrint, start)
int argc, *skip, *yPrint, *xPrint, *start;
char *argv[];
{
   char *file;

   file = NULL;
   fain = stdin;
   ftype = BINARY;
   *start = 0;
   *skip = 1;
   *xPrint = FALSE;
   *yPrint = FALSE;
   while (--argc)
      {
      ++argv;
      if ((*argv)[0] != '-')
         {
         if (file || (*argv)[0]=='?')
            fail("syntax error argument '%s'.\
               \n   format: 'plot [<file>] [-n/s<num>] [-x/y/a/b]',\
               \n   where: <file> to plot (stdin is default),\
               \n          -n<num> plot every <num>th value in file,\
               \n          -s<num> start at the <num>th sample,\
               \n          -x print line # in file every 10 lines,\
               \n          -y print y coordinate every line,\
               \n          -a indicates input file is in ascii format,\
               \n          -b indicates input is in binary sound file format.\
               \n    -x and -y are NOT mutually exclusive; -a and -b ARE.\
               \n    -b is assumed unless input is stdin.", *argv);
         file = *argv;
         printf("| file:%s\n", file);
         }
      else if ((*argv)[1]=='x' || (*argv)[1]=='X')
         *xPrint = TRUE;
      else if ((*argv)[1]=='y' || (*argv)[1]=='Y')
         *yPrint = TRUE;
      else if ((*argv)[1]=='a' || (*argv)[1]=='A')
         ftype = ASCII;
      else if ((*argv)[1]=='b' || (*argv)[1]=='B')
         ftype = BINARY;
      else if ((*argv)[1]=='n' || (*argv)[1]=='N')
         {
         if (1 != sscanf(*argv+2, "%d", skip))
            fail("illegal -n parameter '%s'", *argv);
         }
      else if ((*argv)[1]=='s' || (*argv)[1]=='S')
         {
         if (1 != sscanf(*argv+2, "%d", start))
            fail("illegal -s parameter '%s'", *argv);
         }
      }
   if (!file)
      ftype = ASCII;
   else if (ftype == ASCII)
      {
      if (NULL == (fain = fopen(file, "r")))
         fail("could not open ascii input file '%s'\n", file);
      }
   else
      {
      if (NULL > (fbin = open(file, 0)))
         fail("could not open binary input file '%s'\n", file);
      }
}



main (argc,argv)
int argc;
char *argv[];
{
   int Num, Max, Min, Delta, i;
   HWORD X[MAXNUMINPUT];
   double factor;
   int target, skip, start;
   int xCount, xPrint, yPrint;
   char *Star[41], *Space[41];

   printf("\nData Plotting Program\n");
   printf("by: Christopher Lee Fraley\n");
   printf("Version: %s\n", plotVERSION);

   Star[40] = "****************************************";
   Space[40] = "                                        ";
   for (i=39; i>=0; i--)
      {
      Star[i] = Star[i+1] + 1;
      Space[i] = Space[i+1] + 1;
      }

   getargs(argc, argv, &skip, &xPrint, &yPrint, &start);
   prescan(X, &Num, &Max, &Min, &Delta);

   factor = 1.0/Delta;
   if (factor*Max>39)
      factor = 39.0/Max;
   if (factor*Min<-40)
      factor = 40.0/-Min;

   printf("\n     Number of Data Points:%d  \t  [%d:%d]\n",Num,Min,Max);
   printf("     Scale: %g/char",1.0/factor);
   if (skip > 1)
      printf("  \t\t  Plotting every %dth sample", skip);
   if (start)
      printf("     Starting plot at sample %d", start);
   printf("\n\n=========3=========2=========1=========0=========1=========2\
=========3=========\n");

   if (xPrint)
      xCount = 11;
   else
      xCount = -1;
   for (i=start; i<Num; i+=skip)
      {
      target = X[i]*factor+40;
      --xCount;
      if (target < 40)
         {
         printf(Space[target-1]);
         printf(Star[41-target]);
         if (!xCount)
            printf("\t\t     %6d",i);
         else
            printf("\t\t\t   ");
         if (yPrint)
            printf("  %6d\n",X[i]);
         else
            printf("\n");
         }
      else
         {
         if (yPrint)
            printf(" %6d  ",X[i]);
         else
            printf("\t ");
         if (!xCount)
            printf("%6d                        ",i);
         else
            printf("                              ");
         printf(Star[target-39]);
         printf("\n");
         }
      if (!xCount)
         xCount = 10;
      }

   if (ftype == BINARY)
      (void) close(fbin);
   else if (fain != stdin)
      (void) fclose(fain);
}
