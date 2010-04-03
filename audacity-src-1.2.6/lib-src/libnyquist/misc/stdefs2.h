/*
 * FILE: stdefs.h
 *   BY: Christopher Lee Fraley (cf0v@spice.cs.cmu.edu)
 * DESC: Defines standard stuff for inclusion in C programs.
 *
 * 1.0 ( 6-JUN-88) - created. (cf0v)
 * 1.1 (30-JUN-88) - moved GetUHWORD def here. (cf0v)
 * 1.2 (11-JUL-88) - added ffail() defs. (cf0v)
 * 1.3 (25-JUL-88) - added dB() and undB() defs. (cf0v)
 * 1.4 ( 8-AUG-88) - added stuff used by apple & new NULL def. (cf0v)
 */

/* General purpose constants: */
/* #define TRUE  1
   #define FALSE 0
 */

#define FATAL 1
#define WARN  0

#define PI (3.14159265358979232846)
#define PI2 (6.28318530717958465692)
#define D2R (0.01745329348)          /* (2*pi)/360 */
#define R2D (57.29577951)            /* 360/(2*pi) */


/* General purpose typedefs: */
typedef int		bool;
typedef char		BOOL;
typedef short		HWORD;
typedef unsigned short	UHWORD;
typedef int		WORD;
typedef unsigned int	UWORD;


/* General purpose macros: */
#define GetUHWORD(x,y,z) GetUShort(x,y,z)
#define ffail(x,y)	 fail(sprintf(ERROR,x,y))
#define ffail2(x,y,z)	 fail(sprintf(ERROR,x,y,z))
#define MAX(x,y) ((x)>(y) ?(x):(y))
#define MIN(x,y) ((x)<(y) ?(x):(y))
#define ABS(x)   ((x)<0   ?(-(x)):(x))
#define SGN(x)   ((x)<0   ?(-1):((x)==0?(0):(1)))
#define round(x) ((int)((x) + 0.5))

/* Convert amplitudes to and from dB scale: */
#define MINLIN  undB(-100)
#define undB(x) (((double)(REF)) * pow(10.0, ((double)(x))/20.0))
#define dB(x)   (((x)<MINLIN) ? (20.0 * log10(MINLIN/((double)(REF))))\
                              : (20.0 * log10((x)/((double)(REF)))))
