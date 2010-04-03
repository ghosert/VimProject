/* macfun.c - macintosh user interface functions for xlisp */

#include <Quickdraw.h>
#include <Windows.h>
#include <Memory.h>

#include <string.h>

#include "xlisp.h"

void CtoPstr(char *x)
{
   int len = strlen(x);
   int i;
   
   for(i=len; i>0; i--)
      x[i] = x[i-1];
   x[0] = len;
}

void FlushOutput()
{
}

unsigned long startupTicks = 0;

unsigned long ticks_per_second (void)	{ return 60; }
unsigned long run_tick_count (void)		{ return ((unsigned long) TickCount ()) - startupTicks; }
unsigned long real_tick_count (void)	{ return (unsigned long) TickCount (); }

LVAL xrealtime (void)	{ return cvfixnum ((FIXTYPE)real_tick_count()); }	/* get-internal-real-time */
LVAL xruntime (void)	{ return cvfixnum ((FIXTYPE)run_tick_count()); }	/* get-internal-run-time */
LVAL xtime (void)		{ return cvfixnum ((FIXTYPE)real_tick_count()); }	/* time */

/* get an integer parameter */
LOCAL int getNumber () {
    LVAL num = xlgafixnum ();
    return ((int) getfixnum (num));
}

/* handle commands that require integer arguments */
LOCAL LVAL GrafCmd (char funct, int nArgs) {
    return NIL;
}

LVAL xshowgraphics (void)	{ return GrafCmd ('G', 0); }  /* show graphics win */
LVAL xhidegraphics (void)	{ return GrafCmd ('g', 0); }  /* hide graphics win */
LVAL xcleargraphics (void)	{ return GrafCmd ('x', 0); }  /* clear graphics win */
LVAL xshowpen (void)	{ return GrafCmd ('s', 0); }  /* show the pen */
LVAL xhidepen (void)	{ return GrafCmd ('h', 0); }  /* hide the pen */
LVAL xpenmode (void)	{ return GrafCmd ('d', 1); }  /* set the pen mode */
LVAL xmove (void)		{ return GrafCmd ('M', 2); }  /* move pen in a specified direction */
LVAL xmoveto (void)		{ return GrafCmd ('m', 2); }  /* move pen to a screen location */
LVAL xdraw (void)		{ return GrafCmd ('L', 2); }  /* draw a line in a specified direction */
LVAL xdrawto (void)		{ return GrafCmd ('l', 2); }  /* draw a line to a screen location */
LVAL xpensize (void)	{ return GrafCmd ('S', 2); }  /* set the pen size */
LVAL xpennormal (void)	{ return GrafCmd ('p', 0); }  /* set the pen to normal */
LVAL xcolor (void)		{ return GrafCmd ('c', 3); }  /* set RGB color of pen */


LVAL xgetpen (void) {  /* get the pen position */
    return NIL;
}

LVAL xpenpat (void) {  /* set the pen pattern */
    return NIL;
}

/* The functions below are not yet implemented. */

LVAL xtool (void) {  /* call the toolbox */
    int trap = getNumber ();
    LVAL val;

    return cvfixnum ((FIXTYPE) trap);
}

LVAL xtool16 (void) {  /* call the toolbox with a 16 bit result */
    int trap = getNumber ();
    int val;

    return cvfixnum ((FIXTYPE) trap);
}

LVAL xtool32 (void) {  /* call the toolbox with a 32 bit result */
    int trap = getNumber ();
    long val;

    return cvfixnum ((FIXTYPE) trap);
}

LVAL xnewhandle (void) {  /* allocate a new handle */
    LVAL num = xlgafixnum ();
    long size = getfixnum (num);
    xllastarg ();
    return cvfixnum ((FIXTYPE) NewHandle (size));
}

LVAL xnewptr (void) {  /* allocate memory */
    LVAL num = xlgafixnum ();
    long size = getfixnum (num);
    xllastarg ();
    return cvfixnum ((FIXTYPE) NewPtr (size));
}

LVAL xhiword (void) {  /* return the high order 16 bits of an integer */
    unsigned int val = (unsigned int) (getNumber () >> 16);
    xllastarg ();
    return cvfixnum ((FIXTYPE) val);
}

LVAL xloword (void) {  /* return the low order 16 bits of an integer */
    unsigned int val = (unsigned int) getNumber ();
    xllastarg ();
    return cvfixnum ((FIXTYPE) val);
}

LVAL xrdnohang (void) {  /* get the next character in the look-ahead buffer */
    int ch = 0;
    xllastarg ();
/*    if ((ch = scrnextc ()) == EOF) return NIL; */
    return cvfixnum ((FIXTYPE) ch);
}


void xoserror (char *msg) { /* do nothing */ }

