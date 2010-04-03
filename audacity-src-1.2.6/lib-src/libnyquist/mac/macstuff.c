/* macstuff.c - macintosh interface routines for xlisp */
/* Written by Brian Kendig. */
/* This file contains the stuff that the other xlisp files call directly. */

#include "cext.h"
#include <stdio.h>
#include <stdarg.h>
#include <QuickDraw.h>	/* for Random */
#include <Memory.h>		/* for DisposePtr */
#include <SegLoad.h>	/* for ExitToShell */
#include "xlisp.h"
#include <string.h>
#include "macint.h"
#define DELETE 0x08

/* externals */
extern FILE *tfp;  /* transcript file pointer */
extern int cursorPos;
extern char *macgets (void);

#define GPRINTF_MESSAGE_LEN 500

/* nyquist_printf -- system independent version of printf */
/*
 * this function prints to console like printf, but using GUI
 * rather than stdio when appropriate.
 *
 */
void nyquist_printf(char *format, ...)
{
    char temp[GPRINTF_MESSAGE_LEN];
    va_list pvar;
    char *p = temp;
    va_start(pvar, format);
    vsnprintf(temp, GPRINTF_MESSAGE_LEN, format, pvar);
    va_end(pvar);
    while (*p) ostputc(*p++);
}



/* this should really be in a header for MacFileUtils.c */
void GetFullPath(FSSpec *theSpec, StringPtr theName);





/* this is called when we load a file -- if need_preference_file, 
 * we will build a preference file and insert the path of the file
 * we just opened, assuming it will tell us where to find init.lsp
 */
void setup_preferences(char *filename)
{
}
