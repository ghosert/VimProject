/* winstuff.c - windows interface routines for xlisp */
/* Written by Chris Tchou. */
/* This file contains the stuff that the other xlisp files call directly. */
#include <windows.h>		/*Added by Ning Hu	Apr.2001 */
#include <stdio.h>
//#include <QuickDraw.h>	/* for Random */
#include <memory.h>		/* for DisposPtr */
#include <string.h>
//#include <SegLoad.h>	/* for ExitToShell */
#include "xlisp.h"
#include "userio.h"

#define DELETE 0x08

/* externals */
extern FILE *tfp;  /* transcript file pointer */
extern int cursorPos;
extern char *macgets (void);
//Added by Ning Hu	Apr.2001
extern int _isatty(int);					
extern int redirect_flag;
//Add end

/* local variables */
int lposition;
static char *linebuf = NULL, *lineptr;
static int numChars;


//int isascii (char c) { return 1; }  /* every char is an ascii char, isn't it? */

void osinit (char *banner) {
//	int i;

    char version[] = "\nWindows console interface by Chris Tchou and Morgan Green.\n";
//	InitMac ();  /* initialize the mac interface routines */
//	lposition = 0;  /* initialize the line editor */
//	for (i = 0; banner[i] != '\0'; i++) macputc (banner[i]);
//	for (i = 0; version[i] != '\0'; i++) macputc (version[i]);
    printf(banner);
    printf(version);

    // Added by Ning Hu, to communicate between compiler GUI and Nyquist console	Apr.2001
    if( _isatty( _fileno( stdout ) ) ){
        redirect_flag = 0;
#ifdef DEBUG
      printf( "stdout has not been redirected to a file\n" );		//for debugging use
#endif
    }
    else {
        redirect_flag = 1;
#ifdef DEBUG
      printf( "stdout has been redirected to a file\n");			//for debugging use
#endif
    }
    // Add End
}

/* osrand - return next random number in sequence */
long osrand (long rseed) {
#ifdef OLDBUTINTERESTING
// note that this takes a seed and returns a big number,
// whereas I think XLisp's RANDOM is defined differently
    long k1;

    /* make sure we don't get stuck at zero */
    if (rseed == 0L) rseed = 1L;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
    rseed += 2147483647L;

    /* return a random number between 0 and MAXFIX */
    return rseed;
#endif
    return rand() % rseed;	// rseed is a misnomer
}

FILE *osaopen (char *name, char *mode) {
    return fopen (name, mode);
}

FILE *osbopen (char *name, char *mode) {
    char nmode[4];
    strcpy (nmode, mode); strcat (nmode, "b");
    return (fopen (name, nmode));
}

int osclose (FILE *fp) { return (fclose (fp)); }
int osaputc (int ch, FILE *fp) { return (putc (ch, fp)); }
int osbputc (int ch, FILE *fp) { return (putc (ch, fp)); }

/* osagetc - get a character from an ascii file */
int osagetc(fp)
  FILE *fp;
{
    return (getc(fp));
}

extern int abort_flag;
extern int redirect_flag;			//Added by Ning Hu	Apr.2001

int ostgetc (void) {
/*	int i;

    if (numChars <= 0) {  /* get some more */
/*		if (linebuf) DisposPtr (linebuf);
        linebuf = macgets ();
        i = 0;
        while (linebuf[i] != '\0') i++;
        numChars = i;
        if (tfp) for (i = 0; i < numChars; i++) osaputc (linebuf[i], tfp);
        lineptr = linebuf;
    }
    numChars--;
    if (*lineptr == '\r') {
        lineptr++;
        return '\n';
    } else return (*lineptr++);*/

//  return getchar();			// console		//Old code--removed by Ning Hu
    // Added by Ning Hu		Apr.2001
    int ch = getchar();
    oscheck(); /* in case user typed ^C */
    if (ch == BREAK_CHAR && abort_flag == BREAK_LEVEL) {
        abort_flag = 0;
    }
    switch (ch) {
          case ABORT_CHAR:	/* control-c */
            xltoplevel();
            break;
          case '\025':	/* control-u */
            xlcleanup();
          case '\020':	/* control-p */
            xlcontinue();
            break;
          case BREAK_CHAR:
            ostputc('\n');	/* control-b */
            xlbreak("BREAK",s_unbound);
            break;
          default:
            break;
    }
    return ch;
    //Add End
}

void ostputc (int ch) {
//	macputc (ch);
    putchar(ch);			// console

    if (tfp) osaputc (ch, tfp);
}


void osflush (void) {
    lineptr = linebuf;
    numChars = 0;
    lposition = 0;
}


void oscheck (void) {				
    MSG lpMsg;
/*DoEvent ();*/		
    check_aborted();	
//	printf("Current Thread: %d\n", GetCurrentThreadId());		//for debugging use
    if ((redirect_flag) && (PeekMessage(&lpMsg, NULL, 0, 0, PM_REMOVE)!=0)) { 
        if (lpMsg.message == WM_CHAR) {
            switch (lpMsg.wParam) {
                case ABORT_CHAR: abort_flag = ABORT_LEVEL;
                                break;
                case BREAK_CHAR: abort_flag = BREAK_LEVEL;
                                break;
            }
        }
      //printf("Get message: %d %d \n", lpMsg.wParam, BREAK_CHAR);				//for debugging use
    }	
    if (abort_flag == ABORT_LEVEL) {	
        abort_flag = 0;
        osflush();
        xltoplevel();
    } else if (abort_flag == BREAK_LEVEL) {
        abort_flag = 0;
        osflush();
        xlbreak("BREAK",s_unbound);
    }
}
//Update end

void oserror (char *msg) {
    char line[100], *p;
    sprintf (line,"error: %s\n",msg);
    for (p = line; *p != '\0'; ++p) ostputc (*p);
}

void osfinish (void) {
    /* dispose of everything... */
//	if (linebuf) DisposPtr (linebuf);
//	MacWrapUp ();
//	ExitToShell ();
}

int renamebackup (char *filename) { return 0; }


long randomseed = 1L;

long random () {
// note that this takes a seed and returns a big number,
// whereas I think XLisp's RANDOM is defined differently
    long k1;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = randomseed / 127773L;
    if ((randomseed = 16807L * (randomseed - k1 * 127773L) - k1 * 2836L) < 0L)
      randomseed += 2147483647L;

    /* return a random number between 0 and MAXFIX */
    return randomseed;
}

/* Added by Ning Hu		May.2001 
xsetdir - set current directory of the process */
LVAL xsetdir() {
    TCHAR ssCurDir[MAX_PATH], szCurDir[MAX_PATH];

    strcpy(ssCurDir, getstring(xlgastring()));
    xllastarg();
    if (SetCurrentDirectory(ssCurDir)) {
        if (GetCurrentDirectory(
            sizeof(szCurDir)/sizeof(TCHAR), szCurDir)) {	
        /* create the result string */
            stdputstr("Current Directory:");
            stdputstr(szCurDir);
            stdputstr("\n");
        }	
        else stdputstr("Directory Setting Error\n");
    }
    else stdputstr("Directory Setting Error\n");

    /* return the new string */
    return (NIL);
}
//Updated End

