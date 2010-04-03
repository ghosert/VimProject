/* convert.c -- convert between file name notations */


#include "switches.h"
#include "stdio.h"
#include "string.h"
#include "convert.h"

char dir_separator = 
#ifdef MACINTOSH
':';
#endif
#ifdef DOS
'\\';
#endif
#ifdef UNIX
'/';
#endif
#ifdef AMIGA
'/';
#endif
/* Note: syntax error if not one and only one is defined (this is a feature) */


int pauseflag;


/* convert -- convert filename to local naming conventions */
/**/
void convert(char *filename)
{
        int i;
        /* first test for problem characters */
        for (i = strlen(filename); i >= 0; i--) {
                if (filename[i] == ':' || filename[i] == '\\') {
                        fprintf(stderr, "WARNING: %s has one of \":\\\" and may not port.\n",
                                                                                    filename);
                        fprintf(stderr, "         '/' should be used as directory separator.\n");
                        if (PAUSE) getchar();
                }
        }
#ifdef MACINTOSH
        /* prepend a ":" */
        for (i = strlen(filename); i >= 0; i--) {
                filename[i + 1] = filename[i];
        }
        filename[0] = ':';
#endif
#ifndef UNIX
#ifndef AMIGA
    /* replace '/' with local separator */
    for (i = strlen(filename); i >= 0; i--) {
            if (filename[i] == '/') filename[i] = dir_separator;
    }
#endif
#endif
}


