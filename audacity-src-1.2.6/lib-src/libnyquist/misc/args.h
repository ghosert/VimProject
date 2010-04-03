/*
 * Argument processing macros
 * I got tired of rethinking this everytime,
 * and 4xbsd doesn't have getopt()
 *
 * The following is an example of the use of this stuff
 *

#include "args.h"
#include <stdio.h>

main(argc, argv)
char **argv;
{
    char *a = "a", *b = "b", *c = "c";
    int x = 0, y = 0, z = 0;
    ARGLOOP
        STRINGARG(a)	a = p;		ENDSTRINGARG
        STRINGARG(b)	b = p;		ENDSTRINGARG
        STRINGARG(c)	c = p;		ENDSTRINGARG
        FLAGARG(x)	x++;		ENDFLAGARG
        FLAGARG(y)	y++;		ENDFLAGARG
        FLAGARG(z)	z++;		ENDFLAGARG

        BADARG
            fprintf(stderr, "unknown option %c\n", *p);
            fprintf(stderr, "Usage: usage\n");
            exit(1);
        ENDBADARG
    ENDARGLOOP

    Here, the remaining args are argv[0] to argv[argc - 1] 

    printf("a=%s b=%s c=%s x=%d y=%d z=%d\nargs:", a, b, c, x, y, z);
    { int i; for(i = 0; i < argc; i++)
        printf(" %s", argv[i]);
    }
    printf("\n");
}

 *
 */


#define	ARGLOOP						\
    while(--argc > 0 && **++argv == '-') {		\
        register char *p;				\
        for(p = *argv + 1; *p != '\0'; p++) {	\
        switch(*p) {				\

#define ENDARGLOOP					\
        }					\
        }						\
nextarg:;						\
    }						\

#define FLAGARG(c)	case c:
#define ENDFLAGARG	break;

#define STRINGARG(c)	case c: if(!*++p) p = *++argv, argc--;
#define ENDSTRINGARG	goto nextarg;

#define	BADARG		default:
#define ENDBADARG	break;

