/* sndfaillinux.c -- version of sndfail that prints a message */

/* this should not be compiled into an snd library! handling snd_fail
 * is application specific
 */

#include "stdio.h"

void snd_fail(char *msg)
{
    printf("ERROR: %s\n", msg);
}

