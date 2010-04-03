/* term.c -- Routines for managing terminal I/O settings by Alan Cox.
 * From LJ 17 */

/* Thanks to Dave Cook for rescuing it */

#ifdef WIN32

void term_restore(void)
{
}

void term_exit()
{
}

void term_ctrlz()
{
}

void term_cont()
{
}

void term_init(void)
{
}

void term_character(void)
{
}

void term_line(void)
{
}

int term_testchar()
{
}

#else

#include <termios.h>
#ifndef __APPLE__
#include <asm/ioctls.h>
#endif
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

/* This will be used for new terminal settings. */
static struct termios current;

/* This will hold the initial state so that we can restor it later. */
static struct termios initial;

/* Restor the termianl settings to those saved when term_init was called. */
void term_restore(void)
{
}

void term_exit()
{
}

void term_ctrlz()
{
}

void term_cont()
{
}

void term_init(void)
{
}

void term_character(void)
{
}

void term_line(void)
{
}

int term_testchar()
{
   return 0;
}

#endif

