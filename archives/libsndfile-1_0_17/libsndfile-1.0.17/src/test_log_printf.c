/*
** Copyright (C) 2003-2005 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>

#include "sfconfig.h"

/* 
** This is a bit rough, but it is the nicest way to do it.
*/
#define PSF_LOG_PRINTF_ONLY
#include "common.c"


#define	CMP_0_ARGS(line,err,fmt)	\
	{	psf->logindex = 0 ;			\
		LSF_SNPRINTF (buffer, sizeof (buffer), (fmt)) ;	\
		psf_log_printf (psf, (fmt)) ;				\
		err += compare_strings_or_die (line, fmt, buffer, psf->logbuffer) ;	\
		}

#define	CMP_2_ARGS(line,err,fmt,a)	\
	{	psf->logindex = 0 ;			\
		LSF_SNPRINTF (buffer, sizeof (buffer), (fmt), (a), (a)) ;	\
		psf_log_printf (psf, (fmt), (a), (a)) ;					\
		err += compare_strings_or_die (line, fmt, buffer, psf->logbuffer) ;	\
		}

#define	CMP_4_ARGS(line,err,fmt,a)	\
	{	psf->logindex = 0 ;			\
		LSF_SNPRINTF (buffer, sizeof (buffer), (fmt), (a), (a), (a), (a)) ;	\
		psf_log_printf (psf, (fmt), (a), (a), (a), (a)) ;				\
		err += compare_strings_or_die (line, fmt, buffer, psf->logbuffer) ;	\
		}

#define	CMP_5_ARGS(line,err,fmt,a)	\
	{	psf->logindex = 0 ;			\
		LSF_SNPRINTF (buffer, sizeof (buffer), (fmt), (a), (a), (a), (a), (a)) ;	\
		psf_log_printf (psf, (fmt), (a), (a), (a), (a), (a)) ;					\
		err += compare_strings_or_die (line, fmt, buffer, psf->logbuffer) ;		\
		}

#define	CMP_6_ARGS(line,err,fmt,a)	\
	{	psf->logindex = 0 ;			\
		LSF_SNPRINTF (buffer, sizeof (buffer), (fmt), (a), (a), (a), (a), (a), (a)) ;	\
		psf_log_printf (psf, (fmt), (a), (a), (a), (a), (a), (a)) ;					\
		err += compare_strings_or_die (line, fmt, buffer, psf->logbuffer) ;			\
		}

static int compare_strings_or_die (int linenum, const char *fmt, const char* s1, const char* s3) ;

int
main (void)
{	static char buffer [2048] ;
	SF_PRIVATE	sf_private, *psf ;
	int			k, errors = 0 ;
	int			int_values [] = { 0, 1, 12, 123, 1234, 123456, -1, -12, -123, -1234, -123456 } ;

	printf ("    %-24s : ", "psf_log_printf_test") ;
	fflush (stdout) ;

	psf = &sf_private ;
	memset (psf, 0, sizeof (sf_private)) ;

	CMP_0_ARGS (__LINE__, errors, " ->%%<- ") ;

	/* Test printing of ints. */
	for (k = 0 ; k < ARRAY_LEN (int_values) ; k++)
		CMP_6_ARGS (__LINE__, errors, "int A : %d, % d, %4d, % 4d, %04d, % 04d", int_values [k]) ;

	for (k = 0 ; k < ARRAY_LEN (int_values) ; k++)
		CMP_5_ARGS (__LINE__, errors, "int B : %+d, %+4d, %+04d, %-d, %-4d", int_values [k]) ;

	for (k = 0 ; k < ARRAY_LEN (int_values) ; k++)
		CMP_2_ARGS (__LINE__, errors, "int C : %- d, %- 4d", int_values [k]) ;

	/* Test printing of unsigned ints. */
	for (k = 0 ; k < ARRAY_LEN (int_values) ; k++)
		CMP_4_ARGS (__LINE__, errors, "D : %u, %4u, %04u, %0u", int_values [k]) ;

	/* Test printing of hex ints. */
	for (k = 0 ; k < ARRAY_LEN (int_values) ; k++)
		CMP_4_ARGS (__LINE__, errors, "E : %X, %4X, %04X, %0X", int_values [k]) ;

	/* Test printing of strings. */
	CMP_4_ARGS (__LINE__, errors, "B %s, %3s, %8s, %-8s", "str") ;

	if (errors)
	{	puts ("\nExiting due to errors.\n") ;
		exit (1) ;
		} ;

	puts ("ok") ;

	return 0 ;
} /* main */

static int
compare_strings_or_die (int linenum, const char *fmt, const char* s1, const char* s2)
{	int errors = 0 ;
/*-puts (s1) ;puts (s2) ;-*/

	if (strcmp (s1, s2) != 0)
	{	printf ("\n\nLine %d: string compare mismatch:\n\t", linenum) ;
		printf ("\"%s\"\n", fmt) ;
		printf ("\t\"%s\"\n\t\"%s\"\n", s1, s2) ;
		errors ++ ;
		} ;

	return errors ;
} /* compare_strings_or_die */

/*
** Do not edit or modify anything in this comment block.
** The arch-tag line is a file identity tag for the GNU Arch 
** revision control system.
**
** arch-tag: 45147310-868b-400a-97e8-cc0a572a6270
*/
