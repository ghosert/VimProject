/* switches.h -- conditional compilation features for WIN32 systems */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  major reorganization of conditional compilation in Nyquist
 */

#ifdef SWITCHES
Error: switches.h included more than once.
#endif

#define HAS_STDLIB_H 1
#define HAS_SYS_TYPES_H 1
#define HAS_SYS_STAT_H 1
#undef HAS_STAT_H
#undef HAS_MALLOC_H

/* define one of HAS_GETTIMEOFDAY, HAS_FTIME, */
#undef HAS_GETTIMEOFDAY
#undef HAS_FTIME

#undef READLINE

#undef USE_RAND
#define USE_RANDOM 1


/* since there are 2 versions Nyquist for windows: nyquist and nyqwin, 
   we use WINGUI to decide which to compile 
 */
#ifndef WINGUI
/* use C library printf as nyquist_printf() */
#define USE_PRINTF 1
#endif

/* define this to be printf, or define your own fn of the form
     void nyquist_printf(char *format, ...);
   (for a GUI)
*/
void nyquist_printf(char *format, ...);

#define NEED_ULONG 1
#define NEED_USHORT 1
#define NEED_BYTE 1

#define NEED_ROUND 1

#undef NEED_DEFINE_MALLOC

/* explicitly choose a platform */
#undef UNIX

#ifndef WINDOWS
#define WINDOWS 1
#endif

#ifndef WIN32
#define WIN32 1
#endif

#ifndef MICROSOFT
#define MICROSOFT 1
#endif

#ifndef DOS
#define DOS 1
#endif

#undef MACINTOSH

#define BUFFERED_SYNCHRONOUS_INPUT 1
#define SPACE_FOR_PLAY 10000
#define MAX_CHANNELS 16

/* this will enable code to read midi files, etc. */
#define CMTSTUFF 1

/* NYQUIST tells some CMT code that we're really in
 * XLISP and NYQUIST
 */
#define NYQUIST 1

#include "swlogic.h"
