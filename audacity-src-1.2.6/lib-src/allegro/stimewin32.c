/* stimewin32.c -- portable timer implementation for win32 */



#include "stime.h"



#include "windows.h"

#include "time.h"





TIMECAPS caps;



void stime_start(int resolution)

{

}



unsigned long stime_get()

{

    return timeGetTime();

}



