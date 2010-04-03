/* stimemac.c -- portable timer implementation for macOS */



#include <Events.h>



#include "stime.h"



long gSTimeStartTime = TickCount();

int gSTimeResolution = 1000;



void stime_start(int resolution)

{

  gSTimeStartTime = TickCount();

  gSTimeResolution = resolution;

}



unsigned long stime_get()

{

  long elapsed = (TickCount() - gSTimeStartTime);

  

  double e2 = (elapsed / 60.0) * gSTimeResolution;

  

  return (unsigned long)(e2 + 0.5);

}



