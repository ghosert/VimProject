/* time.h -- portable interface to millisecond timer */



#ifdef __cplusplus

extern "C" {

#endif



void stime_start(int resolution);

unsigned long stime_get();



#ifdef __cplusplus

}

#endif

