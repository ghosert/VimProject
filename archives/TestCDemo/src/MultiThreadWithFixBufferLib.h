#include <windows.h>

typedef struct {
	HANDLE hProducerEvent;
	HANDLE hConsumerEvent;
	int iFixNum;
	PVOID *pPVoid;
	int iPutPosition;
	int iGetPosition;
	int iCount; /* number of request in current buffer. */
    CRITICAL_SECTION cs;
} FIXBUFTHREADPARAMS, *PFIXBUFTHREADPARAMS;

FIXBUFTHREADPARAMS initializeFixBufThread(int iFixNum);

void destroyFixBufThread(PFIXBUFTHREADPARAMS pThreadParams);

/**
 * Use this method to put request to channel in producer thread.
 */
void putRequestToFixBufChannel(PFIXBUFTHREADPARAMS pThreadParams, PVOID pVoid);

/**
 * Use this method to get request from channel in consumer thread.
 */
PVOID getRequestFromFixBufChannel(PFIXBUFTHREADPARAMS pThreadParams);
