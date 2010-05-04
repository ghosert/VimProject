#include <windows.h>
#include "DataStructLib.h"

typedef struct {
	HANDLE hEvent;
	LinkedList *pLinkedList;
    CRITICAL_SECTION cs;
} THREADPARAMS, *PTHREADPARAMS;

THREADPARAMS initializeThread();

void destroyThread(PTHREADPARAMS pThreadParams);

/**
 * Use this method to put request to channel in producer thread.
 */
void putRequestToChannel(PTHREADPARAMS pThreadParams, PVOID pVoid);

/**
 * Use this method to get request from channel in consumer thread.
 */
PVOID getRequestFromChannel(PTHREADPARAMS pThreadParams);
