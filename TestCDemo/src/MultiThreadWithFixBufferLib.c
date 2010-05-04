#include "MultiThreadWithFixBufferLib.h"

FIXBUFTHREADPARAMS initializeFixBufThread(int iFixNum) {
	FIXBUFTHREADPARAMS threadParams;
	threadParams.hProducerEvent = CreateEvent(NULL, TRUE, TRUE, NULL); /* The third parameter should be TRUE, or it will cause a dead lock problem. */
	threadParams.hConsumerEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
	threadParams.iFixNum = iFixNum;
	threadParams.iPutPosition = 0;
	threadParams.iGetPosition = 0;
	threadParams.iCount = 0;
	threadParams.pPVoid = malloc(iFixNum * sizeof(PVOID));
	
	// Initialize Critical Section
    InitializeCriticalSection(&threadParams.cs);
    
    return threadParams;
}

void destroyFixBufThread(PFIXBUFTHREADPARAMS pThreadParams) {
	free(pThreadParams->pPVoid);
	// Delete Critical Section
	DeleteCriticalSection(&pThreadParams->cs);
}

/**
 * Use this method to put request to channel in producer thread.
 */
void putRequestToFixBufChannel(PFIXBUFTHREADPARAMS pThreadParams, PVOID pVoid) {
	
	wait:
    WaitForSingleObject(pThreadParams->hProducerEvent, INFINITE);
    
	// synchronize
	EnterCriticalSection(&pThreadParams->cs);
	if (pThreadParams->iCount >= pThreadParams->iFixNum) {
	    ResetEvent(pThreadParams->hProducerEvent);
	    LeaveCriticalSection(&pThreadParams->cs);
	    goto wait;
	}
	pThreadParams->pPVoid[pThreadParams->iPutPosition] = pVoid;
	pThreadParams->iPutPosition = (pThreadParams->iPutPosition + 1) % pThreadParams->iFixNum;
	pThreadParams->iCount++;
    SetEvent(pThreadParams->hConsumerEvent);
	LeaveCriticalSection(&pThreadParams->cs);
}

/**
 * Use this method to get request from channel in consumer thread.
 */
PVOID getRequestFromFixBufChannel(PFIXBUFTHREADPARAMS pThreadParams) {
	
	wait:
    WaitForSingleObject(pThreadParams->hConsumerEvent, INFINITE);
    
	// synchronize
	EnterCriticalSection(&pThreadParams->cs);
	if (pThreadParams->iCount <= 0) {
	    ResetEvent(pThreadParams->hConsumerEvent);
	    LeaveCriticalSection(&pThreadParams->cs);
	    goto wait;
	}
	PVOID pVoid = pThreadParams->pPVoid[pThreadParams->iGetPosition];
	pThreadParams->iGetPosition = (pThreadParams->iGetPosition + 1) % pThreadParams->iFixNum;
	pThreadParams->iCount--;
    SetEvent(pThreadParams->hProducerEvent);
	LeaveCriticalSection(&pThreadParams->cs);
	return pVoid;
}
