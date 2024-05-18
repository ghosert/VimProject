#include "MultiThreadLib.h"

THREADPARAMS initializeThread() {
	THREADPARAMS threadParams;
	threadParams.hEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
	threadParams.pLinkedList = CreateLinkedList();
	
	// Initialize Critical Section
    InitializeCriticalSection(&threadParams.cs);
    
    return threadParams;
}

void destroyThread(PTHREADPARAMS pThreadParams) {
	DeleteLinkedList(pThreadParams->pLinkedList);
	// Delete Critical Section
	DeleteCriticalSection(&pThreadParams->cs);
}


/**
 * Use this method to put request to channel in producer thread.
 */
void putRequestToChannel(PTHREADPARAMS pThreadParams, PVOID pVoid) {
	// synchronize
	EnterCriticalSection(&pThreadParams->cs);
	Node *pNode = malloc(sizeof(Node)); // Use Node node; here will cause a serious problem because every node will use same memory address all the time.
	pNode->pData = pVoid;
	pNode->next = NULL;
	AddLast(pThreadParams->pLinkedList, pNode);
    SetEvent(pThreadParams->hEvent);
	LeaveCriticalSection(&pThreadParams->cs);
}

/**
 * Use this method to get request from channel in consumer thread.
 */
PVOID getRequestFromChannel(PTHREADPARAMS pThreadParams) {
	
	wait:
    WaitForSingleObject(pThreadParams->hEvent, INFINITE);
    
	// synchronize
	EnterCriticalSection(&pThreadParams->cs);
	if (GetLinkedListSize(pThreadParams->pLinkedList) <= 0) {
	    ResetEvent(pThreadParams->hEvent);
	    LeaveCriticalSection(&pThreadParams->cs);
	    goto wait;
	}
	Node *pNode = RemoveFirst(pThreadParams->pLinkedList);
	PVOID pVoid = pNode->pData;
	free(pNode);
	LeaveCriticalSection(&pThreadParams->cs);
	return pVoid;
}
