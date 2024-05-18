// If you want to create an animation in a non-ui thread
// You can use Sleep function to replace Timer in UI thread
// For you fail to create window in a non-ui thread, so you can't receive message in non-ui thread, including
// WM_TIMER.

// Be care to use critical section in ui thread, for if non-ui thread take some time to do something,
// it will also prevent main thread go ahead. We suggest in non-ui thread, just copy shared parameter value in
// critical section.

/* Sample code snippet
CRITICAL_SECTION cs;
InitializeCriticalSection(&cs);
EnterCriticalSection(&cs);
LeaveCriticalSection(&cs);
DeleteCriticalSection(&cs);
*/

// Thread Local Storage like Thread local in Java.

/* Sample code snippet
Just preface any variable that needs to be different for each thread with __declspec (thread), like so 

__declspec (thread) int iGlobal = 1 ;

for static variables external to any function, or like so 

__declspec (thread) static int iLocal = 2 ;

for static variables within functions. 
*/
#include <windows.h>
#include <process.h>
#include "MultiThreadWithFixBufferLib.h"

/* Define a custom request data struct for producer/consumer thread, eg. you can define function pointer here. */
typedef struct _REQUEST {
		int i;
} REQUEST;

void consumerThread(PVOID pVoid) {
	PFIXBUFTHREADPARAMS pThreadParams = (PFIXBUFTHREADPARAMS) pVoid;
	while (1) {

		REQUEST *pRequest = (REQUEST *) getRequestFromFixBufChannel(pThreadParams);
		
		/* Do anything below you want to do in consumer thread for any long time big job. */
		printf("Consumer Thread have got one request from channel request id: %d\n", pRequest->i);
		/* Finish doing what you want to do above. */
		
		free(pRequest);
		
	    Sleep(1000);
	}
	_endthread();
}

void producerThread(PVOID pVoid) {
	PFIXBUFTHREADPARAMS pThreadParams = (PFIXBUFTHREADPARAMS) pVoid;
	int iData = 0;
	while (1) {
		
		/* Create and put your request to channel in producer thread. For desktop software producer thread is often refered to UI thread.
		 * Or it's a polling thread like a thread to poll the serial port in a short time. */
	    REQUEST *pRequest = malloc(sizeof(REQUEST)); // Use REQUEST request; here will cause a serious problem because while looping will use the same memory address all the time.
	    pRequest->i = iData++;
	    
		printf("Producer Thread plan to put one request to channel request id: %d\n", pRequest->i);
		putRequestToFixBufChannel(pThreadParams, pRequest);
		
	    Sleep(500);
	}
	_endthread();
}

int main(int argc, char *argv[]) {
	
	FIXBUFTHREADPARAMS threadParams = initializeFixBufThread(5);
	
	// Start a consumer thread.
	_beginthread(consumerThread, 0, &threadParams);
	
	// Start a producer thread.
	_beginthread(producerThread, 0, &threadParams);
	
	// Pending the current thread, for if current main thread is over, other threads will also be killed.
	// Opposite to Java.
    HANDLE hEvent = CreateEvent (NULL, FALSE, FALSE, NULL) ;
	WaitForSingleObject(hEvent, INFINITE);
	
	destroyFixBufThread(&threadParams);
	
}
