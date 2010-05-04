/*-------------------------------------------------
   EDRLIB.C -- Easy Drawing Routine Library module

               (c) Charles Petzold, 1998
  -------------------------------------------------*/

#include  <windows.h>
#include "edrlib.h"


// If you need hInstance here later, save it to a global variable, and use it later.
// pvReserved is reserved by system.
// fdwReason can be one of the four values below:
// DLL_PROCESS_ATTACH means a new process invoke the dll.
// DLL_PROCESS_DETACH means process don't need the dll any more. It's a chance here to clear something.
// DLL_THREAD_ATTACH is similar to DLL_PROCESS_ATTACH
// DLL_THREAD_DETACH is similar to DLL_PROCESS_DETACH, you can invoke SendMessage here because the thread is till alive,
// but don't use PostMessage, it is async method, when it returns, maybe the current thread has existed.
// RETURN TRUE OR FALSE here, if FALSE means this dll fail to be invoked.
int WINAPI DllMain (HINSTANCE hInstance, DWORD fdwReason, PVOID pvReserved)
{
     return TRUE ;
}

// Note EXPORT keyword here.
EXPORT BOOL CALLBACK EdrCenterTextA (HDC hdc, PRECT prc, PCSTR pString)
{
     int  iLength ;
     SIZE size ;

     iLength = lstrlenA (pString) ;

     GetTextExtentPoint32A (hdc, pString, iLength, &size) ;

     return TextOutA (hdc, (prc->right - prc->left - size.cx) / 2,
                           (prc->bottom - prc->top - size.cy) / 2,
                      pString, iLength) ;
}

EXPORT BOOL CALLBACK EdrCenterTextW (HDC hdc, PRECT prc, PCWSTR pString)
{
     int  iLength ;
     SIZE size ;

     iLength = lstrlenW (pString) ;

     GetTextExtentPoint32W (hdc, pString, iLength, &size) ;

     return TextOutW (hdc, (prc->right - prc->left - size.cx) / 2,
                           (prc->bottom - prc->top - size.cy) / 2,
                      pString, iLength) ;
}

