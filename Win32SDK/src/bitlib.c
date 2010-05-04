/*--------------------------------------------------------------
   BITLIB.C -- Code entry point for BITLIB dynamic-link library
               (c) Charles Petzold,  1998

  --------------------------------------------------------------*/

#include <windows.h>


// We link and compile this dll as a pure resource, so we do not need EXPORT any function here.
// 	gcc -c -o bin/bitlib.o src/bitlib.c
//  windres -o bin/bitlibres.o src/bitlib.rc
//  gcc -o bin/bitlib.dll bin/bitlib.o bin/bitlibres.o -shared
int WINAPI DllMain (HINSTANCE hInstance, DWORD fdwReason, PVOID pvReserved)
{
     return TRUE ;
}
