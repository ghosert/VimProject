/*----------------------
   EDRLIB.H header file

  ----------------------*/

// Both souce code of exe and dll will use this .h file.


// Need this definition to avoid c++ compile conflicts.
#ifdef __cplusplus
#define EXPORT extern "C" __declspec (dllexport)
#else
#define EXPORT __declspec (dllexport)
#endif


// EXPORT keyword will make sure the the function name will be added to import .lib file and then be linked to exe file so that exe can find
// function name in dll file in the runtime.
EXPORT BOOL CALLBACK EdrCenterTextA (HDC, PRECT, PCSTR) ;
EXPORT BOOL CALLBACK EdrCenterTextW (HDC, PRECT, PCWSTR) ;

#ifdef UNICODE
#define EdrCenterText EdrCenterTextW
#else
#define EdrCenterText EdrCenterTextA
#endif

