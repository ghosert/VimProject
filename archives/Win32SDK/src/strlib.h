/*----------------------
   STRLIB.H header file
  ----------------------*/


#ifdef __cplusplus
#define EXPORT extern "C" __declspec (dllexport)
#else
#define EXPORT __declspec (dllexport)
#endif

     // The maximum number of strings STRLIB will store and their lengths

#define MAX_STRINGS 256
#define MAX_LENGTH 63

     // The callback function type definition uses generic strings

// It's a way to define function pointer type here and later, you can use GETSTRCB to define a function pointer variable.
typedef BOOL (CALLBACK * GETSTRCB) (PTSTR, PVOID) ;

     // Each function has ANSI and Unicode versions

EXPORT BOOL CALLBACK AddStringA (PCSTR) ;
EXPORT BOOL CALLBACK AddStringW (PCWSTR) ;

EXPORT BOOL CALLBACK DeleteStringA (PCSTR) ;
EXPORT BOOL CALLBACK DeleteStringW (PCWSTR) ;

EXPORT int CALLBACK GetStringsA (GETSTRCB, PVOID) ;
EXPORT int CALLBACK GetStringsW (GETSTRCB, PVOID) ;
 
     // Use the correct version depending on the UNICODE identifier

#ifdef UNICODE
#define AddString    AddStringW
#define DeleteString DeleteStringW
#define GetStrings   GetStringsW
#else
#define AddString    AddStringA
#define DeleteString DeleteStringA
#define GetStrings   GetStringsA
#endif

