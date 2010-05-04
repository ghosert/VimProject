/*------------------------------------------------
   STRLIB.C -- Library module for STRPROG program
               (c) Charles Petzold, 1998
  ------------------------------------------------*/

#include <windows.h>
#include <wchar.h>       // for wide-character string functions
#include "strlib.h"

     // shared memory section (requires /SECTION:shared,RWS in link options)

#pragma data_seg ("shared")
int   iTotal = 0 ;
WCHAR szStrings [MAX_STRINGS][MAX_LENGTH + 1] = { '\0' } ;
#pragma data_seg ()

#pragma comment(linker,"/SECTION:shared,RWS")

int WINAPI DllMain (HINSTANCE hInstance, DWORD fdwReason, PVOID pvReserved)
{
     return TRUE ;
}

EXPORT BOOL CALLBACK AddStringA (PCSTR pStringIn)
{
     BOOL  bReturn ;
     int   iLength ;
     PWSTR pWideStr ;

          // Convert string to Unicode and call AddStringW

     iLength = MultiByteToWideChar (CP_ACP, 0, pStringIn, -1, NULL, 0) ;
     pWideStr = malloc (iLength) ;
     MultiByteToWideChar (CP_ACP, 0, pStringIn, -1, pWideStr, iLength) ;
     bReturn = AddStringW (pWideStr) ;
     free (pWideStr) ;

     return bReturn ;
}

EXPORT BOOL CALLBACK AddStringW (PCWSTR pStringIn)
{
     PWSTR pString ;
     int   i, iLength ;
     
     if (iTotal == MAX_STRINGS - 1)
          return FALSE ;
     
     if ((iLength = wcslen (pStringIn)) == 0)
          return FALSE ;

          // Allocate memory for storing string, copy it, convert to upper case

     pString = malloc (sizeof (WCHAR) * (1 + iLength)) ;
     wcscpy (pString, pStringIn) ;
     _wcsupr (pString) ;

          // Alphabetize the strings
     
     for (i = iTotal ; i > 0 ; i--)
     {
          if (wcscmp (pString, szStrings[i - 1]) >= 0)
               break ;
          
          wcscpy (szStrings[i], szStrings[i - 1]) ;
     }
     wcscpy (szStrings[i], pString) ;
     iTotal++ ;

     free (pString) ;
     return TRUE ;
}

EXPORT BOOL CALLBACK DeleteStringA (PCSTR pStringIn)
{
     BOOL  bReturn ;
     int   iLength ;
     PWSTR pWideStr ;

          // Convert string to Unicode and call DeleteStringW

     iLength = MultiByteToWideChar (CP_ACP, 0, pStringIn, -1, NULL, 0) ;
     pWideStr = malloc (iLength) ;
     MultiByteToWideChar (CP_ACP, 0, pStringIn, -1, pWideStr, iLength) ;
     bReturn = DeleteStringW (pWideStr) ;
     free (pWideStr) ;

     return bReturn ;
}

EXPORT BOOL CALLBACK DeleteStringW (PCWSTR pStringIn)
{
     int i, j ;
     
     if (0 == wcslen (pStringIn))
          return FALSE ;
     
     for (i = 0 ; i < iTotal ; i++)
     {
          if (_wcsicmp (szStrings[i], pStringIn) == 0)
               break ;
     }
          // If given string not in list, return without taking action
     
     if (i == iTotal)
          return FALSE ;
     
          // Else adjust list downward
     
     for (j = i ; j < iTotal ; j++)
          wcscpy (szStrings[j], szStrings[j + 1]) ;
     
     szStrings[iTotal--][0] = '\0' ;
     return TRUE ;
}

EXPORT int CALLBACK GetStringsA (GETSTRCB pfnGetStrCallBack, PVOID pParam)
{
     BOOL bReturn ;
     int  i, iLength ;
     PSTR pAnsiStr ;

     for (i = 0 ; i < iTotal ; i++)
     {
               // Convert string from Unicode

          iLength = WideCharToMultiByte (CP_ACP, 0, szStrings[i], -1, NULL, 0,
                                         NULL, NULL) ;
          pAnsiStr = malloc (iLength) ;
          WideCharToMultiByte (CP_ACP, 0, szStrings[i], -1, pAnsiStr, iLength,
                                          NULL, NULL) ;

               // Call callback function

          bReturn = pfnGetStrCallBack (pAnsiStr, pParam) ;
          
          if (bReturn == FALSE)
               return i + 1 ;

          free (pAnsiStr) ;
     }
     return iTotal ;
}

EXPORT int CALLBACK GetStringsW (GETSTRCB pfnGetStrCallBack, PVOID pParam)
{
     BOOL bReturn ;
     int  i ;
     
     for (i = 0 ; i < iTotal ; i++)
     {
          bReturn = pfnGetStrCallBack (szStrings[i], pParam) ;
          
          if (bReturn == FALSE)
               return i + 1 ;
     }
     return iTotal ;
}
