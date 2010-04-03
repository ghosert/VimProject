/**********************************************************************

  Audacity: A Digital Audio Editor

  PlatformCompatibility.cpp

  Markus Meyer

**********************************************************************/

#ifdef _WIN32
#include <windows.h>
#endif

#include "PlatformCompatibility.h"

wxString PlatformCompatibility::GetLongFileName(const wxString& shortFileName)
{
#ifndef _WIN32

   // On other platforms than Win32, this function is only a dummy
   return shortFileName;

#else

   wxString stringLeft = shortFileName;
   wxString s;
   wxString longFileName;

   while (!stringLeft.IsEmpty())
   {
      // Get next path component
      int end = stringLeft.Find('\\');
      
      if (end == -1) {
         s = stringLeft;
         stringLeft.Empty();
      } else {
         s = stringLeft.Left(end);
         stringLeft = stringLeft.Mid(end + 1);
      }

      // Process it unless it is empty, or the drive name
      if (!(s.IsEmpty() || (s.Length() == 2 && s.GetChar(1) == ':')))
      {
         // Now resolve for _every_ path component the long file name.
         // The standard way (only resolve components with '~' in it) is not
         // enough, because the user may have NameNumericTail turned off.
         wxString pathToFind = longFileName + s;
         WIN32_FIND_DATA findData;
         HANDLE findHandle = FindFirstFile((const char*)pathToFind, &findData);

         if (findHandle != INVALID_HANDLE_VALUE)
         {
            s = findData.cFileName; // cFileName always contains long file name
            FindClose(findHandle);
         }
      }

      // Note that we must be careful here to prevent discarding any
      // backslashes. It could e.g. be a network name, too!
      longFileName += s;
      if (end != -1)
         longFileName += "\\";
   }

   return longFileName;

#endif
}
