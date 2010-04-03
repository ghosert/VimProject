/**********************************************************************

  Audacity: A Digital Audio Editor

  Internat.h

  Markus Meyer
  Dominic Mazzoni (Mac OS X code)

**********************************************************************/

#ifndef __AUDACITY_INTERNAT__
#define __AUDACITY_INTERNAT__

#include <wx/string.h>

// This class is used to help internationalisation and in general
// compatibility with different locales and character sets.
// It deals mostly with converting numbers, but also has important
// functions to convert to/from UTF-8, which is used in XML files
// and on Mac OS X for the filesystem.
class Internat
{
public:
   // Initialize internationalisation support. Call this once at
   // program start.
   static void Init();

   // Get the decimal separator for the current locale. Normally, this is
   // a decimal point ('.'), but e.g. Germany uses a comma (',').
   static wxChar GetDecimalSeparator();

   // Convert a string to a number. This function will accept BOTH point
   // and comma as a decimal separator, regardless of the current locale.
   // Returns 'true' on success, and 'false' if an error occurs.
   static bool CompatibleToDouble(const wxString& stringToConvert, double* result);

   // Function version of above.
   static double CompatibleToDouble(const wxString& stringToConvert);

   // Convert a number to a string, using the given decimal separator.
   // The default is to use the decimal separator for the current locale.
   static wxString ToString(double numberToConvert,
                     int digitsAfterDecimalPoint = -1);

   // Convert strings to and from UTF-8 (used for XML files).
   static wxString UTF8ToLocal(const wxString &s);
   static wxString LocalToUTF8(const wxString &s);

   static wxString ToFilename(const wxString &s);
   static wxString FromFilename(const wxString &s);

private:
   static wxChar mDecimalSeparator;
   static wxMBConv *mConvLocal;

   #ifdef __WXMAC__
   static void *mTECToUTF;
   static void *mTECFromUTF;
   #endif
};

// Use this macro to wrap all filenames and pathnames that get
// passed directly to a system call, like opening a file, creating
// a directory, checking to see that a file exists, etc...
#ifdef __WXMAC__
#define FILENAME(X) Internat::ToFilename(X)
#define FROMFILENAME(X) Internat::FromFilename(X)
#else
#define FILENAME(X) (X)
#define FROMFILENAME(X) (X)
#endif

#endif
