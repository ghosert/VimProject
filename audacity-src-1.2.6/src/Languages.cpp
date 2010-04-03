/**********************************************************************

  Audacity: A Digital Audio Editor

  Languages.cpp

  Dominic Mazzoni

  Figure out what translations are installed and return a list
  of language codes (like "es", "fr", or "pt-br") and corresponding
  language names (like "Español", "Français", and "Português").
  We use our own list of translations of language names (i.e.
  "Français" instead of "French") but we fallback on the language
  name in wxWindows if we don't have it listed.

  This code is designed to work well with all of the current
  languages, but adapt to any language that wxWindows supports.
  Other languages will only be supported if they're added to
  the database using wxLocale::AddLanguage.

  But for the most part, this means that somebody could add a new
  translation and have it work immediately.

**********************************************************************/

#include <wx/defs.h>
#include <wx/hashmap.h>
#include <wx/intl.h>

#include "Languages.h"

#include "Audacity.h"
#include "AudacityApp.h"

WX_DECLARE_STRING_HASH_MAP(wxString, LangHash);

bool TranslationExists(wxArrayString &audacityPathList, wxString code)
{
   wxArrayString results;   
   wxGetApp().FindFilesInPathList(wxString::Format("%s/audacity.mo",
                                                   (const char *)code),
                                  audacityPathList,
                                  wxFILE,
                                  results);
   
   wxGetApp().FindFilesInPathList(wxString::Format("%s/LC_MESSAGES/audacity.mo",
                                                   (const char *)code),
                                  audacityPathList,
                                  wxFILE,
                                  results);
   
   return (results.GetCount() > 0);
}

wxString GetSystemLanguageCode()
{
   wxArrayString langCodes;
   wxArrayString langNames;

   GetLanguages(langCodes, langNames);
   int sysLang = wxLocale::GetSystemLanguage();
   const wxLanguageInfo *info = wxLocale::GetLanguageInfo(sysLang);
   
   if (info) {
      wxString fullCode = info->CanonicalName;
      if (fullCode.Length() < 2)
         return "en";

      wxString code = fullCode.Left(2);
      unsigned int i;

      for(i=0; i<langCodes.GetCount(); i++) {
         if (langCodes[i] == fullCode)
            return fullCode;

         if (langCodes[i] == code)
            return code;
      }
   }

   return "en";
}

void GetLanguages(wxArrayString &langCodes, wxArrayString &langNames)
{
   wxArrayString tempNames;
   wxArrayString tempCodes;
   LangHash localLanguageName;
   LangHash reverseHash;
   LangHash tempHash;

   // MM: Use only ASCII characters here to avoid problems with
   //     charset conversion on Linux platforms
   localLanguageName["ar"] = "Arabic";
   localLanguageName["bg"] = "Balgarski";
   localLanguageName["bn"] = "Bangla";
   localLanguageName["ca"] = "Catalan";
   localLanguageName["cs"] = "Czech";
   localLanguageName["cy"] = "Cymraeg";
   localLanguageName["da"] = "Dansk";
   localLanguageName["de"] = "Deutsch";
   localLanguageName["el"] = "Ellinika";
   localLanguageName["en"] = "English";
   localLanguageName["es"] = "Espanol";
   localLanguageName["eu"] = "Euskara";
   localLanguageName["fi"] = "Suomi";
   localLanguageName["fr"] = "Francais";
   localLanguageName["ga"] = "Gaeilge";
   localLanguageName["gl"] = "Galego";
   localLanguageName["it"] = "Italiano";
   localLanguageName["ja"] = "Nihongo";
   localLanguageName["lo"] = "Lao";
   localLanguageName["lt"] = "Lietuviu";
   localLanguageName["hu"] = "Magyar";
   localLanguageName["mk"] = "Makedonski";
   localLanguageName["nl"] = "Nederlands";
   localLanguageName["nb"] = "Norsk";
   localLanguageName["pl"] = "Polski";
   localLanguageName["pt"] = "Portugues";
   localLanguageName["ro"] = "Romana";
   localLanguageName["ru"] = "Russky";
   localLanguageName["sk"] = "Slovensky";
   localLanguageName["sl"] = "Slovenscina";
   localLanguageName["sv"] = "Svenska";
   localLanguageName["tr"] = "Turkce";
   localLanguageName["uk"] = "Ukrainska";
   localLanguageName["zh"] = "Chinese (Simplified)";
   localLanguageName["zh_TW"] = "Chinese (Traditional)";

   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxGetApp().AddUniquePathToPathList(wxString::Format("%s/share/locale",
                                                       INSTALL_PREFIX),
                                      audacityPathList);
   int i;
   for(i=wxLANGUAGE_UNKNOWN; i<wxLANGUAGE_USER_DEFINED;i++) {
      const wxLanguageInfo *info = wxLocale::GetLanguageInfo(i);

      if (!info)
         continue;

      wxString fullCode = info->CanonicalName;
      wxString code = fullCode.Left(2);
      wxString name = info->Description;

      // Logic: Languages codes are sometimes hierarchical, with a
      // general language code and then a subheading.  For example,
      // zh_TW for Traditional Chinese and zh_CN for Simplified
      // Chinese - but just zh for Chinese in general.  First we look
      // for the full code, like zh_TW.  If that doesn't exist, we
      // look for a code corresponding to the first two letters.
      // Note that if the language for a fullCode exists but we only
      // have a name for the short code, we will use the short code's
      // name but associate it with the full code.  This allows someone
      // to drop in a new language and still get reasonable behavior.

      if (fullCode.Length() < 2)
         continue;

      if (localLanguageName[code] != "") {
         name = localLanguageName[code];
      }
      if (localLanguageName[fullCode] != "") {
         name = localLanguageName[fullCode];
      }

      if (TranslationExists(audacityPathList, fullCode)) {
         code = fullCode;
      }

      if (tempHash[code] != "")
         continue;

      if (TranslationExists(audacityPathList, code) || code=="en") {
         tempCodes.Add(code);
         tempNames.Add(name);
         tempHash[code] = name;

         /* for debugging
         printf("code=%s name=%s fullCode=%s name=%s -> %s\n",
                code.c_str(), localLanguageName[code].c_str(),
                fullCode.c_str(), localLanguageName[fullCode].c_str(),
                name.c_str());
         */
      }
   }

   // Sort

   unsigned int j;
   for(j=0; j<tempNames.GetCount(); j++)
      reverseHash[tempNames[j]] = tempCodes[j];

   tempNames.Sort();

   for(j=0; j<tempNames.GetCount(); j++) {
      langNames.Add(tempNames[j]);
      langCodes.Add(reverseHash[tempNames[j]]);
   }
}

