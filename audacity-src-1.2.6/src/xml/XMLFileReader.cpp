/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLFileReader.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/intl.h>

#include <string.h>

#include "../Internat.h"
#include "XMLFileReader.h"

XMLFileReader::XMLFileReader()
{
   mParser = XML_ParserCreate(NULL);
   XML_SetUserData(mParser, (void *)this);
   XML_SetElementHandler(mParser, startElement, endElement);

   mBaseHandler = NULL;
   mMaxDepth = 128;
   mHandler = new XMLTagHandler*[mMaxDepth];
   mDepth = -1;
   mErrorStr = 0;
}

XMLFileReader::~XMLFileReader()
{
   if (mErrorStr)
      delete[] mErrorStr;
   delete[] mHandler;
   XML_ParserFree(mParser);
}

bool XMLFileReader::Parse(XMLTagHandler *baseHandler,
                          const char *fname)
{
   FILE *fp = fopen(FILENAME(fname), "rb");
   if (!fp || ferror(fp)) {
      const char *formatStr = _("Could not open file: \"%s\"");
      if (mErrorStr)
         delete[] mErrorStr;
      mErrorStr = new char[strlen(fname) + strlen(formatStr) + 10];
      sprintf(mErrorStr, formatStr, fname);
      return false;
   }

   mBaseHandler = baseHandler;
   mHandler[0] = NULL;

   const size_t bufferSize = 16384;
   char buffer[16384];
   int done = 0;
   do {
      size_t len = fread(buffer, 1, bufferSize, fp);
      done = (len < bufferSize);
      if (!XML_Parse(mParser, buffer, len, done)) {
         const char *formatStr = _("Error: %s at line %d");
         const char *errorStr = XML_ErrorString(XML_GetErrorCode(mParser));
         if (mErrorStr)
            delete[] mErrorStr;
         mErrorStr = new char[strlen(errorStr) + strlen(formatStr) + 10];
         sprintf(mErrorStr, formatStr,
                 errorStr, XML_GetCurrentLineNumber(mParser));
         fclose(fp);
         return false;
      }
   } while (!done);

   fclose(fp);

   // Even though there were no parse errors, we only succeed if
   // the first-level handler actually got called, and didn't
   // return false.
   if (mHandler[0])
      return true;
   else {
      const char *errorStr = _("Unable to open project file.");
      if (mErrorStr)
         delete[] mErrorStr;
      mErrorStr = new char[strlen(errorStr)+1];
      strcpy(mErrorStr, errorStr);

      return false;
   }
}

const char *XMLFileReader::GetErrorStr()
{
   return (const char *)mErrorStr;
}

// static
void XMLFileReader::startElement(void *userData, const char *name,
                                 const char **atts)
{
   XMLFileReader *This = (XMLFileReader *)userData;

   This->mDepth++;

   if (This->mDepth >= This->mMaxDepth) {
      XMLTagHandler  **newHandler = new XMLTagHandler*[This->mMaxDepth*2];
      for(int i=0; i<This->mMaxDepth; i++)
         newHandler[i] = This->mHandler[i];
      This->mMaxDepth *= 2;
   }

   if (This->mDepth==0)
      This->mHandler[This->mDepth] = This->mBaseHandler;
   else {
      if (This->mHandler[This->mDepth-1])
         This->mHandler[This->mDepth] =
            This->mHandler[This->mDepth-1]->ReadXMLChild(name);
      else
         This->mHandler[This->mDepth] = NULL;
   }

   if (This->mHandler[This->mDepth]) {
      if (!This->mHandler[This->mDepth]->ReadXMLTag(name, atts))
         This->mHandler[This->mDepth] = 0;
   }
}

// static
void XMLFileReader::endElement(void *userData, const char *name)
{
   XMLFileReader *This = (XMLFileReader *)userData;

   if (This->mHandler[This->mDepth])
      This->mHandler[This->mDepth]->ReadXMLEndTag(name);

   This->mDepth--;
}
