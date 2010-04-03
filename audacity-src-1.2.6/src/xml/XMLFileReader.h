/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLFileReader.h

  Dominic Mazzoni

**********************************************************************/

#include "xmlparse/xmlparse.h"

#include "XMLTagHandler.h"

class XMLFileReader {
 public:
   XMLFileReader();
   virtual ~XMLFileReader();

   bool Parse(XMLTagHandler *baseHandler,
              const char *fname);

   const char *GetErrorStr();

   // Callback functions for expat

   static void startElement(void *userData, const char *name,
                            const char **atts);

   static void endElement(void *userData, const char *name);

 private:
   XML_Parser       mParser;
   int              mMaxDepth;
   int              mDepth;
   XMLTagHandler  **mHandler;
   XMLTagHandler   *mBaseHandler;
   char            *mErrorStr;
};


