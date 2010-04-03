/**********************************************************************

  Audacity: A Digital Audio Editor

  Envelope.h

  Dominic Mazzoni

  This class manages an envelope - i.e. a piecewise linear funtion
  that the user can edit by dragging control points around.  The
  envelope is most commonly used to control the amplitude of a
  waveform, but it is also used to shape a general FFT filter.

**********************************************************************/

#ifndef __AUDACITY_ENVELOPE__
#define __AUDACITY_ENVELOPE__

#include <stdlib.h>

#include <wx/dynarray.h>
#include <wx/brush.h>
#include <wx/pen.h>

#include "xml/XMLTagHandler.h"
#include "Internat.h"

class wxRect;
class wxDC;
class wxMouseEvent;
class wxTextFile;

class DirManager;

#define ENV_DB_RANGE 36

struct EnvPoint : public XMLTagHandler {
   double t;
   double val;

   bool HandleXMLTag(const char *tag, const char **attrs)
   {
      if (!strcmp(tag, "controlpoint")) {
         while (*attrs) {
            const char *attr = *attrs++;
            const char *value = *attrs++;
            if (!strcmp(attr, "t"))
               t = Internat::CompatibleToDouble(value);
            else if (!strcmp(attr, "val"))
               val = Internat::CompatibleToDouble(value);
         }
         return true;
      }
      else
         return false;
   }

   XMLTagHandler *HandleXMLChild(const char *tag)
   {
      return NULL;
   }

   void WriteXML(int depth, FILE *fp)
   {
      int i;
      for (i=0; i<depth; i++)
         fprintf(fp, "\t");
      fprintf(fp, "<controlpoint t='%s' val='%s'/>\n",
            Internat::ToString(t,8).c_str(),
            Internat::ToString(val).c_str());
   }
};

WX_DEFINE_ARRAY(EnvPoint *, EnvArray);

class Envelope : public XMLTagHandler {
 public:
   Envelope();
   void Initialize(int numPoints);

   virtual ~ Envelope();

   void SetInterpolateDB(bool db);
   void Mirror(bool mirror);

   void Flatten(double value);
   void SetDefaultValue(double value) {mDefaultValue=value;}

#if LEGACY_PROJECT_FILE_SUPPORT
   // File I/O

   virtual bool Load(wxTextFile * in, DirManager * dirManager);
   virtual bool Save(wxTextFile * out, bool overwrite);
#endif

   // Newfangled XML file I/O
   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);


   // Event Handlers

   void Draw(wxDC & dc, wxRect & r, double h, double pps, bool dB,
             float zoomMin=-1.0, float zoomMax=1.0);

   // Each ofthese returns true if parents needs to be redrawn

   bool MouseEvent(wxMouseEvent & event, wxRect & r,
                   double h, double pps, bool dB,
                   float zoomMin=-1.0, float zoomMax=1.0);

   bool HandleMouseButtonDown( wxMouseEvent & event, wxRect & r,
                               double h, double pps, bool dB,
                               float zoomMin=-1.0, float zoomMax=1.0);
   bool HandleDragging( wxMouseEvent & event, wxRect & r,
                        double h, double pps, bool dB,
                        float zoomMin=-1.0, float zoomMax=1.0);
   bool HandleMouseButtonUp( wxMouseEvent & event, wxRect & r,
                             double h, double pps, bool dB,
                             float zoomMin=-1.0, float zoomMax=1.0);

   void GetEventParams( int &height, bool &upper, bool dB,
                        wxMouseEvent & event, wxRect & r,
                        float &zoomMin, float &zoomMax);

   // Handling Cut/Copy/Paste events
   void CollapseRegion(double t0, double t1);
   void CopyFrom(const Envelope * e, double t0, double t1);
   void Paste(double t0, Envelope *e);
   void InsertSpace(double t0, double tlen);

   // Control

   void SetOffset(double newOffset);
   void SetTrackLen(double trackLen);

   // Accessors

   double GetValue(double t) const; // value at time.
   double GetValueAtX( int x, wxRect & r, double h, double pps); // value at pixel X

   // This is much faster than calling GetValue() multiple times
   // if you need more than one value in a row.
   void GetValues(double *buffer, int len, double t0, double tstep) const;

   int NumberOfPointsAfter(double t);
   double NextPointAfter(double t);

   double Average( double t0, double t1 );
   double Integral( double t0, double t1 );
   double Integral( double t0, double t1, double minY, double maxY );

   void print();
   void testMe();

   bool IsDirty() const;

   // Add a point at a particular spot
   int Insert(double when, double value);

   // Move a point at when to value
   // Returns 0 if point moved, -1 if not found.
   int Move(double when, double value);

   // Return number of points
   int GetNumberOfPoints() const;

   // Returns the sets of when and value pairs
   void GetPoints(double *bufferWhen,
                  double *bufferValue,
                  int bufferLen) const;

 private:

   bool mMirror;

   double fromDB(double x) const;
   double toDB(double x);

   EnvArray mEnv;
   double mOffset;
   double mTrackLen;
   double mTrackEpsilon;
   double mDefaultValue;

   int mDragPoint;
   int mInitialX;
   int mInitialY;
   int mContourOffset; // Number of pixels contour is from the true envelope.
   double mInitialWhen;
   double mInitialVal;
   bool mUpper;
   bool mIsDeleting;

   bool mDB;
   bool mDirty;

   // These are memoizing variables for Integral()
   double lastIntegral_t0;
   double lastIntegral_t1;
   double lastIntegral_result;
   // and this function resets them (call whenever the Envelope changes)
   void resetIntegralMemoizer() { lastIntegral_t0=0; lastIntegral_t1=0; lastIntegral_result=0; }

   float ValueOfPixel( int y, int height, bool upper, bool dB,
                       float zoomMin, float zoomMax );
};

#endif

