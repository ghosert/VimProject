/**********************************************************************

  Audacity: A Digital Audio Editor

  Envelope.cpp

  Dominic Mazzoni (original author)
  Dr William Bland (integration - the Calculus kind)
  Monty (xiphmont) (important bug fixes)

**********************************************************************/

#include "Envelope.h"

#include <math.h>

#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/event.h>
#include <wx/pen.h>
#include <wx/textfile.h>
#include <wx/log.h>

#include "AColor.h"
#include "Prefs.h"
#include "DirManager.h"
#include "TrackArtist.h"

Envelope::Envelope()
{
   mOffset = 0.0;
   mTrackLen = 0.0;

   // Anything with a sample rate of no more than 200 KHz
   // will have samples spaced apart by at least this amount,
   // "epsilon".  We use this to enforce that there aren't
   // allowed to be multiple control points at the same t
   // value.
   mTrackEpsilon = 1.0 / 200000.0;

   mDB = true;

   mDefaultValue = 1.0;

   mDragPoint = -1;
   mDirty = false;

   mIsDeleting = false;

   mMirror = true;
}

Envelope::~Envelope()
{
   WX_CLEAR_ARRAY(mEnv);
}

void Envelope::SetInterpolateDB(bool db)
{
   mDB = db;
}

void Envelope::Mirror(bool mirror)
{
   mMirror = mirror;
}

void Envelope::Flatten(double value)
{
   WX_CLEAR_ARRAY(mEnv);
   SetDefaultValue(value);
}

void Envelope::CopyFrom(const Envelope *e, double t0, double t1)
{
   mOffset = wxMax(t0, e->mOffset);
   mTrackLen = wxMin(t1, e->mOffset + e->mTrackLen) - mOffset;

   WX_CLEAR_ARRAY(mEnv);
   
   int len = e->mEnv.Count();

   // Skip the points that come before the copied region
   int i = 0;
   while (i < len && e->mOffset + e->mEnv[i]->t <= t0)
      i++;

   // Create the point at 0 if it needs interpolated representation
   if (i) {
      EnvPoint *pt = new EnvPoint();
      pt->t = 0;
      pt->val = e->GetValue(mOffset);
      mEnv.Add(pt);
   }

   // Copy points from inside the copied region
   while (i < len && e->mOffset + e->mEnv[i]->t < mOffset + mTrackLen) {
      EnvPoint *pt = new EnvPoint();
      pt->t = e->mEnv[i]->t + e->mOffset - mOffset;
      pt->val = e->mEnv[i]->val;
      mEnv.Add(pt);
      i++;
   }

   // Create the final point if it needs interpolated representation
   if (mTrackLen > 0 && i < len) {
      EnvPoint *pt = new EnvPoint();
      pt->t = mTrackLen;
      pt->val = e->GetValue(mOffset + mTrackLen);
      mEnv.Add(pt);
   }
}

double Envelope::toDB(double value)
{
   if (value == 0)
      return 0;
   
   double envdBRange = gPrefs->Read("/GUI/EnvdBRange", ENV_DB_RANGE);
   double sign = (value >= 0 ? 1 : -1);

   double db = 20 * log10(fabs(value));
   double val = (db + envdBRange) / envdBRange;
   if (val < 0.0)
      val = 0.0;
   if (val > 1.0)
      val = 1.0;

   return sign * val;
}

double Envelope::fromDB(double value) const
{
   if (value == 0)
      return 0;

   double sign = (value >= 0 ? 1 : -1);
   double envdBRange = gPrefs->Read("/GUI/EnvdBRange", ENV_DB_RANGE);
   return pow(10.0, ((fabs(value) * envdBRange) - envdBRange) / 20.0)*sign;;
}

void DrawPoint(wxDC & dc, wxRect & r, int x, int y, bool top, bool contour)
{
   if (y >= 0 && y <= r.height) {
      if(contour){
         wxRect circle(r.x + x - 2, r.y + y - 2,
                       4, 4);
         dc.DrawEllipse(circle);
      }else{
         wxRect circle(r.x + x - 2, r.y + (top? y-1: y-4),
                       4, 4);
         dc.DrawEllipse(circle);
      }
   }
}

void Envelope::Draw(wxDC & dc, wxRect & r, double h, double pps, bool dB,
                    float zoomMin, float zoomMax)
{
   h -= mOffset;

   double tright = h + (r.width / pps);
   double dBr = gPrefs->Read("/GUI/EnvdBRange", ENV_DB_RANGE);

   dc.SetPen(AColor::envelopePen);
   dc.SetBrush(*wxWHITE_BRUSH);

   for (int i = 0; i < (int)mEnv.Count(); i++) {
      if (mEnv[i]->t >= h && mEnv[i]->t <= tright) {
         if (i == mDragPoint) {
            dc.SetPen(AColor::envelopePen);
            dc.SetBrush(AColor::envelopeBrush);
         }

         double v = mEnv[i]->val;
         int x = int ((mEnv[i]->t - h) * pps);
         int y,y2;

	 y = GetWaveYPosNew(v, zoomMin, zoomMax, r.height, dB,
			    true, dBr, false);
	 DrawPoint(dc, r, x, y, true, false);

         if (mMirror) {
	   y2 = GetWaveYPosNew(-v-.000000001, zoomMin, zoomMax, r.height, dB,
			       true, dBr, false);
	   DrawPoint(dc, r, x, y2, false, false);
	   
	   // Contour
	   y = GetWaveYPosNew(v, zoomMin, zoomMax, r.height, dB,
			      false, dBr, false);
	   y2 = GetWaveYPosNew(-v-.000000001, zoomMin, zoomMax, r.height, dB,
			       false, dBr, false);
	   if(y<=y2){
	     DrawPoint(dc, r, x, y, true, true);
	     DrawPoint(dc, r, x, y2, false, true);
            }
         }

         if (i == mDragPoint) {
            dc.SetPen(AColor::envelopePen);
            dc.SetBrush(*wxWHITE_BRUSH);
         }
      }
   }
}

bool Envelope::HandleXMLTag(const char *tag, const char **attrs)
{
   if (!strcmp(tag, "envelope")) {
      int numPoints = 0;

      while (*attrs) {
         const char *attr = *attrs++;
         const char *value = *attrs++;
         if (!strcmp(attr, "numpoints"))
            numPoints = atoi(value);
      }
      WX_CLEAR_ARRAY(mEnv);
      mEnv.Alloc(numPoints);
      return true;
   }
   else {
      return false;
   }
}

XMLTagHandler *Envelope::HandleXMLChild(const char *tag)
{
   if (!strcmp(tag, "controlpoint")) {
      EnvPoint *e = new EnvPoint();
      mEnv.Add(e);
      return e;
   }
   else
      return NULL;
}

void Envelope::WriteXML(int depth, FILE *fp)
{
   unsigned int ctrlPt;
   int i;

   for (i = 0; i < depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<envelope numpoints='%ld'>\n", (long)mEnv.GetCount());

   for (ctrlPt = 0; ctrlPt < mEnv.GetCount(); ctrlPt++) {
      for(i = 0; i < depth+1; i++)
         fprintf(fp, "\t");
      fprintf(fp, "<controlpoint t='%s' val='%s'/>\n",
            Internat::ToString(mEnv[ctrlPt]->t, 12).c_str(),
            Internat::ToString(mEnv[ctrlPt]->val, 12).c_str());
   }

   for (i = 0; i < depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "</envelope>\n");
}

#ifndef SQR
#define SQR(X) ((X)*(X))
#endif


float Envelope::ValueOfPixel( int y, int height, bool upper, bool dB,
                              float zoomMin, float zoomMax ){
   float v;

   v = zoomMax - (y/(float)height) * (zoomMax - zoomMin);

   if (mContourOffset) 
     if( v > 0.0 )
       v += .5;
     else
       v -= .5;

   if (dB)
      v = fromDB(v);

   if ((upper && v < 0) || (!upper && v > 0))
      v = 0;
   else
   if (!upper)
      v = -v;

   return v;
}

bool Envelope::HandleMouseButtonDown(wxMouseEvent & event, wxRect & r,
                                     double h, double pps, bool dB,
                                     float zoomMin, float zoomMax)
{
   bool upper;
   int ctr = (int)(r.height * zoomMax / (zoomMax - zoomMin));
   upper = (event.m_y - r.y < ctr);
   
   int clip_y = event.m_y - r.y;
   if(clip_y < 0) clip_y = 0;
   if(clip_y > r.height) clip_y = r.height;
   
   mIsDeleting = false;
   double tleft = h - mOffset;
   double tright = tleft + (r.width / pps);
   int bestNum = -1;
   int bestDist = 10; // Must be within 10 pixel radius.
   
   double dBr = gPrefs->Read("/GUI/EnvdBRange", ENV_DB_RANGE);
   
   mContourOffset = false;
   
   //   wxLogDebug("Y:%i Height:%i Offset:%i", y, height, mContourOffset );
   
   int len = mEnv.Count();
   for (int i = 0; i < len; i++) {
      if (mEnv[i]->t >= tleft && mEnv[i]->t <= tright) {
         
         int x = int ((mEnv[i]->t + mOffset - h) * pps) + r.x;
         int y[4];
         int numControlPoints;

         // Outer control points
         y[0] = GetWaveYPosNew( mEnv[i]->val, zoomMin, zoomMax, r.height,
                                dB, true, dBr, false);
         y[1] = GetWaveYPosNew( -mEnv[i]->val, zoomMin, zoomMax, r.height,
                                dB, true, dBr, false);

         // Inner control points(contour)
         y[2] = GetWaveYPosNew( mEnv[i]->val, zoomMin, zoomMax, r.height,
                                dB, false, dBr, false);
         y[3] = GetWaveYPosNew( -mEnv[i]->val-.00000001, zoomMin, zoomMax, 
                                r.height, dB, false, dBr, false);

         numControlPoints = 4;

         if (y[2] > y[3])
            numControlPoints = 2;

         if (!mMirror)
            numControlPoints = 1;
         
         for(int j=0; j<numControlPoints; j++){
            
            int d = (int)(sqrt((double)(SQR(x-event.m_x) + SQR(y[j]-(event.m_y-r.y)))) + 0.5);
            if (d < bestDist) {
               bestNum = i;
               bestDist = d;
               mContourOffset = (bool)(j > 1);
            }
         }
      }
   }

   if (bestNum >= 0) {
      mDragPoint = bestNum;
   }
   else {
      // Create new point
      double when = h + (event.m_x - r.x) / pps - mOffset;
      
      //      if (when <= 0 || when >= mTrackLen)
      //         return false;
      
      double v = GetValueAtX( event.m_x, r, h, pps );
      
      int ct = GetWaveYPosNew( v, zoomMin, zoomMax, r.height, dB, 
                               false, dBr, false) ;
      int cb = GetWaveYPosNew( -v-.000000001, zoomMin, zoomMax, r.height, dB, 
                               false, dBr, false) ;
      if( ct <= cb || !mMirror ){
         int t = GetWaveYPosNew( v, zoomMin, zoomMax, r.height, dB, 
                                 true, dBr, false) ;
         int b = GetWaveYPosNew( -v, zoomMin, zoomMax, r.height, dB, 
                                 true, dBr, false) ;
         
         ct = (t + ct) / 2;
         cb = (b + cb) / 2;
         
         if(mMirror &&
            (event.m_y - r.y) > ct &&
            ((event.m_y - r.y) < cb))
            mContourOffset = true;
         else
            mContourOffset = false;
      }
      
      double newVal = ValueOfPixel(clip_y, r.height, upper, dB,
                                   zoomMin, zoomMax);
      
      //float MaxAmplify = ( mContourOffset ) ? 1.4 : 1.0;
      float MaxAmplify = 2.0;
      
      if (newVal > MaxAmplify)
         newVal = MaxAmplify;
      
      mDragPoint = Insert(when, newVal);
      mDirty = true;
   }

   mUpper = upper;

   mInitialWhen = mEnv[mDragPoint]->t;
   mInitialVal = mEnv[mDragPoint]->val;

   mInitialX = event.m_x;
   mInitialY = event.m_y+mContourOffset;

   return true;
}

bool Envelope::HandleDragging( wxMouseEvent & event, wxRect & r,
                               double h, double pps, bool dB,
                               float zoomMin, float zoomMax )
{
   int clip_y = event.m_y - r.y;
   if(clip_y < 0) clip_y = 0;
   if(clip_y > r.height) clip_y = r.height;

   mDirty = true;

   wxRect larger = r;
   larger.Inflate(10, 10);

   if (!mIsDeleting &&
       !larger.Inside(event.m_x, event.m_y)){
      
      if( mEnv.Count()> 1) {
         
         if(mDragPoint > 0){
            mEnv[mDragPoint]->t = mEnv[mDragPoint - 1]->t;
            mEnv[mDragPoint]->val = mEnv[mDragPoint - 1]->val;
         }
         else{
            mEnv[mDragPoint]->t = mEnv[mDragPoint + 1]->t;
            mEnv[mDragPoint]->val = mEnv[mDragPoint + 1]->val;
         }
         
      }
      else {
         // temporary state when dragging only! 
         mEnv[mDragPoint]->t = -1000000.0;
         mEnv[mDragPoint]->val = mDefaultValue;
      }

      mIsDeleting = true;
      
      return true;
   }

   if (larger.Inside(event.m_x, event.m_y))
      mIsDeleting = false;

   if (mIsDeleting)
      return false;


   double newVal = ValueOfPixel(clip_y, r.height, mUpper, dB,
                                zoomMin, zoomMax);   

   //float MaxAmplify = ( mContourOffset ) ? 1.4 : 1.0;
   float MaxAmplify = 2.0;

   if (newVal > MaxAmplify)
      newVal = MaxAmplify;

   // We no longer tolerate multiple envelope points at the same t.
   // epsilon is less than the time offset of a single sample
   double newWhen = mInitialWhen + (event.m_x - mInitialX) / pps;

   if (mDragPoint > 0 && 
       newWhen - mTrackEpsilon < mEnv[mDragPoint - 1]->t)
      newWhen = mEnv[mDragPoint - 1]->t + mTrackEpsilon;
   
   if (mDragPoint < (int)mEnv.Count() - 1 && 
       newWhen + mTrackEpsilon > (double)mEnv[mDragPoint + 1]->t)
      newWhen = mEnv[mDragPoint + 1]->t - mTrackEpsilon;
   
   if(newWhen < 0.0)
      newWhen = 0.0;
   
   if(newWhen > mTrackLen)
      newWhen = mTrackLen;
   
   mEnv[mDragPoint]->t = newWhen;
   mEnv[mDragPoint]->val = newVal;

   return true;
}

bool Envelope::HandleMouseButtonUp( wxMouseEvent & event, wxRect & r,
                                    double h, double pps, bool dB,
                                    float zoomMin, float zoomMax )
{
   if (mIsDeleting) {
      delete mEnv[mDragPoint];
      mEnv.RemoveAt(mDragPoint);
   }
   mDragPoint = -1;
   return true;
}


// Returns true if parent needs to be redrawn
bool Envelope::MouseEvent(wxMouseEvent & event, wxRect & r,
                          double h, double pps, bool dB,
                          float zoomMin, float zoomMax)
{

   if (event.ButtonDown())
      return HandleMouseButtonDown( event, r, h, pps,dB,
                                    zoomMin, zoomMax);

   if (event.Dragging() && mDragPoint >= 0) 
      return HandleDragging( event, r, h, pps,dB,
                             zoomMin, zoomMax);

   if (event.ButtonUp()) 
      return HandleMouseButtonUp( event, r, h, pps, dB,
                                  zoomMin, zoomMax);

   return false;
}

void Envelope::CollapseRegion(double t0, double t1)
{
   // This gets called when somebody clears samples.  All of the
   // control points within the region disappear and the points
   // to the right get shifted over.

   if(t0 < mOffset)
      t0 = mOffset;

   if(t1 > mTrackLen + mOffset)
      t1 = mOffset + mTrackLen;

   t0 -= mOffset;
   t1 -= mOffset;

   int len = mEnv.Count();
   int i;

   for (i = 0; i < len - 0; i++)
      if (mEnv[i]->t >= t0 && mEnv[i]->t < t1) {
         delete mEnv[i];
         mEnv.RemoveAt(i);
         len--;
         i--;
      }
   
   for (i = 0; i < len; i++)
      if (mEnv[i]->t >= t1)
         mEnv[i]->t -= (t1 - t0);
   
   mTrackLen -= (t1-t0);
}

// This operation is trickier than it looks; the basic rub is that
// a track's envelope runs the range from t=0 to t=tracklen; the t=0
// envelope point applies to the first sample, but the t=tracklen
// envelope point applies one-past the last actual sample.
// Rather than going to a .5-offset-index, we special case the framing.

void Envelope::Paste(double t0, Envelope *e)
{
   t0 = wxMin(t0 - mOffset, mTrackLen);
   double deltat = e->mTrackLen;

   unsigned int i;
   unsigned int len = mEnv.Count();

   // get values to perform framing of the insertion 
   double splitval = GetValue(t0 + mOffset);
   double leftval  = e->GetValue(0+e->mOffset);
   double rightval = e->GetValue(e->mTrackLen+e->mOffset);
   int pos = 0;


   // Shift existing points to the right
   for (i = 0; i < len; i++)
      if (mEnv[i]->t > t0)
         mEnv[i]->t += deltat;
   
   // unless insert point is end of track, we need to frame the right
   // boundary.  Do it before modifying mTrackLen.
   len = e->mEnv.Count();
   if(t0  < mTrackLen) {
      mTrackLen += deltat;
      // was the last inserted controlpoint at the end of insertion?
      if(len && e->mEnv[len-1]->t == e->mTrackLen){
         // it both must be shifted and functions as the framing point
	 if (mEnv.IsEmpty())
	    {   //we have an issue - the destination track has no points in it
	    }
	 else
	    {	// we do this bit normally
            mEnv[pos]->t-=mTrackEpsilon;
	    }
      }
      else {
         Insert(t0 + e->mTrackLen - mTrackEpsilon, rightval);
      }

      Insert(t0 + e->mTrackLen, splitval);
   }
   else {
      mTrackLen += deltat;
   }

   // unless t0 = 0.0, we need to frame the left boundary
   if(t0 > 0) {
      // shift the last point before the cut left by epsilon 
      Insert(t0 - mTrackEpsilon, splitval);
      Insert(t0, leftval); // if this duplicates the first point below,
                           // it will be replaced
   }

   // Copy points from inside the selection
   for (i = 0; i < len; i++)
      pos=Insert(t0 + e->mEnv[i]->t, e->mEnv[i]->val);
}

void Envelope::InsertSpace(double t0, double tlen)
{
   unsigned int len = mEnv.Count();
   unsigned int i;

   for (i = 0; i < len; i++)
      if (mEnv[i]->t > t0)
         mEnv[i]->t += tlen;
   mTrackLen += tlen;
}

int Envelope::Move(double when, double value)
{
   int len = mEnv.Count();
   if (len == 0)
      return -1;

   int i = 0;
   while (i < len && when > mEnv[i]->t)
      i++;

   if (i >= len || when < mEnv[i]->t)
      return -1;

   mEnv[i]->val = value;
   return 0;
}


int Envelope::GetNumberOfPoints() const
{
   return mEnv.Count();
}

void Envelope::GetPoints(double *bufferWhen,
                         double *bufferValue,
                         int bufferLen) const
{
   int n = mEnv.Count();
   if (n > bufferLen)
      n = bufferLen;
   int i;
   for (i = 0; i < n; i++) {
      bufferWhen[i] = mEnv[i]->t;
      bufferValue[i] = mEnv[i]->val;
   }
}

// Private methods

// We no longer tolerate multiple envelope control points at the exact
// same t; the behavior can be well-defined, but it is still incorrect
// in that it vastly complicates paste operations behaving as a user
// reasonably expects.  The most common problem occurs pasting an
// envelope into another track; the boundary behavior causes the
// t=insert_point envelope level of the insertee to apply to sample 0
// of the inserted sample, causing a pop.  This most visibly manifests
// itself in undo and mixing when a v=1.0 sample magically shows
// up at boundaries causing a pop.

// Although this renders the name a slight misnomer, a duplicate
// 'replaces' the current control point.

int Envelope::Insert(double when, double value)
{
   int len = mEnv.Count();

   if (len && when < 0.0)
      return 0;
   if ((len > 1) && when > mTrackLen)
      return len - 1;

   if (when < 0.0)
      when = 0.0;
   if ((len>1) && when > mTrackLen)
      when = mTrackLen;

   int i = 0;
   
   while (i < len && when > mEnv[i]->t)
      i++;

   if(i < len && when == mEnv[i]->t) {

     // modify existing
     mEnv[i]->val = value;

   }
   else{

     // Add new
     EnvPoint *e = new EnvPoint();
     e->t = when;
     e->val = value;
      if (i < len) {
         mEnv.Insert(e, i);
      } else {
         mEnv.Add(e);
      }
   }
   return i;
}

// Control

void Envelope::SetOffset(double newOffset)
{
   mOffset = newOffset;
}

void Envelope::SetTrackLen(double trackLen)
{
   mTrackLen = trackLen;

   int len = mEnv.Count();
   for (int i = 0; i < len; i++)
      if (mEnv[i]->t > mTrackLen) {
         delete mEnv[i];
         mEnv.RemoveAt(i);
         len--;
      }
}

// Accessors
double Envelope::GetValue(double t) const
{
   double temp;

   GetValues(&temp, 1, t, 1.0);
   return temp;
}

// 'X' is in pixels and relative to track.
double Envelope::GetValueAtX( int x, wxRect & r, double h, double pps )
{
   // Convert x to time.
   double t = (x - r.x) / pps + h ;//-mOffset;
   return GetValue( t );
}


void Envelope::GetValues(double *buffer, int bufferLen,
                         double t0, double tstep) const
{
   t0 -= mOffset;

   int len = mEnv.Count();

   double t = t0;

   double tprev, vprev, tnext = 0, vnext, vstep = 0;

   for (int b = 0; b < bufferLen; b++) {

      if (len <= 0) {
	 buffer[b] = mDefaultValue;
	 t += tstep;
	 continue;
      }
      if (t <= mEnv[0]->t) {
         buffer[b] = mEnv[0]->val;
         t += tstep;
         continue;
      }
      if (t >= mEnv[len - 1]->t) {
         buffer[b] = mEnv[len - 1]->val;
         t += tstep;
         continue;
      }

      if (b == 0 || t > tnext) {

         // binary search
         int lo = 0;
         int hi = len - 1;
         while (hi > (lo + 1)) {
            int mid = (lo + hi) / 2;
            if (t < mEnv[mid]->t)
               hi = mid;
            else
               lo = mid;
         }

         tprev = mEnv[lo]->t;
         tnext = mEnv[hi]->t;

         if (mDB) {
            vprev = log10(mEnv[lo]->val);
            vnext = log10(mEnv[hi]->val);

            // Special case for the log of zero
            if (mEnv[lo]->val <= 0.0)
               vprev = -7;       // This corresponds to -140 dB
            if (mEnv[hi]->val <= 0.0)
               vnext = -7;
       } else {
            vprev = mEnv[lo]->val;
            vnext = mEnv[hi]->val;
         }

         // Interpolate

         double dt = (tnext - tprev);

         double to = t - tprev;
         double v;
         if (dt > 0.0)
            v = (vprev * (dt - to) + vnext * to) / dt;
         else
            v = tnext;

         if (mDB) {
            buffer[b] = pow(10.0, v);
            if (dt > 0.0)
               vstep = pow(10.0, (vnext - vprev) * tstep / dt);
            else
               vstep = 1.0;
         }
         else {
            buffer[b] = v;
            if (dt > 0.0)
               vstep = (vnext - vprev) * tstep / dt;
            else
               vstep = 0.0;
         }
      } else {
	if (mDB){
            buffer[b] = buffer[b - 1] * vstep;
         }else{
            buffer[b] = buffer[b - 1] + vstep;
      }
      }

      t += tstep;
   }
}

int Envelope::NumberOfPointsAfter(double t)
{
   if( t >= mEnv[mEnv.Count()-1]->t )
      return 0;
   else if( t < mEnv[0]->t )
      return mEnv.Count();
   else
      {
         int lo = 0;
         int hi = mEnv.Count() - 1;
         while (hi > (lo + 1))
            {
               int mid = (lo + hi) / 2;
               if (t < mEnv[mid]->t)
                  hi = mid;
               else
                  lo = mid;
            }
         if( mEnv[hi]->t == t )
            return mEnv.Count() - (hi+1);
         else
            return mEnv.Count() - hi;
      }
}

double Envelope::NextPointAfter(double t)
{
   if( mEnv[mEnv.Count()-1]->t < t )
      return t;
   else if( t < mEnv[0]->t )
      return mEnv[0]->t;
   else
      {
         int lo = 0;
         int hi = mEnv.Count() - 1;
         while (hi > (lo + 1))
            {
               int mid = (lo + hi) / 2;
               if (t < mEnv[mid]->t)
                  hi = mid;
               else
                  lo = mid;
            }
         if( mEnv[hi]->t == t )
            return mEnv[hi+1]->t;
         else
            return mEnv[hi]->t;
      }
}

double Envelope::Average( double t0, double t1 )
{
  if( t0 == t1 )
    return GetValue( t0 );
  else
    return Integral( t0, t1 ) / (t1 - t0);
}

//
// Integration and debugging functions
//
// The functions below are used by the TimeTrack and possibly for
// other debugging.  They do not affect normal amplitude envelopes
// for waveforms, not frequency envelopes for the FFT Filter or
// equalization.
//

// We should be able to write a very efficient memoizer for this
// but make sure it gets reset when the envelope is changed.
double Envelope::Integral( double t0, double t1 )
{
   //printf( "\n\nIntegral:  t0=%f, t1=%f\n", t0, t1 );
   double total=0;
   
   if( t0 == t1 )
      return 0;
   if( t0 > t1 )
      {
         printf( "Odd things happening in Integral!\n" );
         return mDefaultValue;
      }
   
   unsigned int i = 0;
   double lastT, lastVal;
   
   // t0 is one of three cases:

   // 0) in an 'empty' envelope
   // 1) preceeding the first point
   // 2) enclosed by points
   // 3) following the last point

   
   if( mEnv.Count() < 1 )                      // 0: 'empty' envelope
      {
       return (t1 - t0) * mDefaultValue;  
     }

   else if( t0 < mEnv[0]->t )                  // 1: preceeds the first
      {
	if( t1 <= mEnv[0]->t ){
	  return (t1 - t0) * mEnv[0]->val;
	}
	total += (mEnv[0]->t - t0) * mEnv[0]->val;
         lastT = mEnv[0]->t;
         lastVal = mEnv[0]->val;
      }

   else if( t0 >= mEnv[mEnv.Count()-1]->t )    // 3: follows the last
      {
	return (t1 - t0) * mEnv[mEnv.Count()-1]->val;
      }

   else 
     {                                         // 2: bracketed
         // Skip any points that come before t0 using binary search
         int lo = 0;
         int hi = mEnv.Count() - 1;
         while (hi > (lo + 1)) {
            int mid = (lo + hi) / 2;
            if (t0 < mEnv[mid]->t)
               hi = mid;
            else
               lo = mid;
         }
         i = lo;
         
         // i is now the point immediately before t0.
         lastVal = ((mEnv[i]->val * (mEnv[i+1]->t - t0))
                    + (mEnv[i+1]->val *(t0 - mEnv[i]->t)))
            / (mEnv[i+1]->t - mEnv[i]->t); // value at t0
         lastT = t0;
      }
   
   // loop through the rest of the envelope points until we get to t1
   while (1)
      {

	if(i >= mEnv.Count()-1)
      {
	    // the requested range extends beyond last point
	    return total + (t1 - lastT) * lastVal;
	  }
	else if (mEnv[i+1]->t >= t1)
	  {
	    // last,i+1 bracket t1
         double thisVal = ((mEnv[i]->val * (mEnv[i+1]->t - t1))
                           + (mEnv[i+1]->val *(t1 - mEnv[i]->t)))
	      / (mEnv[i+1]->t - mEnv[i]->t); 
   
	    return total + (t1 - lastT) * (thisVal + lastVal) / 2;
	  }
	else
      {
	    // t1 still follows last,i+1
	    total += (mEnv[i+1]->t - lastT) *  (mEnv[i+1]->val + lastVal) / 2;
         lastT = mEnv[i+1]->t;
         lastVal = mEnv[i+1]->val;
         i++;
      }
      }
}

// This one scales the y-axis before integrating.
// To re-scale [0,1] to [minY,maxY] we use the mapping y -> minY + (maxY - minY)y
// So we want to find the integral of (minY + (maxY - minY)f(t)), where f is our envelope.
// But that's just (t1 - t0)minY + (maxY - minY)Integral( t0, t1 ).
double Envelope::Integral( double t0, double t1, double minY, double maxY )
{
   return ((t1 - t0) * minY) + ((maxY - minY) * Integral( t0, t1 ));
}

void Envelope::print()
{
   for( unsigned int i = 0; i < mEnv.Count(); i++ )
      printf( "(%.2f, %.2f)\n", mEnv[i]->t, mEnv[i]->val );
}

void checkResult( int n, double a, double b )
{
   if( (a-b > 0 ? a-b : b-a) > 0.0000001 )
   {
      printf( "Envelope:  Result #%d is: %f, should be %f\n", n, a, b );
      //exit( -1 );
   }
}

void Envelope::testMe()
{
   double t0=0, t1=0;

   SetInterpolateDB(false);
   Mirror(false);

   SetDefaultValue(0.5);
   Flatten(0.5);
   checkResult( 1, Integral(0.0,100.0), 50);
   checkResult( 2, Integral(-10.0,10.0), 10);

   SetDefaultValue(1.0);
   Flatten(0.5);
   checkResult( 3, Integral(0.0,100.0), 50);
   checkResult( 4, Integral(-10.0,10.0), 10);
   checkResult( 5, Integral(-20.0,-10.0), 5);

   SetDefaultValue(0.5);
   Flatten(0.5);
   Insert( 5.0, 0.5 );
   checkResult( 6, Integral(0.0,100.0), 50);
   checkResult( 7, Integral(-10.0,10.0), 10);

   SetDefaultValue(0.5);
   Flatten(0.0);
   Insert( 0.0, 0.0 );
   Insert( 5.0, 1.0 );
   Insert( 10.0, 0.0 );
   t0 = 10.0 - .1;
   t1 = 10.0 + .1;
   double result = Integral(0.0,t1);
   double resulta = Integral(0.0,t0);
   double resultb = Integral(t0,t1);
   // Integrals should be additive
   checkResult( 8, result - resulta - resultb, 0);

   SetDefaultValue(0.5);
   Flatten(0.0);
   Insert( 0.0, 0.0 );
   Insert( 5.0, 1.0 );
   Insert( 10.0, 0.0 );
   t0 = 10.0 - .1;
   t1 = 10.0 + .1;
   checkResult( 9, Integral(0.0,t1), 5);
   checkResult( 10, Integral(0.0,t0), 4.999);
   checkResult( 11, Integral(t0,t1), .001);

   WX_CLEAR_ARRAY(mEnv);
   Insert( 0.0, 0.0 );
   Insert( 5.0, 1.0 );
   Insert( 10.0, 0.0 );
   checkResult( 12, NumberOfPointsAfter( -1 ), 3 );
   checkResult( 13, NumberOfPointsAfter( 0 ), 2 );
   checkResult( 14, NumberOfPointsAfter( 1 ), 2 );
   checkResult( 15, NumberOfPointsAfter( 5 ), 1 );
   checkResult( 16, NumberOfPointsAfter( 7 ), 1 );
   checkResult( 17, NumberOfPointsAfter( 10 ), 0 );
   checkResult( 18, NextPointAfter( 0 ), 5 );
   checkResult( 19, NextPointAfter( 5 ), 10 );
}
