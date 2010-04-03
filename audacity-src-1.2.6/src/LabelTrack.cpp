/**********************************************************************

  Audacity: A Digital Audio Editor

  LabelTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/brush.h>
#include <wx/dc.h>
#include <wx/event.h>
#include <wx/pen.h>
#include <wx/string.h>
#include <wx/textfile.h>
#include <wx/intl.h>

#include "AColor.h"
#include "LabelTrack.h"
#include "DirManager.h"
#include "Internat.h"
#include "Prefs.h"

wxFont LabelTrack::msFont;

LabelTrack *TrackFactory::NewLabelTrack()
{
   return new LabelTrack(mDirManager);
}

LabelTrack::LabelTrack(DirManager * projDirManager):
Track(projDirManager)
{
   SetName(_("Label Track"));

   ResetFont();

   mHeight = 30;     // Label tracks are narrow

   mSelIndex = -1;
}

LabelTrack::LabelTrack(const LabelTrack &orig) :
Track(orig)
{
   int len = orig.mLabels.Count();

   for (int i = 0; i < len; i++) {
      LabelStruct *l = new LabelStruct();
      l->t = orig.mLabels[i]->t;
      l->t1 = orig.mLabels[i]->t1;
      l->title = orig.mLabels[i]->title;
      mLabels.Add(l);
   }

   mSelIndex = orig.mSelIndex;
}

LabelTrack::~LabelTrack()
{
   int len = mLabels.Count();

   for (int i = 0; i < len; i++)
      delete mLabels[i];
}

void LabelTrack::ResetFont()
{
   wxString facename = gPrefs->Read("/GUI/LabelFontFacename", "");
   if (facename != "") {
      msFont = wxFont(12, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL,
                      wxFONTWEIGHT_NORMAL, FALSE, facename,
                      wxFONTENCODING_SYSTEM);
   }
}

void LabelTrack::Draw(wxDC & dc, wxRect & r, double h, double pps,
                      double sel0, double sel1)
{
   if (msFont.Ok())
      dc.SetFont(msFont);

   double right = h + r.width / pps;
   double dsel0 = sel0;
   if (dsel0 < h)
      dsel0 = h;
   if (dsel0 > right)
      dsel0 = right;
   double dsel1 = sel1;
   if (dsel1 < h)
      dsel1 = h;
   if (dsel1 > right)
      dsel1 = right;

   wxRect before = r;
   before.width = int ((dsel0 - h) * pps);
   dc.SetBrush(AColor::labelUnselectedBrush);
   dc.SetPen(AColor::labelUnselectedPen);
   dc.DrawRectangle(before);

   wxRect selr = r;
   selr.x += before.width;
   selr.width = int ((dsel1 - dsel0) * pps);
   dc.SetBrush(AColor::labelSelectedBrush);
   dc.SetPen(AColor::labelSelectedPen);
   dc.DrawRectangle(selr);

   wxRect after = r;
   after.x += (before.width + selr.width);
   after.width -= (before.width + selr.width);
   if (after.x < r.x) {
      after.width -= (r.x - after.x);
      after.x = r.x;
   }
   dc.SetBrush(AColor::labelUnselectedBrush);
   dc.SetPen(AColor::labelUnselectedPen);
   dc.DrawRectangle(after);

   dc.SetBrush(AColor::labelFlagBrush);
   dc.SetPen(AColor::labelFlagPen);

   int nextx = 0;

   for (int i = 0; i < (int)mLabels.Count(); i++) {

      int x = r.x + (int) ((mLabels[i]->t - h) * pps);
      int y = r.y;
      int height = r.height;

      if (x >= r.x && x < r.x + r.width) {

         if (x < nextx && i != mSelIndex) {
            // Draw flag obscured by the previous label

            dc.DrawLine(x, y, x, y + 2);
            dc.DrawLine(x, y + height - 2, x, y + height);

            mLabels[i]->width = 0;
         } else {
            // Draw the flag and label

            wxPoint tri[3];
            tri[0].x = x;
            tri[0].y = y;
            tri[1].x = x - 8;
            tri[1].y = y + 4;
            tri[2].x = x;
            tri[2].y = y + 8;

            if (mSelIndex == i)
               dc.SetBrush(*wxWHITE_BRUSH);
            dc.DrawPolygon(3, tri);
            if (mSelIndex == i)
               dc.SetBrush(AColor::labelFlagBrush);

            dc.DrawLine(x, y, x, y + height);

            dc.SetTextForeground(wxColour(204, 0, 0));

#ifdef __WXMAC__
            long textWidth, textHeight;
#else
            int textWidth, textHeight;
#endif

            dc.GetTextExtent(mLabels[i]->title, &textWidth, &textHeight);
            dc.DrawText(mLabels[i]->title, x + 4, y + 4);

            if (i == mSelIndex) {
               wxRect outline;

               outline.x = x + 2;
               outline.y = y + 2;
               outline.width = textWidth + 4;
               outline.height = height - 4;

               dc.SetBrush(*wxTRANSPARENT_BRUSH);
               dc.DrawRectangle(outline);
               dc.SetBrush(AColor::labelFlagBrush);
            }

            mLabels[i]->width = textWidth + 8;

            nextx = x + textWidth + 8;
         }

      }
   }
}

double LabelTrack::GetStartTime()
{
   int len = mLabels.Count();
   
   if (len == 0)
      return 0.0;
   else
      return mLabels[0]->t;
}

double LabelTrack::GetEndTime()
{
   int len = mLabels.Count();

   if (len == 0)
      return 0.0;
   else
      return mLabels[len - 1]->t;
}

void LabelTrack::MouseDown(int x, int y, wxRect & r, double h, double pps,
                           double *newSel0, double *newSel1)
{
   double mouseH = h + (x - r.x) / pps;

   for (int i = 0; i < (int)mLabels.Count(); i++) {
      if (mLabels[i]->t - (8 / pps) < mouseH &&
          mouseH < mLabels[i]->t + (mLabels[i]->width / pps)) {
         mSelIndex = i;
         *newSel0 = mLabels[i]->t;
         *newSel1 = mLabels[i]->t1;
         return;
      }
   }

   mSelIndex = -1;
}

#ifdef __WXMAC__
 #if ((wxMAJOR_VERSION == 2) && (wxMINOR_VERSION <= 4))
  // HACK: Workaround wxMac <=2.4 bug
  bool gMacRepeat = false;
 #endif
#endif

void LabelTrack::KeyEvent(double sel0, double sel1, wxKeyEvent & event)
{ 
#ifdef __WXMAC__
 #if ((wxMAJOR_VERSION == 2) && (wxMINOR_VERSION <= 4))
  // HACK: Workaround wxMac <=2.4 bug
   gMacRepeat = !gMacRepeat;
   if (gMacRepeat)
      return;
 #endif
#endif

   long keyCode = event.KeyCode();

   if (mSelIndex >= 0) {
      switch (keyCode) {
      case WXK_BACK:{
            int len = mLabels[mSelIndex]->title.Length();
            if (len > 0)
               mLabels[mSelIndex]->title =
                   mLabels[mSelIndex]->title.Left(len - 1);
         }
         break;

      case WXK_RETURN:
      case WXK_ESCAPE:
         if (mLabels[mSelIndex]->title == "") {
            delete mLabels[mSelIndex];
            mLabels.RemoveAt(mSelIndex);
         }
         mSelIndex = -1;
         break;

      case WXK_TAB:
         if (event.ShiftDown()) {
            if (mSelIndex > 0)
               mSelIndex--;
         } else {
            if (mSelIndex < (int)mLabels.Count() - 1)
               mSelIndex++;
         }
         break;

      case WXK_DELETE:
         break;

      default:
         if (keyCode > 31 && keyCode < 127)
            mLabels[mSelIndex]->title += keyCode;
         break;
      }
   } else if (keyCode > 31 && keyCode < 127) {
      // Create new label

      LabelStruct *l = new LabelStruct();
      l->t = sel0;
      l->t1 = sel1;
      l->title += wxChar(keyCode);

      int len = mLabels.Count();
      int pos = 0;

      while (pos < len && l->t > mLabels[pos]->t)
         pos++;

      mLabels.Insert(l, pos);

      mSelIndex = pos;
   }
}

void LabelTrack::Unselect()
{
   mSelIndex = -1;
}

bool LabelTrack::IsSelected() const
{
   return (mSelIndex >= 0 && mSelIndex < (int)mLabels.Count());
}

// TODO: Make Export include label end-times (LabelStruct::t1). 
// Make Import handle files with or without end-times.

void LabelTrack::Export(wxTextFile & f)
{
   for (int i = 0; i < (int)mLabels.Count(); i++) {
      f.AddLine(wxString::Format("%f\t%s",
                                 (double)mLabels[i]->t,
                                 (const char *) (mLabels[i]->title)));
   }
}

void LabelTrack::Import(wxTextFile & in)
{
   wxString currentLine;
   int i, len;
   int index, lines;
   wxString s;
   wxString title;
   double t;

   lines = in.GetLineCount();

   mLabels.Clear();
   mLabels.Alloc(lines);

   for (index = 0; index < lines; index++) {
      currentLine = in.GetLine(index);

      len = currentLine.Length();
      if (len == 0)
         return;

      i = 0;
      while (i < len && currentLine.GetChar(i) != ' '
             && currentLine.GetChar(i) != '\t')
         i++;

      s = currentLine.Left(i);
      if (!Internat::CompatibleToDouble(s, &t))
         return;

      while (i < len
             && (currentLine.GetChar(i) == ' '
                 || currentLine.GetChar(i) == '\t'))
         i++;

      title = currentLine.Right(len - i);

      LabelStruct *l = new LabelStruct();
      l->t = t;
      l->t1 = t;
      l->title = title;
      mLabels.Add(l);
   }
}

bool LabelTrack::HandleXMLTag(const char *tag, const char **attrs)
{
   if (!strcmp(tag, "label")) {

      LabelStruct *l = new LabelStruct();

      // loop through attrs, which is a null-terminated list of
      // attribute-value pairs
      bool has_t1 = false;
      while(*attrs) {
         const char *attr = *attrs++;
         const char *value = *attrs++;
         
         if (!value)
            break;
         
         if (!strcmp(attr, "t"))
            Internat::CompatibleToDouble(wxString(value), &l->t);
         else if (!strcmp(attr, "t1")) {
            has_t1 = true;
            Internat::CompatibleToDouble(wxString(value), &l->t1);
         }
         else if (!strcmp(attr, "title"))
            l->title = value;

      } // while

      // Handle files created by Audacity 1.1.   Labels in Audacity 1.1
      // did not have separate start- and end-times.
      if (!has_t1)
         l->t1 = l->t;

      mLabels.Add(l);

      return true;
   }
   else if (!strcmp(tag, "labeltrack")) {
      if (*attrs) {
         const char *attr = *attrs++;
         const char *value = *attrs++;
         
         if (!value)
            return true;

         if (!strcmp(attr, "name"))
            mName = value;
         else if (!strcmp(attr, "numlabels")) {
            int len = atoi(value);
            mLabels.Clear();
            mLabels.Alloc(len);
         }
      }

      return true;
   }

   return false;
}

XMLTagHandler *LabelTrack::HandleXMLChild(const char *tag)
{
   if (!strcmp(tag, "label"))
      return this;
   else
      return NULL;
}

void LabelTrack::WriteXML(int depth, FILE *fp)
{
   int len = mLabels.Count();
   int i, j;

   for(j=0; j<depth; j++)
      fprintf(fp, "\t");
   fprintf(fp, "<labeltrack ");
   fprintf(fp, "name=\"%s\" ", XMLEsc(mName).c_str());
   fprintf(fp, "numlabels=\"%d\">\n", len);

   for (i = 0; i < len; i++) {
      for(j=0; j<depth+1; j++)
         fprintf(fp, "\t");
      fprintf(fp, "<label t=\"%s\" t1=\"%s\" title=\"%s\"/>\n",
            Internat::ToString(mLabels[i]->t, 8).c_str(),
            Internat::ToString(mLabels[i]->t1, 8).c_str(),
            XMLEsc(mLabels[i]->title).c_str());
   }
   for(j=0; j<depth; j++)
      fprintf(fp, "\t");
   fprintf(fp, "</labeltrack>\n");
}

#if LEGACY_PROJECT_FILE_SUPPORT
bool LabelTrack::Load(wxTextFile * in, DirManager * dirManager)
{
   if (in->GetNextLine() != "NumMLabels")
      return false;

   unsigned long len;
   if (!(in->GetNextLine().ToULong(&len)))
      return false;

   unsigned int i;
   for (i = 0; i < mLabels.Count(); i++)
      delete mLabels[i];
   mLabels.Clear();
   mLabels.Alloc(len);

   for (i = 0; i < len; i++) {
      LabelStruct *l = new LabelStruct();
      if (!Internat::CompatibleToDouble(in->GetNextLine(), &l->t))
         return false;
      // Legacy file format does not include label end-times.
      l->t1 = l->t;
      l->title = in->GetNextLine();
      mLabels.Add(l);
   }

   if (in->GetNextLine() != "MLabelsEnd")
      return false;

   return true;
}

bool LabelTrack::Save(wxTextFile * out, bool overwrite)
{
   out->AddLine("NumMLabels");
   int len = mLabels.Count();
   out->AddLine(wxString::Format("%d", len));

   for (int i = 0; i < len; i++) {
      out->AddLine(wxString::Format("%lf", mLabels[i]->t));
      out->AddLine(mLabels[i]->title);
   }
   out->AddLine("MLabelsEnd");

   return true;
}
#endif

bool LabelTrack::Cut(double t0, double t1, Track ** dest)
{
   *dest = new LabelTrack(GetDirManager());
   int len = mLabels.Count();

   for (int i = 0; i < len; i++) {
      if (t0 <= mLabels[i]->t && mLabels[i]->t <= t1) {
         mLabels[i]->t -= t0;
         mLabels[i]->t1 -= t0;
         ((LabelTrack *) (*dest))->mLabels.Add(mLabels[i]);
         mLabels.RemoveAt(i);
         len--;
         i--;
      }
      else if (mLabels[i]->t > t1) {
         mLabels[i]->t -= (t1 - t0);
         mLabels[i]->t1 -= (t1 - t0);
      }
   }
   ((LabelTrack *) (*dest))->mClipLen = (t1 - t0);

   return true;
}

bool LabelTrack::Copy(double t0, double t1, Track ** dest) const
{
   *dest = new LabelTrack(GetDirManager());
   int len = mLabels.Count();

   for (int i = 0; i < len; i++) {
      if (t0 <= mLabels[i]->t && mLabels[i]->t <= t1) {
         LabelStruct *l = new LabelStruct();
         l->t = mLabels[i]->t - t0;
         l->t1 = mLabels[i]->t1 - t0;
         l->title = mLabels[i]->title;
         ((LabelTrack *) (*dest))->mLabels.Add(l);
      }
   }
   ((LabelTrack *) (*dest))->mClipLen = (t1 - t0);

   return true;
}

bool LabelTrack::Paste(double t, const Track * src)
{
   if (src->GetKind() != Track::Label)
      return false;

   int len = mLabels.Count();
   int pos = 0;

   while (pos < len && mLabels[pos]->t < t)
      pos++;

   LabelTrack *sl = (LabelTrack *) src;
   for (unsigned int j = 0; j < sl->mLabels.Count(); j++) {
      LabelStruct *l = new LabelStruct();
      l->t = sl->mLabels[j]->t + t;
      l->t1 = sl->mLabels[j]->t1 + t;
      l->title = sl->mLabels[j]->title;
      mLabels.Insert(l, pos++);
      len++;
   }

   while (pos < len) {
      mLabels[pos]->t += sl->mClipLen;
      mLabels[pos]->t1 += sl->mClipLen;
      pos++;
   }

   return true;
}

bool LabelTrack::Clear(double t0, double t1)
{
   int len = mLabels.Count();

   for (int i = 0; i < len; i++) {
      if (t0 <= mLabels[i]->t && mLabels[i]->t <= t1) {
         mLabels.RemoveAt(i);
         len--;
         i--;
      }
      else if (mLabels[i]->t > t1) {
         mLabels[i]->t -= (t1 - t0);
         mLabels[i]->t1 -= (t1 - t0);
      }
   }

   return true;
}

bool LabelTrack::Silence(double t0, double t1)
{
   int len = mLabels.Count();

   for (int i = 0; i < len; i++) {
      if (t0 <= mLabels[i]->t && mLabels[i]->t <= t1) {
         mLabels.RemoveAt(i);
         len--;
         i--;
      }
   }

   return true;
}

bool LabelTrack::InsertSilence(double t, double len)
{
   int numLabels = mLabels.Count();

   for (int i = 0; i < numLabels; i++) {
      if (mLabels[i]->t >= t)
         mLabels[i]->t += len;

      if (mLabels[i]->t1 >= t)
         mLabels[i]->t1 += len;
   }

   return true;
}

int LabelTrack::GetNumLabels() const
{
   return mLabels.Count();
}

const LabelStruct *LabelTrack::GetLabel(int index) const
{
   return mLabels[index];
}

void LabelTrack::AddLabel(double t, double t1, const wxString &title)
{
   LabelStruct *l = new LabelStruct();
   l->t = t;
   l->t1 = t1;
   l->title = title;

   int len = mLabels.Count();
   int pos = 0;

   while (pos < len && mLabels[pos]->t < t)
      pos++;

   mLabels.Insert(l, pos);

   mSelIndex = pos;
}
