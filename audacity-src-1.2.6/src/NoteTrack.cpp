/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/dc.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/intl.h>

#include "allegro.h"

#include "AColor.h"
#include "NoteTrack.h"
#include "DirManager.h"

NoteTrack *TrackFactory::NewNoteTrack()
{
   return new NoteTrack(mDirManager);
}

NoteTrack::NoteTrack(DirManager * projDirManager):
Track(projDirManager)
{
   SetName(_("Note Track"));

   mSeq = NULL;
   mLen = 0.0;

   mDirManager = projDirManager;

   mBottomNote = 24;

   mVisibleChannels = 0xFFFF;
}

Track *NoteTrack::Duplicate()
{
   return new NoteTrack(mDirManager);
}

void NoteTrack::DrawLabelControls(wxDC & dc, wxRect & r)
{
   int wid = 23;
   int ht = 16;

   if (r.height < ht * 4)
      return;

   int x = r.x + r.width / 2 - wid * 2;
   int y = r.y + 4;

   for (int row = 0; row < 4; row++)
      for (int col = 0; col < 4; col++) {
         int channel = row * 4 + col + 1;

         wxRect box;
         box.x = x + col * wid;
         box.y = y + row * ht;
         box.width = wid;
         box.height = ht;

         if (mVisibleChannels & (1 << (channel - 1))) {
            AColor::MIDIChannel(&dc, channel);
            dc.DrawRectangle(box);

            AColor::LightMIDIChannel(&dc, channel);
            dc.DrawLine(box.x, box.y, box.x + box.width - 1, box.y);
            dc.DrawLine(box.x, box.y, box.x, box.y + box.height - 1);

            AColor::DarkMIDIChannel(&dc, channel);
            dc.DrawLine(box.x + box.width - 1, box.y,
                        box.x + box.width - 1, box.y + box.height);
            dc.DrawLine(box.x, box.y + box.height - 1, box.x + box.width,
                        box.y + box.height - 1);
         } else {
            AColor::MIDIChannel(&dc, 0);
            dc.DrawRectangle(box);
         }

         dc.DrawText(wxString::Format("%d", channel), box.x + 5,
                     box.y + 3);
      }

}

bool NoteTrack::LabelClick(wxRect & r, int mx, int my, bool right)
{
   int wid = 23;
   int ht = 16;

   if (r.height < ht * 4)
      return false;

   int x = r.x + r.width / 2 - wid * 2;
   int y = r.y + 4;

   int col = (mx - x) / wid;
   int row = (my - y) / ht;

   if (row < 0 || row >= 4 || col < 0 || col >= 4)
      return false;

   int channel = row * 4 + col;

   if (right) {
      if (mVisibleChannels == (1 << channel))
         mVisibleChannels = 0xFFFF;
      else
         mVisibleChannels = (1 << channel);
   } else
      mVisibleChannels ^= (1 << channel);

   return true;
}

void NoteTrack::CalcLen()
{
   int numEvents = mSeq->notes.len;

   if (numEvents <= 0)
      mLen = 0.0;
   else {
      mLen = 0.0;
      for (int i = 0; i < numEvents; i++) {
         if (mSeq->notes[numEvents - 1]->time > mLen)
            mLen = mSeq->notes[numEvents - 1]->time;
         if (mSeq->notes[numEvents - 1]->type == 'n') {
            double endtime = mSeq->notes[numEvents - 1]->time +
                ((Allegro_note_ptr) mSeq->notes[numEvents - 1])->dur;
            if (endtime > mLen)
               mLen = endtime;
         }
      }
   }
}

void NoteTrack::SetSequence(Seq *seq)
{
   if (mSeq)
      delete mSeq;

   mSeq = seq;

   CalcLen();
}

bool NoteTrack::HandleXMLTag(const char *tag, const char **attrs)
{
   return false;
}

XMLTagHandler *NoteTrack::HandleXMLChild(const char *tag)
{
   return NULL;
}

void NoteTrack::WriteXML(int depth, FILE *fp)
{
}
