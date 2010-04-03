/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackArtist.h

  Dominic Mazzoni

  This singleton class handles the actual rendering of WaveTracks
  (both waveforms and spectra), NoteTracks, and LabelTracks.

  It's actually a little harder than it looks, because for
  waveforms at least it needs to cache the samples that are
  currently on-screen.

**********************************************************************/

#ifndef __AUDACITY_TRACKARTIST__
#define __AUDACITY_TRACKARTIST__

#include <wx/brush.h>
#include <wx/pen.h>

class wxDC;
class wxRect;
class wxHashTable;

class Track;
class WaveTrack;
class NoteTrack;
class LabelTrack;
class TimeTrack;
class TrackList;
class Ruler;
struct ViewInfo;

#ifndef uchar
typedef unsigned char uchar;
#endif

class TrackArtist {

 public:
   TrackArtist();
   ~TrackArtist();

   void DrawTracks(TrackList * tracks,
                   wxDC & dc, wxRect & r,
                   wxRect & clip, ViewInfo * viewInfo, 
                   bool drawEnvelope,bool drawSamples,bool drawSliders);

   void DrawVRuler(Track * t, wxDC * dc, wxRect & r);
   int GetWaveYPos(float value, int height, bool dB, float dBr);
   int GetWaveYPosUnclipped(float value, int height, bool dB, float dBr);

   void SetInset(int left, int top, int right, int bottom);

   //
   // Lower-level drawing functions
   //

   void DrawWaveform(WaveTrack *track,
                     wxDC & dc, wxRect & r,
                     ViewInfo * viewInfo, 
                     bool drawEnvelope, bool drawSamples,
                     bool drawSliders, bool dB, bool muted);

   void DrawSpectrum(WaveTrack *track,
                     wxDC & dc, wxRect & r,
                     ViewInfo * viewInfo, bool autocorrelation);

   void DrawNoteTrack(NoteTrack *track,
                      wxDC & dc, wxRect & r, ViewInfo * viewInfo);

   void DrawLabelTrack(LabelTrack *track,
                       wxDC & dc, wxRect & r, ViewInfo * viewInfo);

   void DrawTimeTrack(TimeTrack *track,
                       wxDC & dc, wxRect & r, ViewInfo * viewInfo);

   void DrawTimeSlider(WaveTrack *track,
                       wxDC & dc, wxRect & r, ViewInfo * viewInfo,
                       bool rightwards);

   void SetBackgroundBrushes(wxBrush unselectedBrush, wxBrush selectedBrush,
			     wxPen unselectedPen, wxPen selectedPen) {
     this->unselectedBrush = unselectedBrush;
     this->selectedBrush = selectedBrush;
     this->unselectedPen = unselectedPen;
     this->selectedPen = selectedPen;
   }

 private:

   int mInsetLeft;
   int mInsetTop;
   int mInsetRight;
   int mInsetBottom;

   wxBrush blankBrush;
   wxBrush unselectedBrush;
   wxBrush selectedBrush;
   wxBrush sampleBrush;
   wxBrush selsampleBrush;
   wxBrush dragsampleBrush;// for samples which are draggable.
   wxBrush muteSampleBrush;
   wxPen blankPen;
   wxPen unselectedPen;
   wxPen selectedPen;
   wxPen samplePen;
   wxPen rmsPen;
   wxPen muteRmsPen;
   wxPen selsamplePen;
   wxPen muteSamplePen;
   wxPen shadowPen;

   Ruler *vruler;

   // Waveform utility functions

   void DrawWaveformBackground(wxDC &dc, wxRect r, uchar *imageBuffer,
                               int *where, int ssel0, int ssel1,
                               double *env, 
                               float zoomMin, float zoomMax,
                               bool dB, bool drawEnvelope);

   void DrawIndividualSamples(wxDC &dc, wxRect r,
                              WaveTrack *track,
                              double t0, double pps, double h,
                              float zoomMin, float zoomMax,
                              bool dB,
                              bool drawSamples,
                              bool showPoints, bool muted);

   void DrawMinMaxRMS(wxDC &dc, wxRect r, uchar *imageBuffer,
                      float zoomMin, float zoomMax,
                      double *envValues,
                      float *min, float *max, float *rms,
                      bool dB, bool muted);

   void DrawNegativeOffsetTrackArrows(wxDC &dc, wxRect &r);

   void DrawEnvLine(wxDC &dc, wxRect r, int x, int y, bool top);

};

extern int GetWaveYPosNew(float value, float min, float max,
			  int height, bool dB, bool outer, float dBr, 
			  bool clip);



#endif                          // define __AUDACITY_TRACKARTIST__
