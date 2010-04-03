/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackPanel.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TRACK_PANEL__
#define __AUDACITY_TRACK_PANEL__

#include <wx/timer.h>
#include <wx/window.h>

//Stm:  The following included because of the sampleCount struct.
#include "Sequence.h"  
  
class wxMenu;
class wxRect;
class wxStatusBar;

class TrackList;
class Track;
class TrackPanel;
class TrackArtist;
class WaveTrack;
class Ruler;
class AdornedRulerPanel;
class LWSlider;
class ControlToolBar; //Needed because state of controls can affect what gets drawn.

struct ViewInfo;

struct tpBitmap
{
   wxBitmap *bitmap;
   wxCoord x;
   wxCoord y;
};

WX_DEFINE_ARRAY(tpBitmap *, tpBitmapArray);
WX_DEFINE_ARRAY(LWSlider *, LWSliderArray);

class TrackPanelListener {
 public:
   virtual void TP_DisplaySelection() = 0;
   virtual void TP_DisplayStatusMessage(const char *msg, int fieldNum) = 0;
   virtual int TP_GetCurrentTool() = 0;
   virtual ControlToolBar * TP_GetControlToolBar() = 0;
   virtual void TP_OnPlayKey() = 0;
   virtual void TP_PushState(wxString shortDesc, wxString longDesc,
                             bool consolidate = false) = 0;
   virtual void TP_ModifyState() = 0;
   virtual void TP_RedrawScrollbars() = 0;
   virtual void TP_ScrollLeft() = 0;
   virtual void TP_ScrollRight() = 0;
   virtual void TP_ScrollWindow(double scrollto) = 0;
   virtual void TP_ScrollUpDown(int delta) = 0;
   virtual void TP_HasMouse() = 0;
   virtual void TP_HandleResize() = 0;
};


/// The TrackLabel is shown to the side of a track 
/// It has the menus, pan and gain controls displayed in it.
///
/// In its current implementation it is not derived from a
/// wxWindow.  Following the original coding style, it has 
/// been coded as a 'flyweight' class, which is passed 
/// state as needed, except for the array of gains and pans.
/// 
/// An alternative way to code this is to have an instance
/// of this class for each instance displayed.
/// 
class TrackLabel
{
public:
   TrackLabel(wxWindow * pParentIn);
   ~TrackLabel();

   int GetTitleWidth() const { return 100; }
private:
   void MakeMoreSliders();
   void EnsureSufficientSliders(int index);

   void DrawBackground(wxDC * dc, const wxRect r, bool bSelected, const int labelw);
   void DrawCloseBox(wxDC * dc, const wxRect r, bool down);
   void DrawTitleBar(wxDC * dc, const wxRect r, Track * t, bool down);
   void DrawMuteSolo(wxDC * dc, const wxRect r, Track * t, bool down, bool solo);
   void DrawVRuler(wxDC * dc, const wxRect r, Track * t);
   void DrawSliders(wxDC *dc, WaveTrack *t, wxRect r, int index);

   void GetTrackControlsRect(const wxRect r, wxRect &dest) const;
   void GetCloseBoxRect(const wxRect r, wxRect &dest) const;
   void GetTitleBarRect(const wxRect r, wxRect &dest) const;
   void GetMuteSoloRect(const wxRect r, wxRect &dest, bool solo) const;
   void GetGainRect(const wxRect r, wxRect &dest) const;
   void GetPanRect(const wxRect r, wxRect &dest) const;

public:
   LWSliderArray mGains;
   LWSliderArray mPans;
   wxWindow * pParent;

friend class TrackPanel;
};



const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.

/// The TrackPanel manages multiple tracks and their TrackLabels.
/// Note that with stereo tracks there will be one TrackLabel
/// being used by two wavetracks.
class TrackPanel:public wxWindow {
 public:

   TrackPanel(wxWindow * parent,
              wxWindowID id,
              const wxPoint & pos,
              const wxSize & size,
              TrackList * tracks,
              ViewInfo * viewInfo, TrackPanelListener * listener);

   virtual ~ TrackPanel();

   void UpdatePrefs();

   void OnPaint(wxPaintEvent & event);
   void OnMouseEvent(wxMouseEvent & event);
   void OnKeyEvent(wxKeyEvent & event);

   double GetMostRecentXPos();

   void OnTimer();

   int GetRulerHeight();
   int GetLeftOffset() const { return GetLabelWidth() + 1;}

   void GetTracksUsableArea(int *width, int *height) const;

   void SelectNone();

   void SetStop(bool bStopped);

   virtual void Refresh(bool eraseBackground = TRUE,
                        const wxRect *rect = (const wxRect *) NULL);
   void DisplaySelection();

   void SetSelectionFormat(int iformat);
   void SetSnapTo(int snapto);

   void HandleShiftKey(bool down);

 private:

   void TrackSpecificMouseEvent(wxMouseEvent & event);
   void DrawCursors(wxDC * dc = NULL);
   void RemoveStaleCursors(wxRegionIterator * upd);

   void ScrollDuringDrag();
   void UpdateIndicator(wxDC * dc = NULL);
   void RemoveStaleIndicators(wxRegionIterator * upd);

   // Working out where to dispatch the event to.
   int DetermineToolToUse( ControlToolBar * pCtb, wxMouseEvent & event);
   bool HitTestEnvelope(Track *track, wxRect &r, wxMouseEvent & event);
   bool HitTestSamples(Track *track, wxRect &r, wxMouseEvent & event);
   bool HitTestSlide(Track *track, wxRect &r, wxMouseEvent & event);

   // AS: Selection handling
   void HandleSelect(wxMouseEvent & event);
   void SelectionHandleDrag(wxMouseEvent &event);
   void SelectionHandleClick(wxMouseEvent &event, 
			     Track* pTrack, wxRect r, int num);
   void StartSelection (int, int);
   void ExtendSelection(int, int);

   // AS: Cursor handling
   void HandleCursor(wxMouseEvent & event);

   // AS: Envelope editing handlers
   void HandleEnvelope(wxMouseEvent & event);
   void ForwardEventToEnvelope(wxMouseEvent &event);

   // AS: Track sliding handlers
   void HandleSlide(wxMouseEvent & event);
   void StartSlide(wxMouseEvent &event, double& totalOffset, wxString& name);
   void DoSlide(wxMouseEvent &event, double& totalOffset);

   // AS: Handle zooming into tracks
   void HandleZoom(wxMouseEvent & event);
   void DragZoom(int x);
   void DoZoomInOut(wxMouseEvent &event, int x_center);

   void HandleVZoom(wxMouseEvent & event);

   // BG: Handle drawing
   void HandleDraw(wxMouseEvent & event);

   // MM: Handle mouse wheel rotation
   void HandleWheelRotation(wxMouseEvent & event);

   void DoPopupMenu(wxMouseEvent &event, wxRect& titleRect, 
		    Track* t, wxRect &r, int num);


   void HandleResize(wxMouseEvent & event);

   void HandleLabelClick(wxMouseEvent & event);
   void HandleRearrange(wxMouseEvent & event);
   void CalculateRearrangingThresholds(wxMouseEvent & event);
   void HandleClosing(wxMouseEvent & event);
   void HandleMutingSoloing(wxMouseEvent & event, bool solo);
   void HandleSliders(wxMouseEvent &event, bool pan);
   bool MuteSoloFunc(Track *t, wxRect r, int x, int f, bool solo);
   bool GainFunc(Track * t, wxRect r, wxMouseEvent &event,
                 int index, int x, int y);
   bool PanFunc(Track * t, wxRect r, wxMouseEvent &event,
                int index, int x, int y);
   void MakeParentRedrawScrollbars();
   
   // AS: Pushing the state preserves state for Undo operations.
   void MakeParentPushState(wxString desc, wxString shortDesc,
                            bool consolidate = false);
   void MakeParentModifyState();

   void MakeParentResize();

   void OnSetName(wxCommandEvent &event);
   void OnSetFont(wxCommandEvent &event);

   void OnMoveTrack    (wxCommandEvent &event);
   void OnChangeOctave (wxCommandEvent &event);
   void OnChannelChange(wxCommandEvent &event);
   void OnSetDisplay   (wxCommandEvent &event);
   void OnSetTimeTrackRange (wxCommandEvent &event);

   void SetMenuCheck( wxMenu & menu, int newId );
   void SetRate(Track *pTrack, double rate);
   void OnRateChange(wxCommandEvent &event);
   void OnRateOther(wxCommandEvent &event);

   void OnFormatChange(wxCommandEvent &event);

   void OnSplitStereo(wxCommandEvent &event);
   void OnMergeStereo(wxCommandEvent &event);

   void RemoveTrack(Track * toRemove);

   // Find track info by coordinate
   Track *FindTrack(int mouseX, int mouseY, bool label, bool link,
                     wxRect * trackRect = NULL, int *trackNum = NULL);

//   int GetTitleWidth() const { return 100; }
   int GetTitleOffset() const { return 0; }
   int GetVRulerWidth() const { return 36;}
   int GetVRulerOffset() const { return GetTitleOffset() + mTrackLabel.GetTitleWidth();}
   int GetLabelWidth() const { return mTrackLabel.GetTitleWidth() + GetVRulerWidth();}

   void DrawRuler(wxDC * dc, bool text = true);
   void DrawTrackIndicator(wxDC *dc);

   void DrawTracks(wxDC * dc);

   void DrawEverythingElse(wxDC *dc, const wxRect panelRect, const wxRect clip);
   void DrawEverythingElse(Track *t, wxDC *dc, wxRect &r, wxRect &wxTrackRect,
                           int index);
   void DrawOutside(Track *t, wxDC *dc, const wxRect rec, const int labelw, 
                    const int vrul, const wxRect trackRect, int index);
   void DrawZooming(wxDC* dc, const wxRect clip);

   void DrawShadow            (Track *t, wxDC* dc, const wxRect r);
   void DrawBordersAroundTrack(Track *t, wxDC* dc, const wxRect r, const int labelw, const int vrul);
   void DrawOutsideOfTrack    (Track *t, wxDC* dc, const wxRect r);

   int IdOfRate( int rate );
   int IdOfFormat( int format );

   wxString TrackSubText(Track *t);

   TrackLabel mTrackLabel;

   TrackPanelListener *mListener;

   TrackList *mTracks;
   ViewInfo *mViewInfo;
   wxStatusBar *mStatusBar;

   AdornedRulerPanel *mRuler;


   TrackArtist *mTrackArtist;

   class AudacityTimer:public wxTimer {
   public:
     virtual void Notify() { parent->OnTimer(); }
     TrackPanel *parent;
   } mTimer;
   

   //This stores the parts of the screen that get overwritten by the indicator
   tpBitmapArray mScreenAtIndicator;
   
   // This indicates whether the last indicator drawing
   // existed, so that we can draw over it to erase it
   bool mPlayIndicatorExists;

   tpBitmapArray mPreviousCursorData;

   int mTimeCount;

   wxBitmap *mBitmap;
   int mPrevWidth;
   int mPrevHeight;

   double mSelStart;

   Track *mCapturedTrack;
   wxRect mCapturedRect;
   int mCapturedNum;

   bool mRedrawAfterStop;

   bool mIndicatorShowing;

   wxMouseEvent mLastMouseEvent;

   int mMouseClickX;
   int mMouseClickY;

   int mMouseMostRecentX;
   int mMouseMostRecentY;

   int mZoomStart;
   int mZoomEnd;

   Track * mDrawingTrack;          // Keeps track of which track you are drawing on between events cf. HandleDraw()
   int mDrawingTrackTop;           // Keeps track of the top position of the drawing track.
   sampleCount mDrawingStartSample;   // sample of last click-down
   float mDrawingStartSampleValue;    // value of last click-down
   sampleCount mDrawingLastDragSample; // sample of last drag-over
   float mDrawingLastDragSampleValue;  // value of last drag-over
 
   double PositionToTime(int mouseXCoordinate,
                         int trackLeftEdge) const;
   int TimeToPosition(double time,
                      int trackLeftEdge) const;

   int mInitialTrackHeight;
   int mInitialUpperTrackHeight;
   bool mAutoScrolling;

   bool mIsVZooming;
   bool mIsClosing;
   bool mIsSelecting;
   bool mIsResizing;
   bool mIsResizingBelowLinkedTracks;
   bool mIsResizingBetweenLinkedTracks;
   bool mIsRearranging;
   bool mIsSliding;
   bool mIsEnveloping;
   bool mIsMuting;
   bool mIsSoloing;
   bool mIsGainSliding;
   bool mIsPanSliding;   

   bool mAdjustSelectionEdges;

   // JH: if the user is dragging a track, at what y
   //   coordinate should the dragging track move up or down?
   int mMoveUpThreshold;
   int mMoveDownThreshold;
   
   bool IsDragZooming() const { return abs(mZoomEnd - mZoomStart) > DragThreshold;}

   wxCursor *mArrowCursor;
   wxCursor *mPencilCursor;
   wxCursor *mSelectCursor;
   wxCursor *mResizeCursor;
   wxCursor *mSlideCursor;
   wxCursor *mEnvelopeCursor;
   wxCursor *mSmoothCursor;
   wxCursor *mZoomInCursor;
   wxCursor *mZoomOutCursor;
   wxCursor *mRearrangeCursor;
   wxCursor *mDisabledCursor;
   wxCursor *mAdjustLeftSelectionCursor;
   wxCursor *mAdjustRightSelectionCursor;

   wxMenu *mWaveTrackMenu;
   wxMenu *mNoteTrackMenu;
   wxMenu *mTimeTrackMenu;
   wxMenu *mLabelTrackMenu;
   wxMenu *mRateMenu;
   wxMenu *mFormatMenu;

   Track *mPopupMenuTarget;

 public:

   DECLARE_EVENT_TABLE()
};

//This constant determines the size of the vertical region (in pixels) around
//the bottom of a track that can be used for vertical track resizing.
#define TRACK_RESIZE_REGION 5

//This constant determines the size of the horizontal region (in pixels) around
//the right and left selection bounds that can be used for horizontal selection adjusting
#define SELECTION_RESIZE_REGION 3

#define SMOOTHING_KERNEL_RADIUS 3
#define SMOOTHING_BRUSH_RADIUS 5
#define SMOOTHING_PROPORTION_MAX 0.7
#define SMOOTHING_PROPORTION_MIN 0.0

#endif
