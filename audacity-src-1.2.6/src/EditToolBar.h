/**********************************************************************

  Audacity: A Digital Audio Editor

  
  EditToolbar.h
 
  Dominic Mazzoni
  Shane T. Mueller
 
  This class, which is a child of Toolbar, creates the
  window containing interfaces to commonly-used edit
  functions that are otherwise only available through
  menus. The window can be embedded within a normal project
  window, or within a ToolbarFrame that is managed by a
  global ToolBarStub called gControlToolBarStub.

  All of the controls in this window were custom-written for
  Audacity - they are not native controls on any platform -
  however, it is intended that the images could be easily
  replaced to allow "skinning" or just customization to
  match the look and feel of each platform.

**********************************************************************/

#ifndef __AUDACITY_EDIT_TOOLBAR__
#define __AUDACITY_EDIT_TOOLBAR__

#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/minifram.h>
#include <wx/object.h>

#include "ToolBar.h"

class AButton;
class ASlider;
class EditToolBar;
class ToolBar;
class ToolBarFrame;

class wxImage;
class wxSize;
class wxPoint;

enum {
   ETBCutID,
   ETBCopyID,
   ETBPasteID,
   ETBTrimID,
   ETBSilenceID,

   ETBUndoID,
   ETBRedoID,

   ETBZoomInID,
   ETBZoomOutID,

   #if 0 // Disabled for version 1.2.0 since it doesn't work quite right...
   ETBZoomToggleID,
   #endif

   ETBZoomSelID,
   ETBZoomFitID,

   ETBNumButtons
};

class EditToolBar:public ToolBar {
 public:
   EditToolBar() {};
   EditToolBar(wxWindow * parent, wxWindowID id,
               const wxPoint & pos, const wxSize & size);
   EditToolBar(wxWindow * parent);
   virtual ~ EditToolBar();
   void InitializeEditToolBar();

   virtual void OnPaint(wxPaintEvent & event);
   virtual void OnKeyEvent(wxKeyEvent & event);
   virtual void EnableDisableButtons();

   void OnButton(wxCommandEvent &event);

 private:
   void AddButton(const char **fg, const char **disabled, const char **alpha,
                  int id, const char *tooltip);
   void AddSeparator();

   void MakeButtons();
   
   AButton *mButtons[ETBNumButtons];

   int mButtonPos;
   wxImage *upImage;
   wxImage *downImage;
   wxImage *hiliteImage;

   int mDividers[100];
   int mNumDividers;

   DECLARE_EVENT_TABLE()
};

#endif
