/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.h

  Dominic Mazzoni

  Manages color brushes and pens and provides utility
  drawing functions

**********************************************************************/

#ifndef __AUDACITY_COLOR__
#define __AUDACITY_COLOR__

#include <wx/brush.h>
#include <wx/pen.h>

class wxDC;
class wxRect;

class AColor {
 public:
   static void Init(wxDC * dc);

   static void SetLabelFont(wxDC & dc);

   static void Bevel(wxDC & dc, bool up, wxRect & r);

   static void Light(wxDC * dc, bool selected);
   static void Medium(wxDC * dc, bool selected);
   static void Dark(wxDC * dc, bool selected);

   static void CursorColor(wxDC * dc);
   static void IndicatorColor(wxDC * dc, bool recording);

   static void Mute(wxDC * dc, bool on, bool selected, bool soloing);
   static void Solo(wxDC * dc, bool on, bool selected);

   static void MIDIChannel(wxDC * dc, int channel /* 1 - 16 */ );
   static void LightMIDIChannel(wxDC * dc, int channel /* 1 - 16 */ );
   static void DarkMIDIChannel(wxDC * dc, int channel /* 1 - 16 */ );

   static wxBrush lightBrush[2];
   static wxBrush mediumBrush[2];
   static wxBrush darkBrush[2];
   static wxPen lightPen[2];
   static wxPen mediumPen[2];
   static wxPen darkPen[2];
   
   static wxPen cursorPen;
   static wxPen indicatorPen[2];
   static wxBrush indicatorBrush[2];

   static wxBrush muteBrush[2];
   static wxBrush soloBrush;

   static wxPen envelopePen;
   static wxPen WideEnvelopePen;
   static wxBrush envelopeBrush;

   static wxBrush labelFlagBrush;
   static wxBrush labelUnselectedBrush;
   static wxBrush labelSelectedBrush;
   static wxPen labelFlagPen;
   static wxPen labelUnselectedPen;
   static wxPen labelSelectedPen;

   static wxBrush tooltipBrush;

   static wxFont labelFont;

 private:
   static bool inited;

};

void GetColorGradient(float value,
                      bool selected,
                      bool grayscale,
                      unsigned char *red,
                      unsigned char *green, unsigned char *blue);

#endif
