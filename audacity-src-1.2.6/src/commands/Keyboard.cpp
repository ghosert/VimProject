/**********************************************************************

  Audacity: A Digital Audio Editor

  Keyboard.cpp

  Dominic Mazzoni
  Brian Gunlogson

**********************************************************************/

#include "../Audacity.h"

#include "Keyboard.h"

wxString KeyEventToKeyString(wxKeyEvent &event)
{
   wxString newStr = "";
   
   long key = event.GetKeyCode();
   
   if(event.ControlDown())
      newStr += "Ctrl+";
   
   if(event.AltDown())
      newStr += "Alt+";
   
   if(event.ShiftDown())
      newStr += "Shift+";

   if(event.MetaDown()) {
      #ifdef __WXMAC__
      newStr += "Cmd+";
      #endif
   }

   #if 0
   //BG: Don't allow noncombo letters to be bound
   if(((!event.ControlDown() && !event.AltDown() && !event.ShiftDown()) &&
       (key >= 33 && key <= 126)) ||
      ((!event.ControlDown() && !event.AltDown() &&
        event.ShiftDown()) && (key >= 33 && key <= 126)))
      return "";
   #endif
   
   if (event.ControlDown() && key >= 1 && key <= 26)
      newStr += (char)(64 + key);
   else if (key >= 33 && key <= 126)
      newStr += (char)key;
   else
   {
      switch(key)
      {
      case WXK_BACK:
         newStr += "Backspace";
         break;
      case WXK_DELETE:
         newStr += "Delete";
         break;
      case WXK_SPACE:
         newStr += "Spacebar";
         break;
      case WXK_PRIOR:
         newStr += "PageUp";
         break;
      case WXK_NEXT:
         newStr += "PageDown";
         break;
      case WXK_END:
         newStr += "End";
         break;
      case WXK_HOME:
         newStr += "Home";
         break;
      case WXK_LEFT:
         newStr += "Left";
         break;
      case WXK_UP:
         newStr += "Up";
         break;
      case WXK_RIGHT:
         newStr += "Right";
         break;
      case WXK_DOWN:
         newStr += "Down";
         break;
      case WXK_INSERT:
         newStr += "Insert";
         break;
      case WXK_NUMPAD0:
         newStr += "NUMPAD0";
         break;
      case WXK_NUMPAD1:
         newStr += "NUMPAD1";
         break;
      case WXK_NUMPAD2:
         newStr += "NUMPAD2";
         break;
      case WXK_NUMPAD3:
         newStr += "NUMPAD3";
         break;
      case WXK_NUMPAD4:
         newStr += "NUMPAD4";
         break;
      case WXK_NUMPAD5:
         newStr += "NUMPAD5";
         break;
      case WXK_NUMPAD6:
         newStr += "NUMPAD6";
         break;
      case WXK_NUMPAD7:
         newStr += "NUMPAD7";
         break;
      case WXK_NUMPAD8:
         newStr += "NUMPAD8";
         break;
      case WXK_NUMPAD9:
         newStr += "NUMPAD9";
         break;
      case WXK_MULTIPLY:
         newStr += "*";
         break;
      case WXK_ADD:
         newStr += "+";
         break;
      case WXK_SUBTRACT:
         newStr += "-";
         break;
      case WXK_DECIMAL:
         newStr += ".";
         break;
      case WXK_DIVIDE:
         newStr += "/";
         break;
      case WXK_F1:
         newStr += "F1";
         break;
      case WXK_F2:
         newStr += "F2";
         break;
      case WXK_F3:
         newStr += "F3";
         break;
      case WXK_F4:
         newStr += "F4";
         break;
      case WXK_F5:
         newStr += "F5";
         break;
      case WXK_F6:
         newStr += "F6";
         break;
      case WXK_F7:
         newStr += "F7";
         break;
      case WXK_F8:
         newStr += "F8";
         break;
      case WXK_F9:
         newStr += "F9";
         break;
      case WXK_F10:
         newStr += "F10";
         break;
      case WXK_F11:
         newStr += "F11";
         break;
      case WXK_F12:
         newStr += "F12";
         break;
      case WXK_F13:
         newStr += "F13";
         break;
      case WXK_F14:
         newStr += "F14";
         break;
      case WXK_F15:
         newStr += "F15";
         break;
      case WXK_F16:
         newStr += "F16";
         break;
      case WXK_F17:
         newStr += "F17";
         break;
      case WXK_F18:
         newStr += "F18";
         break;
      case WXK_F19:
         newStr += "F19";
         break;
      case WXK_F20:
         newStr += "F20";
         break;
      case WXK_F21:
         newStr += "F21";
         break;
      case WXK_F22:
         newStr += "F22";
         break;
      case WXK_F23:
         newStr += "F23";
         break;
      case WXK_F24:
         newStr += "F24";
         break;
      case WXK_PAGEUP:
         newStr += "PageUp";
         break;
      case WXK_PAGEDOWN:
         newStr += "PageDown";
         break;
      case WXK_NUMPAD_ENTER:
         newStr += "NUMPAD_ENTER";
         break;
      case WXK_NUMPAD_F1:
         newStr += "NUMPAD_F1";
         break;
      case WXK_NUMPAD_F2:
         newStr += "NUMPAD_F2";
         break;
      case WXK_NUMPAD_F3:
         newStr += "NUMPAD_F3";
         break;
      case WXK_NUMPAD_F4:
         newStr += "NUMPAD_F4";
         break;
      case WXK_NUMPAD_HOME:
         newStr += "NUMPAD_HOME";
         break;
      case WXK_NUMPAD_LEFT:
         newStr += "NUMPAD_LEFT";
         break;
      case WXK_NUMPAD_UP:
         newStr += "NUMPAD_UP";
         break;
      case WXK_NUMPAD_RIGHT:
         newStr += "NUMPAD_RIGHT";
         break;
      case WXK_NUMPAD_DOWN:
         newStr += "NUMPAD_DOWN";
         break;
      case WXK_NUMPAD_PRIOR:
         newStr += "NUMPAD_PAGEUP";
         break;
      case WXK_NUMPAD_PAGEUP:
         newStr += "NUMPAD_PAGEUP";
         break;
      case WXK_NUMPAD_NEXT:
         newStr += "NUMPAD_PAGEDOWN";
         break;
      case WXK_NUMPAD_PAGEDOWN:
         newStr += "NUMPAD_PAGEDOWN";
         break;
      case WXK_NUMPAD_END:
         newStr += "NUMPAD_END";
         break;
      case WXK_NUMPAD_BEGIN:
         newStr += "NUMPAD_HOME";
         break;
      case WXK_NUMPAD_INSERT:
         newStr += "NUMPAD_INSERT";
         break;
      case WXK_NUMPAD_DELETE:
         newStr += "NUMPAD_DELETE";
         break;
      case WXK_NUMPAD_EQUAL:
         newStr += "NUMPAD_EQUAL";
         break;
      case WXK_NUMPAD_MULTIPLY:
         newStr += "NUMPAD_MULTIPLY";
         break;
      case WXK_NUMPAD_ADD:
         newStr += "NUMPAD_ADD";
         break;
      case WXK_NUMPAD_SUBTRACT:
         newStr += "NUMPAD_SUBTRACT";
         break;
      case WXK_NUMPAD_DECIMAL:
         newStr += "NUMPAD_DECIMAL";
         break;
      case WXK_NUMPAD_DIVIDE:
         newStr += "NUMPAD_DIVIDE";
         break;
      default:
         return ""; // Don't do anything if we don't recognize the key
      }
   }

   return newStr;
}

