/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.h

  Brian Gunlogson
  Dominic Mazzoni

  This class implements a system for organizing all user-callable
  commands, by creating and managing a menu bar with a command
  associated with each item, and managing other commands callable
  by keyboard shortcuts.

  Commands are implemented by overriding an abstract functor class.
  See Menus.cpp for an example use.

  Menus or submenus containing lists of items can be added at once,
  with a single function (functor) to be called when any of the
  items is selected, with the index number of the selection as the
  parameter.  This is useful for dynamic menus (effects) and
  submenus containing a list of choices (selection formats).

  Menu items can be enabled or disabled individually, groups of
  "multi-items" can be enabled or disabled all at once, or entire
  sets of commands can be enabled or disabled all at once using
  flags.  The flags should be a bitfield stored in a 32-bit
  integer but can be whatever you want.  You specify both the
  desired values of the flags, and the set of flags relevant to
  a particular command, by using a combination of a flags parameter
  and a mask parameter.  Any flag set to 0 in the mask parameter is
  the same as "don't care".  Any command whose mask is set to zero
  will not be affected by enabling/disabling by flags.

**********************************************************************/

#ifndef __AUDACITY_COMMAND_MANAGER__
#define __AUDACITY_COMMAND_MANAGER__

#include <wx/string.h>
#include <wx/dynarray.h>
#include <wx/menu.h>
#include <wx/hashmap.h>

#include "../xml/XMLTagHandler.h"

class CommandFunctor
{
public:
   virtual void operator()(int index = 0) = 0;
};

struct MenuBarListEntry
{
   wxString name;
   wxMenuBar *menubar;
};

struct SubMenuListEntry
{
   wxString name;
   wxMenu *menu;
};

struct CommandListEntry
{
   int id;
   wxString name;
   wxString key;
   wxString defaultKey;
   wxString label;
   wxMenu *menu;
   CommandFunctor *callback;
   bool multi;
   int index;
   int count;
   bool enabled;
   wxUint32 flags;
   wxUint32 mask;
};

WX_DEFINE_ARRAY(MenuBarListEntry *, MenuBarList);
WX_DEFINE_ARRAY(SubMenuListEntry *, SubMenuList);
WX_DEFINE_ARRAY(CommandListEntry *, CommandList);

WX_DECLARE_STRING_HASH_MAP(CommandListEntry *, CommandNameHash);
WX_DECLARE_HASH_MAP(int, CommandListEntry *, wxIntegerHash, wxIntegerEqual, CommandIDHash);

class CommandManager: public XMLTagHandler
{
 public:

   //
   // Constructor / Destructor
   //

   CommandManager();
   virtual ~CommandManager();

   void PurgeData();

   //
   // Creating menus and adding commands
   //

   wxMenuBar *AddMenuBar(wxString sMenu);

   void BeginMenu(wxString tName);
   void EndMenu();

   wxMenu* BeginSubMenu(wxString tName);
   void EndSubMenu();

   void AddItem(wxString name, wxString label, CommandFunctor *callback);
   void AddItemList(wxString name, wxArrayString labels,
                    CommandFunctor *callback, bool plugins = false);

   void AddSeparator();

   // A command doesn't actually appear in a menu but might have a
   // keyboard shortcut.
   void AddCommand(wxString name, wxString label, CommandFunctor *callback);

   //
   // Command masks
   //

   // For new items/commands
   void SetDefaultFlags(wxUint32 flags, wxUint32 mask);

   void SetCommandFlags(wxString name, wxUint32 flags, wxUint32 mask);
   void SetCommandFlags(const char **names,
                        wxUint32 flags, wxUint32 mask);
   // Pass multiple command names as const char *, terminated by NULL
   void SetCommandFlags(wxUint32 flags, wxUint32 mask, ...);

   //
   // Modifying menus
   //

   void EnableUsingFlags(wxUint32 flags, wxUint32 mask);
   void Enable(wxString name, bool enabled);
   void Modify(wxString name, wxString newLabel);

   //
   // Executing commands
   //

   bool HandleMenuID(int id, wxUint32 flags, wxUint32 mask);

   bool HandleKey(wxKeyEvent &evt, wxUint32 flags, wxUint32 mask);

   //
   // Accessing
   //

   void GetAllCommandNames(wxArrayString &names, bool includeMultis);

   wxString GetLabelFromName(wxString name);
   wxString GetKeyFromName(wxString name);
   wxString GetDefaultKeyFromName(wxString name);

   //
   // Loading/Saving
   //

   virtual bool HandleXMLTag(const char *tag, const char **attrs);
   virtual void HandleXMLEndTag(const char *tag);
   virtual XMLTagHandler *HandleXMLChild(const char *tag);
   virtual void WriteXML(int depth, FILE *fp);

 protected:

   wxMenuBar * CurrentMenuBar();
   wxMenuBar * GetMenuBar(wxString sMenu);
   wxMenu * CurrentSubMenu();
   wxMenu * CurrentMenu();

   int NextIdentifier(int ID);
   int NewIdentifier(wxString name, wxString label, wxMenu *menu,
                     CommandFunctor *callback,
                     bool multi, int index, int count);
   void Enable(CommandListEntry *entry, bool enabled);

   wxString GetKey(wxString label);

private:
   MenuBarList  mMenuBarList;
   SubMenuList  mSubMenuList;
   CommandList  mCommandList;
   CommandNameHash  mCommandNameHash;
   CommandNameHash  mCommandKeyHash;
   CommandIDHash  mCommandIDHash;
   int mCurrentID;
   int mHiddenID;
   int mXMLKeysRead;
   wxMenu * mCurrentMenu;

   wxUint32 mDefaultFlags;
   wxUint32 mDefaultMask;
};

#endif
