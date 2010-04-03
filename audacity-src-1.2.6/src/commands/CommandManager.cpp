/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManager.cpp

  Brian Gunlogson
  Dominic Mazzoni

  See CommandManager.h for an overview of this class.

**********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/hash.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/log.h>

#include "../Prefs.h"

#include "CommandManager.h"

#include "Keyboard.h"

// On wxGTK, there may be many many many plugins, but the menus don't automatically
// allow for scrolling, so we build sub-menus.  If the menu gets longer than
// MAX_MENU_LEN, we put things in submenus that have MAX_SUBMENU_LEN items in them.
// 
#ifdef __WXGTK__
#define MAX_MENU_LEN 20
#define MAX_SUBMENU_LEN 15
#else
#define MAX_MENU_LEN 1000
#define MAX_SUBMENU_LEN 1000
#endif


///
///  Standard Constructor
///
CommandManager::CommandManager():
   mCurrentID(0),
   mHiddenID(0),
   mCurrentMenu(NULL),
   mDefaultFlags(0),
   mDefaultMask(0)
{
}

///
///  Class Destructor.  Includes PurgeData, which removes
///  menubars
CommandManager::~CommandManager()
{
   //WARNING: This removes menubars that could still be assigned to windows!
   PurgeData();
}

void CommandManager::PurgeData()
{
   // Delete callback functors BEFORE clearing mCommandList!
   // These are the items created within 'FN()'
   size_t i;
   CommandFunctor * pCallback = NULL;
   for(i=0; i<mCommandList.GetCount(); i++)
   {
      CommandListEntry *tmpEntry = mCommandList[i];
      // JKC: We only want to delete each callbacks once.
      // AddItemList() may have inserted the same callback 
      // several times over.
      if( tmpEntry->callback != pCallback )
      {
         pCallback = tmpEntry->callback;
         delete pCallback;
      }
   }

   // mCommandList contains pointers to CommandListEntrys
   // mMenuBarList contains pointers to MenuBarListEntrys.
   // mSubMenuList contains pointers to SubMenuListEntrys
   WX_CLEAR_ARRAY( mCommandList );
   WX_CLEAR_ARRAY( mMenuBarList );
   WX_CLEAR_ARRAY( mSubMenuList );

   mCommandNameHash.clear();
   mCommandKeyHash.clear();
   mCommandIDHash.clear();

   mCurrentMenu = NULL;
   mCurrentID = 0;
}


///
/// Makes a new menubar for placement on the top of a project
/// Names it according to the passed-in string argument.
///
/// If the menubar already exists, simply returns it.
wxMenuBar *CommandManager::AddMenuBar(wxString sMenu)
{
   wxMenuBar *menuBar = GetMenuBar(sMenu);
   if (menuBar)
      return menuBar;

   MenuBarListEntry *tmpEntry = new MenuBarListEntry;

   tmpEntry->menubar = new wxMenuBar();
   tmpEntry->name = sMenu;

   mMenuBarList.Add(tmpEntry);

   return tmpEntry->menubar;
}


///
/// Retrieves the menubar based on the name given in AddMenuBar(name) 
///
wxMenuBar * CommandManager::GetMenuBar(wxString sMenu)
{
   for(unsigned int i = 0; i < mMenuBarList.GetCount(); i++)
   {
      if(!mMenuBarList[i]->name.Cmp(sMenu))
         return mMenuBarList[i]->menubar;
   }

   return NULL;
}


///
/// Retrieve the 'current' menubar; either NULL or the
/// last on in the mMenuBarList.
wxMenuBar * CommandManager::CurrentMenuBar()
{
   if(mMenuBarList.IsEmpty())
      return NULL;

   return mMenuBarList[mMenuBarList.GetCount()-1]->menubar;
}


///
/// This makes a new menu and adds it to the 'CurrentMenuBar'
///
/// If the menu already exists, all of the items in it are
/// cleared instead.
///
void CommandManager::BeginMenu(wxString tName)
{
   

   wxMenu *tmpMenu = new wxMenu();

   mCurrentMenu = tmpMenu;

   CurrentMenuBar()->Append(mCurrentMenu, tName);
}


///
/// This ends a menu by setting the pointer to it
/// to NULL.  It is still attached to the CurrentMenuBar()
void CommandManager::EndMenu()
{
   mCurrentMenu = NULL;
}



///
/// This starts a new submenu, and names it according to
/// the function's argument.
wxMenu* CommandManager::BeginSubMenu(wxString tName)
{
   SubMenuListEntry *tmpEntry = new SubMenuListEntry;

   tmpEntry->menu = new wxMenu();
   tmpEntry->name = tName;

   mSubMenuList.Add(tmpEntry);
   
   return(tmpEntry->menu);
}


///
/// This function is called after the final item of a SUBmenu is added.
/// Submenu items are added just like regular menu items; they just happen
/// after BeginSubMenu() is called but before EndSubMenu() is called.
void CommandManager::EndSubMenu()
{
   size_t submenu_count = mSubMenuList.GetCount()-1;

   //Save the submenu's information
   SubMenuListEntry *tmpSubMenu = mSubMenuList[submenu_count];

   //Pop off the new submenu so CurrentMenu returns the parent of the submenu
   mSubMenuList.RemoveAt(submenu_count);

   //Add the submenu to the current menu
   CurrentMenu()->Append(0, tmpSubMenu->name, tmpSubMenu->menu, tmpSubMenu->name);

   delete tmpSubMenu;
}


///
/// This returns the 'Current' Submenu, which is the one at the
///  end of the mSubMenuList (or NULL, if it doesn't exist).
wxMenu * CommandManager::CurrentSubMenu()
{
   if(mSubMenuList.IsEmpty())
      return NULL;

   return mSubMenuList[mSubMenuList.GetCount()-1]->menu;
}

///
/// This returns the current menu that we're appending to - note that
/// it could be a submenu if BeginSubMenu was called and we haven't
/// reached EndSubMenu yet.
wxMenu * CommandManager::CurrentMenu()
{
   if(!mCurrentMenu)
      return NULL;

   wxMenu * tmpCurrentSubMenu = CurrentSubMenu();

   if(!tmpCurrentSubMenu)
   {
      return mCurrentMenu;
   }

   return tmpCurrentSubMenu;
}

///
/// Add a menu item to the current menu.  When the user selects it, the
/// given functor will be called
void CommandManager::AddItem(wxString name, wxString label,
                             CommandFunctor *callback)
{
   int ID = NewIdentifier(name, label, CurrentMenu(), callback, false, 0, 0);

   // Replace the accel key with the one from the preferences
   label = label.BeforeFirst('\t');

   // This is a very weird hack.  Under GTK, menu labels are totally
   // linked to accelerators the first time you create a menu item
   // with that label and can't be changed.  This causes all sorts of
   // problems.  As a workaround, we create each menu item with a
   // made-up name (just an ID number string) but with the accelerator
   // we want, then immediately change the label to the correct string.
   // -DMM
   mHiddenID++;
   wxString dummy, newLabel;
   dummy.Printf("%s%08d", (const char *)label, mHiddenID);
   newLabel = label;

   bool shortcut = false;

   if (mCommandIDHash[ID]->key.Length() > 0)
      shortcut = true;
   
   // Mac OS X fixes
  #ifdef __WXMAC__
   if (newLabel.Length() > 0 && newLabel[0] == '&')
      newLabel = newLabel.Right(newLabel.Length()-1);

   if (shortcut == true &&
       (mCommandIDHash[ID]->key.Length() < 5 ||
        mCommandIDHash[ID]->key.Left(5) != "Ctrl+"))
      shortcut = false;
  #endif
   
   if (shortcut) {
      dummy = dummy + "\t" + mCommandIDHash[ID]->key;
      newLabel = newLabel + "\t" + mCommandIDHash[ID]->key;
   }

   CurrentMenu()->Append(ID, dummy);
   CurrentMenu()->SetLabel(ID, newLabel);
}

///
/// Add a list of menu items to the current menu.  When the user selects any
/// one of these, the given functor will be called
/// with its position in the list as the index number.
/// When you call Enable on this command name, it will enable or disable
/// all of the items at once.
void CommandManager::AddItemList(wxString name, wxArrayString labels,
                                 CommandFunctor *callback,
                                 bool plugins /*= false*/)
{
   unsigned int i;

   #ifndef __WXGTK__
   plugins = false;
   #endif

   if (CurrentMenu()->GetMenuItemCount() + labels.GetCount() < MAX_MENU_LEN)
      plugins = false;

   if (!plugins) {
      for(i=0; i<labels.GetCount(); i++) {
         int ID = NewIdentifier(name, labels[i], CurrentMenu(), callback,
                                true, i, labels.GetCount());
         CurrentMenu()->Append(ID, labels[i]);
      }
      return;
   }

   wxString label;
   unsigned int effLen = labels.GetCount();
   int listnum = 1;
   int tmpmax = MAX_SUBMENU_LEN  < effLen? MAX_SUBMENU_LEN: effLen;

   //The first submenu starts at 1.
   BeginSubMenu(wxString::Format(_("Plugins 1 to %i"), tmpmax));

   for(i=0; i<effLen; i++) {
      int ID = NewIdentifier(name, labels[i], CurrentMenu(), callback,
                             true, i, effLen);

      CurrentMenu()->Append(ID, labels[i]);
     
      if(((i+1) % MAX_SUBMENU_LEN) == 0 && i != (effLen - 1)) {
         EndSubMenu();
         listnum++;
         
         //This label the plugins by number in the submenu title (1 to 15, 15 to 30, etc.)
         tmpmax = i + MAX_SUBMENU_LEN  < effLen? 1 + i + MAX_SUBMENU_LEN: effLen;
         BeginSubMenu(wxString::Format(_("Plugins %i to %i"),i+2,tmpmax));
      }
   }
   EndSubMenu();
}

///
/// Add a command that doesn't appear in a menu.  When the key is pressed, the
/// given function pointer will be called (via the CommandManagerListener)
void CommandManager::AddCommand(wxString name, wxString label,
                                CommandFunctor *callback)
{
   NewIdentifier(name, label, NULL, callback, false, 0, 0);
}

void CommandManager::AddSeparator()
{
   CurrentMenu()->AppendSeparator();
}

int CommandManager::NextIdentifier(int ID)
{
   ID++;

   //Skip the reserved identifiers used by wxWindows
   if((ID >= wxID_LOWEST) && (ID <= wxID_HIGHEST))
      ID = wxID_HIGHEST+1;

   return ID;
}

///Given all of the information for a command, comes up with a new unique
///ID, adds it to a list, and returns the ID.
///WARNING: Does this conflict with the identifiers set for controls/windows?
///If it does, a workaround may be to keep controls below wxID_LOWEST
///and keep menus above wxID_HIGHEST
int CommandManager::NewIdentifier(wxString name, wxString label, wxMenu *menu,
                                  CommandFunctor *callback,
                                  bool multi, int index, int count)
{
   CommandListEntry *tmpEntry = new CommandListEntry;
   
   // wxMac 2.5 and higher will do special things with the
   // Preferences, Exit (Quit), and About menu items, if we give
   // them the right IDs.
   // Otherwise we just pick increasing ID numbers for each new
   // command.  Note that the name string we are comparing
   // ("About", "Preferences") is the internal command name
   // (untranslated), not the label that actually appears in the
   // menu (which might be translated).

   mCurrentID = NextIdentifier(mCurrentID);
   tmpEntry->id = mCurrentID;

  #if (defined(__WXMAC__) && \
    !((wxMAJOR_VERSION == 2) && (wxMINOR_VERSION <= 4)))
   if (name == "Preferences")
      tmpEntry->id = wxID_PREFERENCES;
   else if (name == "Exit")
      tmpEntry->id = wxID_EXIT;
   else if (name == "About")
      tmpEntry->id = wxID_ABOUT;
  #endif

   tmpEntry->name = name;
   tmpEntry->label = label;
   tmpEntry->menu = menu;
   tmpEntry->callback = callback;
   tmpEntry->multi = multi;
   tmpEntry->index = index;
   tmpEntry->count = count;
   tmpEntry->key = GetKey(label);
   tmpEntry->defaultKey = GetKey(label);
   tmpEntry->flags = mDefaultFlags;
   tmpEntry->mask = mDefaultMask;
   tmpEntry->enabled = true;

   // Key from preferences overridse the default key given
   gPrefs->SetPath("/NewKeys");
   if (gPrefs->HasEntry(name)) {
      tmpEntry->key = gPrefs->Read(name, GetKey(label));
   }
   gPrefs->SetPath("/");
   
   mCommandList.Add(tmpEntry);
   mCommandIDHash[tmpEntry->id] = tmpEntry;   

   if (index==0 || !multi)
      mCommandNameHash[name] = tmpEntry;

   if (tmpEntry->key != "")
      mCommandKeyHash[tmpEntry->key] = tmpEntry;

   return tmpEntry->id;
}

wxString CommandManager::GetKey(wxString label)
{
   int loc = -1;

   loc = label.Find('\t');
   if (loc == -1)
      loc = label.Find("\\t");

   if (loc == -1)
      return "";

   return label.Right(label.Length() - (loc+1));
}

///Enables or disables a menu item based on its name (not the
///label in the menu bar, but the name of the command.)
///If you give it the name of a multi-item (one that was
///added using AddItemList(), it will enable or disable all
///of them at once
void CommandManager::Enable(CommandListEntry *entry, bool enabled)
{
   // Don't do anything if the command's enabled state
   // is already the same
   if (entry->enabled == enabled)
      return;

   entry->enabled = enabled;

   if (!entry->menu)
      return;

   entry->menu->Enable(entry->id, enabled);
   if (entry->multi) {
      int i;
      int ID = entry->id;

      for(i=1; i<entry->count; i++) {
         ID = NextIdentifier(ID);
         entry->menu->Enable(ID, enabled);            
      }
   }
}

void CommandManager::Enable(wxString name, bool enabled)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry && entry->menu) {
      //printf("WARNING: Unknown command enabled: '%s'\n", (const char *)name);
      return;
   }

   Enable(entry, enabled);
}

void CommandManager::EnableUsingFlags(wxUint32 flags, wxUint32 mask)
{
   unsigned int i;

   for(i=0; i<mCommandList.GetCount(); i++) {
      CommandListEntry *entry = mCommandList[i];
      if (entry->multi && entry->index != 0)
         continue;

      wxUint32 combinedMask = (mask & entry->mask);
      if (combinedMask) {
         bool enable = ((flags & combinedMask) ==
                        (entry->flags & combinedMask));
         Enable(entry, enable);
      }
   }
}

///Changes the label text of a menu item
void CommandManager::Modify(wxString name, wxString newLabel)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry && entry->menu) {
      newLabel = newLabel.BeforeFirst('\t');
      if (entry->key)
        newLabel = newLabel + "\t" + entry->key;
      entry->menu->SetLabel(entry->id, newLabel);
   }
}

///Call this when a menu event is received.
///If it matches a command, it will call the appropriate
///CommandManagerListener function.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.
bool CommandManager::HandleMenuID(int id, wxUint32 flags, wxUint32 mask)
{
   CommandListEntry *entry = mCommandIDHash[id];
   if (!entry)
      return false;

   wxUint32 combinedMask = (mask & entry->mask);
   if (combinedMask) {
      bool allowed = ((flags & combinedMask) ==
                      (entry->flags & combinedMask));
      if (!allowed)
         return true;
   }
   
   (*(entry->callback))(entry->index);

   return true;
}

///Call this when a key event is received.
///If it matches a command, it will call the appropriate
///CommandManagerListener function.  If you pass any flags,
///the command won't be executed unless the flags are compatible
///with the command's flags.

bool CommandManager::HandleKey(wxKeyEvent &evt, wxUint32 flags, wxUint32 mask)
{
   wxString keyStr = KeyEventToKeyString(evt);
	CommandListEntry *entry = mCommandKeyHash[keyStr];
   if (!entry)
      return false;

   wxUint32 combinedMask = (mask & entry->mask);
   if (combinedMask) {
      bool allowed = ((flags & combinedMask) ==
                      (entry->flags & combinedMask));
      if (!allowed)
         return true;
   }

   (*(entry->callback))(entry->index);

   return true;
}

void CommandManager::GetAllCommandNames(wxArrayString &names,
                                        bool includeMultis)
{
   unsigned int i;

   for(i=0; i<mCommandList.GetCount(); i++) {
      if (includeMultis || !mCommandList[i]->multi)
         names.Add(mCommandList[i]->name);
   }
}

wxString CommandManager::GetLabelFromName(wxString name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return "";

   return entry->label;
}

wxString CommandManager::GetKeyFromName(wxString name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return "";
 
   return entry->key;
}

wxString CommandManager::GetDefaultKeyFromName(wxString name)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (!entry)
      return "";
 
   return entry->defaultKey;
}

bool CommandManager::HandleXMLTag(const char *tag, const char **attrs)
{
   if (!strcmp(tag, "audacitykeyboard")) {
      mXMLKeysRead = 0;
   }

   if (!strcmp(tag, "command")) {
      wxString name;
      wxString key;

      while(*attrs) {
         const char *attr = *attrs++;
         const char *value = *attrs++;
         
         if (!value)
            break;
         
         if (!strcmp(attr, "name"))
            name = value;
         if (!strcmp(attr, "key"))
            key = value;
      }

      if (mCommandNameHash[name]) {
         mCommandNameHash[name]->key = key;
         mXMLKeysRead++;
      }
   }

   return true;
}

void CommandManager::HandleXMLEndTag(const char *tag)
{
   if (!strcmp(tag, "audacitykeyboard")) {
      wxMessageBox(wxString::Format(_("Loaded %d "
                                      "keyboard shortcuts\n"),
                                    mXMLKeysRead),
                   _("Loading keyboard shortcuts"),
                   wxOK | wxCENTRE);
   }
}

XMLTagHandler *CommandManager::HandleXMLChild(const char *tag)
{
   return this;
}

void CommandManager::WriteXML(int depth, FILE *fp)
{
   int i;
   unsigned int j;

   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "<audacitykeyboard audacityversion=\"%s\">\n",
           AUDACITY_VERSION_STRING);
   for(j=0; j<mCommandList.GetCount(); j++)
      if (!mCommandList[j]->multi) {
         for(i=0; i<depth+1; i++)
            fprintf(fp, "\t");
         
         wxString label = mCommandList[j]->label;
         label = wxMenuItem::GetLabelFromText(label.BeforeFirst('\t'));
         
         fprintf(fp, "<command name=\"%s\" label=\"%s\" key=\"%s\" />\n",
                 XMLEsc(mCommandList[j]->name).c_str(),
                 XMLEsc(label).c_str(),
                 XMLEsc(mCommandList[j]->key).c_str());
      }
   for(i=0; i<depth; i++)
      fprintf(fp, "\t");
   fprintf(fp, "</audacitykeyboard>\n");
}

void CommandManager::SetDefaultFlags(wxUint32 flags, wxUint32 mask)
{
   mDefaultFlags = flags;
   mDefaultMask = mask;
}

void CommandManager::SetCommandFlags(wxString name,
                                     wxUint32 flags, wxUint32 mask)
{
   CommandListEntry *entry = mCommandNameHash[name];
   if (entry) {
      entry->flags = flags;
      entry->mask = mask;
   }
}

void CommandManager::SetCommandFlags(const char **names,
                                     wxUint32 flags, wxUint32 mask)
{
   const char **nptr = names;
   while(*nptr) {
      SetCommandFlags(wxString(*nptr), flags, mask);
      nptr++;
   }
}

void CommandManager::SetCommandFlags(wxUint32 flags, wxUint32 mask, ...)
{
   va_list list;
   va_start(list, mask); 
   for(;;) {
      const char *name = va_arg(list, const char *);
      if (!name)
         break;
      SetCommandFlags(wxString(name), flags, mask);
   }
   va_end(list);
}
