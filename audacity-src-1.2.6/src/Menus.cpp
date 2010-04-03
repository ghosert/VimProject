/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.cpp

  Dominic Mazzoni
  Brian Gunlogson
  et. al.

  This file implements the method that creates the menu bar, plus
  all of the methods that get called when you select an item
  from a menu.

**********************************************************************/

#include "Audacity.h"

#include <math.h>

#include <wx/defs.h>
#include <wx/docview.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>
#include <wx/textfile.h>
#include <wx/progdlg.h>
#include <wx/scrolbar.h>

#include "Project.h"

#include "AudioIO.h"
#include "LabelTrack.h"
#include "import/ImportMIDI.h"
#include "import/ImportRaw.h"
#include "export/Export.h"
#include "export/ExportMultiple.h"
#include "prefs/PrefsDialog.h"
#include "HistoryWindow.h"
#include "Internat.h"
#include "FileFormats.h"
#include "FormatSelection.h"
#include "FreqWindow.h"
#include "Prefs.h"
#include "NoteTrack.h"
#include "Tags.h"
#include "Mix.h"
#include "AboutDialog.h"
#include "Help.h"
#include "Benchmark.h"

// Printing
#include <wx/print.h>
#include <wx/printdlg.h>
#include "TrackArtist.h"
#include "widgets/Ruler.h"

#include "Resample.h"

enum {
   kAlignZero=0,
   kAlignCursor,
   kAlignSelStart,
   kAlignSelEnd,
   kAlignEndCursor,
   kAlignEndSelStart,
   kAlignEndSelEnd,
   kAlign
};

typedef void (AudacityProject::*audCommandFunction)();
typedef void (AudacityProject::*audCommandListFunction)(int);

class AudacityProjectCommandFunctor:public CommandFunctor
{
public:
   AudacityProjectCommandFunctor(AudacityProject *project,
                                 audCommandFunction commandFunction)
   {
      mProject = project;
      mCommandFunction = commandFunction;
      mCommandListFunction = NULL;
   }

   AudacityProjectCommandFunctor(AudacityProject *project,
                                 audCommandListFunction commandFunction)
   {
      mProject = project;
      mCommandFunction = NULL;
      mCommandListFunction = commandFunction;
   }

   virtual void operator()(int index = 0)
   {
      if (mCommandListFunction)
         (mProject->*(mCommandListFunction)) (index);
      else
         (mProject->*(mCommandFunction)) ();
   }

private:
   AudacityProject *mProject;
   audCommandFunction mCommandFunction;
   audCommandListFunction mCommandListFunction;
};

// These flags represent the majority of the states that affect
// whether or not items in menus are enabled or disabled.
enum {
   AudioIONotBusyFlag     = 0x00000001,
   TimeSelectedFlag       = 0x00000002,
   TracksSelectedFlag     = 0x00000004,
   TracksExistFlag        = 0x00000008,
   LabelTracksExistFlag   = 0x00000010,
   WaveTracksSelectedFlag = 0x00000020,
   ClipboardFlag          = 0x00000040,
   UnsavedChangesFlag     = 0x00000080,
   HasLastEffectFlag      = 0x00000100,
   UndoAvailableFlag      = 0x00000200,
   RedoAvailableFlag      = 0x00000400,
   ZoomInAvailableFlag    = 0x00000800,
   ZoomOutAvailableFlag   = 0x00001000
};

#define FN(X) new AudacityProjectCommandFunctor(this, &AudacityProject:: X )

void AudacityProject::CreateMenusAndCommands()
{
   CommandManager *c = &mCommandManager;
   EffectArray *effects;
   wxArrayString names;
   unsigned int i;

   wxMenuBar *menubar = c->AddMenuBar("appmenu");

   //
   // File menu
   //

   c->BeginMenu(_("&File"));
   c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);
   c->AddItem("New",            _("&New\tCtrl+N"),                   FN(OnNew));
   c->SetCommandFlags("New", 0, 0);
   c->AddItem("Open",           _("&Open...\tCtrl+O"),               FN(OnOpen));
   c->SetCommandFlags("Open", 0, 0);
   c->AddItem("Close",          _("&Close\tCtrl+W"),                 FN(OnClose));
   c->AddItem("Save",           _("&Save Project\tCtrl+S"),          FN(OnSave));
   c->SetCommandFlags("Save",
                      AudioIONotBusyFlag | UnsavedChangesFlag,
                      AudioIONotBusyFlag | UnsavedChangesFlag);
   c->AddItem("SaveAs",         _("Save Project &As..."),            FN(OnSaveAs));
   c->AddSeparator();

   // Recent Files and Recent Projects menus
   wxMenu* pm = c->BeginSubMenu(_("Recent &Files..."));
   c->EndSubMenu();
   // TODO - read the number of files to store in history from preferences
   mRecentFiles = new wxFileHistory();
   mRecentFiles->UseMenu(pm);
   gPrefs->SetPath("/RecentFiles");
   mRecentFiles->Load(*gPrefs);
   gPrefs->SetPath("..");
   c->AddSeparator();

   c->AddItem("Export",         _("Export As..."),                   FN(OnExportMix));
   c->AddItem("ExportSel",      _("Export Selection As..."),         FN(OnExportSelection));
   c->AddSeparator();
   c->AddItem("ExportMP3",      _("Export As MP3..."),               FN(OnExportMP3Mix));
   c->AddItem("ExportMP3Sel",   _("Export Selection As MP3..."),     FN(OnExportMP3Selection));
   c->AddSeparator();
   c->AddItem("ExportOgg",      _("Export As Ogg Vorbis..."),        FN(OnExportOggMix));
   c->AddItem("ExportOggSel",   _("Export Selection As Ogg Vorbis..."), FN(OnExportOggSelection));
   c->AddSeparator();
   c->AddItem("ExportLabels",   _("Export &Labels..."),              FN(OnExportLabels));
   c->AddItem("ExportMultiple",   _("Export &Multiple..."),              FN(OnExportMultiple));
   // Enable Export commands only when there are tracks
   c->SetCommandFlags(AudioIONotBusyFlag | TracksExistFlag,
                      AudioIONotBusyFlag | TracksExistFlag,
                      "Export", "ExportMP3", "ExportOgg", NULL);
   // Enable Export Selection commands only when there's a selection
   c->SetCommandFlags(AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag,
                      "ExportSel", "ExportMP3Sel", "ExportOggSel", NULL);

   c->SetCommandFlags("ExportLabels",
                      AudioIONotBusyFlag | LabelTracksExistFlag,
                      AudioIONotBusyFlag | LabelTracksExistFlag);
   c->SetCommandFlags("ExportMultiple",
                      AudioIONotBusyFlag | TracksExistFlag,
                      AudioIONotBusyFlag | TracksExistFlag);

   c->AddSeparator();
   c->AddItem("PageSetup",   _("Page Setup..."),              FN(OnPageSetup));
   c->AddItem("Print",       _("Print..."),                   FN(OnPrint));
   c->SetCommandFlags("PageSetup",
                      AudioIONotBusyFlag | TracksExistFlag,
                      AudioIONotBusyFlag | TracksExistFlag);
   c->SetCommandFlags("Print",
                      AudioIONotBusyFlag | TracksExistFlag,
                      AudioIONotBusyFlag | TracksExistFlag);

   // On Mac OS X, Preferences and Quit are in the application menu,
   // not the File menu.  In wx 2.4 and lower, we handle this by
   // not including these menus at all.  In wx 2.5 and higher, we
   // include them, but wxMac automatically moves them to the appropriate
   // place.
   // Moved Preferences to Edit Menu 02/09/05 Richard Ash.

#ifdef __WXMAC__
 #if ((wxMAJOR_VERSION == 2) && (wxMINOR_VERSION <= 4))
   // Leave these menus out entirely...
 #else
   c->AddItem("Exit",           _("E&xit"),                          FN(OnExit));
   c->SetCommandFlags("Exit", 0, 0);
 #endif
#else
   c->AddSeparator();

   c->AddItem("Exit",           _("E&xit"),                          FN(OnExit));
   c->SetCommandFlags("Exit", 0, 0);
  #endif
   c->EndMenu();

   //
   // Edit Menu
   //

   c->BeginMenu(_("&Edit"));
   c->SetDefaultFlags(AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TimeSelectedFlag | TracksSelectedFlag);

   c->AddItem("Undo",           _("&Undo\tCtrl+Z"),                  FN(OnUndo));
   c->SetCommandFlags("Undo",
                      AudioIONotBusyFlag | UndoAvailableFlag,
                      AudioIONotBusyFlag | UndoAvailableFlag);

   // The default shortcut key for Redo is different
   // on different platforms.

   wxString redoLabel = _("&Redo");
   #ifdef __WXMSW__
   redoLabel += "\tCtrl+Y";
   #else
   redoLabel += "\tCtrl+Shift+Z";
   #endif

   c->AddItem("Redo",           redoLabel,                           FN(OnRedo));
   c->SetCommandFlags("Redo",
                      AudioIONotBusyFlag | RedoAvailableFlag,
                      AudioIONotBusyFlag | RedoAvailableFlag);

   c->AddSeparator();
   c->AddItem("Cut",            _("Cu&t\tCtrl+X"),                   FN(OnCut));
   c->AddItem("Copy",           _("&Copy\tCtrl+C"),                  FN(OnCopy)); 
   c->AddItem("Paste",          _("&Paste\tCtrl+V"),                 FN(OnPaste));
   c->SetCommandFlags("Paste",
                      AudioIONotBusyFlag | ClipboardFlag,
                      AudioIONotBusyFlag | ClipboardFlag);
   c->AddItem("Trim",           _("&Trim\tCtrl+T"),                  FN(OnTrim));
   c->AddSeparator();
   c->AddItem("Delete",         _("&Delete\tCtrl+K"),                FN(OnDelete));
   c->AddItem("Silence",        _("&Silence\tCtrl+L"),               FN(OnSilence));
   c->AddSeparator();
   c->AddItem("Split",          _("Spl&it"),                         FN(OnSplit));
   c->AddItem("Duplicate",      _("D&uplicate\tCtrl+D"),             FN(OnDuplicate));

   c->AddSeparator();

   c->BeginSubMenu(_("Select..."));
   c->AddItem("SelectAll",      _("&All\tCtrl+A"),                   FN(OnSelectAll));
   c->SetCommandFlags("SelectAll",
                      TracksExistFlag, TracksExistFlag);
   c->AddItem("SelStartCursor", _("Start to Cursor"),                FN(OnSelectStartCursor));
   c->AddItem("SelCursorEnd",   _("Cursor to End"),                  FN(OnSelectCursorEnd));
   c->SetCommandFlags(TracksSelectedFlag, TracksSelectedFlag,
                      "SelStartCursor", "SelCursorEnd", NULL);

   c->EndSubMenu();
   c->AddItem("ZeroCross",      _("Find Zero Crossings\tZ"),         FN(OnZeroCrossing));
   c->AddSeparator();
   c->AddItem("SelSave",        _("Selection Save"),                 FN(OnSelectionSave));
   c->AddItem("SelRestore",     _("Selection Restore"),              FN(OnSelectionRestore));
   c->SetCommandFlags(TracksExistFlag, TracksExistFlag,
                      "SelSave", "SelRestore", NULL);
   c->AddSeparator();

   c->BeginSubMenu(_("Move Cursor..."));

   c->AddItem("CursTrackStart", _("to Track Start"),                 FN(OnCursorTrackStart));
   c->AddItem("CursTrackEnd",   _("to Track End"),                   FN(OnCursorTrackEnd));
   c->SetCommandFlags(TracksSelectedFlag, TracksSelectedFlag,
                      "CursTrackStart", "CursTrackEnd", NULL);
   c->AddItem("CursSelStart",   _("to Selection Start"),             FN(OnCursorSelStart));
   c->AddItem("CursSelEnd",     _("to Selection End"),               FN(OnCursorSelEnd));
   c->SetCommandFlags(TimeSelectedFlag, TimeSelectedFlag,
                      "CursSelStart", "CursSelEnd", NULL);
                      
   c->EndSubMenu();

   c->BeginSubMenu(_("Snap-To..."));

   /* i18n-hint: Set snap-to mode on or off */
   c->AddItem("SnapOn",         _("Snap On"),                        FN(OnSnapOn));
   /* i18n-hint: Set snap-to mode on or off */
   c->AddItem("SnapOff",        _("Snap Off"),                       FN(OnSnapOff));

   c->SetCommandFlags(0, 0, "SnapOn", "SnapOff", NULL);

   c->EndSubMenu();

   // Alternate strings
   wxString dummy1 = _("Turn Snap-To On");
   wxString dummy2 = _("Turn Snap-To Off");
   wxString dummy3 = _("Turn Grid Snap On");
   wxString dummy4 = _("Turn Grid Snap Off");

   // On Mac OS X, Preferences and Quit are in the application menu,
   // not the File menu.  In wx 2.4 and lower, we handle this by
   // not including these menus at all.  In wx 2.5 and higher, we
   // include them, but wxMac automatically moves them to the appropriate
   // place.
   // Moved Preferences from File Menu 02/09/05 Richard Ash.

#ifdef __WXMAC__
 #if ((wxMAJOR_VERSION == 2) && (wxMINOR_VERSION <= 4))
   // Leave these menus out entirely...
 #else
   c->AddItem("Preferences",    _("&Preferences...\tCtrl+P"),        FN(OnPreferences));
   c->SetCommandFlags("Preferences", AudioIONotBusyFlag, AudioIONotBusyFlag);

 #endif
#else
   c->AddSeparator();
   c->AddItem("Preferences",    _("&Preferences...\tCtrl+P"),        FN(OnPreferences));
   c->SetCommandFlags("Preferences", AudioIONotBusyFlag, AudioIONotBusyFlag);
   /* enabled whenever transport isn't running */
  #endif

   
   c->EndMenu();

   //
   // View Menu
   //

   c->BeginMenu(_("&View"));
   c->SetDefaultFlags(0, 0);
   c->AddItem("ZoomIn",         _("Zoom &In\tCtrl+1"),               FN(OnZoomIn));
   c->SetCommandFlags("ZoomIn", ZoomInAvailableFlag, ZoomInAvailableFlag);

   c->AddItem("ZoomNormal",     _("Zoom &Normal\tCtrl+2"),           FN(OnZoomNormal));
   c->AddItem("ZoomOut",        _("Zoom &Out\tCtrl+3"),              FN(OnZoomOut));
   c->SetCommandFlags("ZoomOut", ZoomOutAvailableFlag, ZoomOutAvailableFlag);

   c->AddItem("FitInWindow",    _("&Fit in Window\tCtrl+F"),         FN(OnZoomFit));
   c->AddItem("FitV",           _("Fit &Vertically\tCtrl+Shift+F"),  FN(OnZoomFitV));
   c->AddItem("ZoomSel",        _("&Zoom to Selection\tCtrl+E"),     FN(OnZoomSel));
   c->AddSeparator();

   c->BeginSubMenu(_("Set Selection Format"));
   c->AddItemList("SelectionFormat", GetSelectionFormats(), FN(OnSelectionFormat));
   c->EndSubMenu();

   c->AddSeparator();
   c->AddItem("UndoHistory",    _("&History..."),               FN(OnHistory));
   c->AddSeparator();
   c->AddItem("FloatControlTB", _("Float Control Toolbar"),          FN(OnFloatControlToolBar));
   c->AddItem("FloatEditTB",    _("Float Edit Toolbar"),             FN(OnFloatEditToolBar));
   c->AddItem("FloatMixerTB",   _("Float Mixer Toolbar"),            FN(OnFloatMixerToolBar));
   c->AddItem("FloatMeterTB",   _("Float Meter Toolbar"),            FN(OnFloatMeterToolBar));

   c->SetCommandFlags("FloatMeterTB", AudioIONotBusyFlag, AudioIONotBusyFlag);

   c->EndMenu();

   //
   // Project Menu
   //

   c->BeginMenu(_("&Project"));
   c->SetDefaultFlags(AudioIONotBusyFlag, AudioIONotBusyFlag);
   c->AddItem("ImportAudio",    _("&Import Audio...\tCtrl+I"),       FN(OnImport));
   c->AddItem("ImportLabels",   _("Import &Labels..."),              FN(OnImportLabels));
   c->AddItem("ImportMIDI",     _("Import &MIDI..."),                FN(OnImportMIDI));
   c->AddItem("ImportRaw",      _("Import &Raw Data..."),            FN(OnImportRaw));
   c->AddSeparator();
   c->AddItem("EditID3",        _("&Edit ID3 Tags..."),              FN(OnEditID3));
   c->AddSeparator();
   c->AddItem("QuickMix",       _("&Quick Mix"),                     FN(OnQuickMix));
   c->SetCommandFlags("QuickMix",
                      AudioIONotBusyFlag | WaveTracksSelectedFlag,
                      AudioIONotBusyFlag | WaveTracksSelectedFlag);
   c->AddSeparator();
   c->AddItem("NewAudioTrack",  _("New &Audio Track"),               FN(OnNewWaveTrack));
   c->AddItem("NewStereoTrack", _("New &Stereo Track"),              FN(OnNewStereoTrack));
   c->AddItem("NewLabelTrack",  _("New La&bel Track"),               FN(OnNewLabelTrack));
   c->AddItem("NewTimeTrack",   _("New &Time Track"),                FN(OnNewTimeTrack));
   c->AddSeparator();
   c->AddItem("RemoveTracks",   _("Remo&ve Tracks"),                 FN(OnRemoveTracks));
   c->SetCommandFlags("RemoveTracks",
                      AudioIONotBusyFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag);
   c->AddSeparator();

   wxArrayString alignLabels;
   alignLabels.Add(_("Align with &Zero"));
   alignLabels.Add(_("Align with &Cursor"));
   alignLabels.Add(_("Align with Selection &Start"));
   alignLabels.Add(_("Align with Selection &End"));
   alignLabels.Add(_("Align End with Cursor"));
   alignLabels.Add(_("Align End with Selection Start"));
   alignLabels.Add(_("Align End with Selection End"));
   alignLabels.Add(_("Align Tracks Together"));

   c->BeginSubMenu(_("Align Tracks..."));
   c->AddItemList("Align", alignLabels, FN(OnAlign));
   c->SetCommandFlags("Align",
                      AudioIONotBusyFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag);
   c->EndSubMenu();

   alignLabels.Remove(7); // Can't align together and move cursor

   c->BeginSubMenu(_("Align and move cursor..."));
   c->AddItemList("AlignMove", alignLabels, FN(OnAlignMoveSel));
   c->SetCommandFlags("AlignMove",
                      AudioIONotBusyFlag | TracksSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag);
   c->EndSubMenu();

   c->AddSeparator();   
   c->AddItem("AddLabel",       _("Add Label At Selection\tCtrl+B"), FN(OnAddLabel));
   c->AddItem("AddLabelPlaying",       _("Add Label At Playback Position\tCtrl+M"), FN(OnAddLabelPlaying));
   c->SetCommandFlags("AddLabel", 0, 0);
   c->SetCommandFlags("AddLabelPlaying", 0, AudioIONotBusyFlag);
   c->EndMenu();

   //
   // Generate, Effect & Analyze menus
   //

   c->BeginMenu(_("&Generate"));
   c->SetDefaultFlags(AudioIONotBusyFlag,
                      AudioIONotBusyFlag);

   effects = Effect::GetEffects(INSERT_EFFECT | BUILTIN_EFFECT);
   if(effects->GetCount()){
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("Generate", names, FN(OnGenerateEffect));
   }
   delete effects;

   effects = Effect::GetEffects(INSERT_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("GeneratePlugin", names, FN(OnGeneratePlugin), true);
   }
   delete effects;
   c->EndMenu();

   c->BeginMenu(_("Effe&ct"));
   c->SetDefaultFlags(AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag,
                      AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag);

   c->AddItem("RepeatLastEffect",     _("Repeat Last Effect\tCtrl+R"),    FN(OnRepeatLastEffect));
   c->SetCommandFlags("RepeatLastEffect",
                      AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag | HasLastEffectFlag,
                      AudioIONotBusyFlag | TimeSelectedFlag | WaveTracksSelectedFlag | HasLastEffectFlag);
   c->AddSeparator();

   effects = Effect::GetEffects(PROCESS_EFFECT | BUILTIN_EFFECT);
   if(effects->GetCount()){
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("Effect", names, FN(OnProcessEffect));
   }
   delete effects;

   effects = Effect::GetEffects(PROCESS_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("EffectPlugin", names, FN(OnProcessPlugin), true);
   }
   delete effects;
   c->EndMenu();

   c->BeginMenu(_("&Analyze"));
	/* plot spectrum moved from view */
   c->AddItem("PlotSpectrum",   _("&Plot Spectrum"),                 FN(OnPlotSpectrum));
   c->SetCommandFlags("PlotSpectrum",
                      AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag,
                      AudioIONotBusyFlag | WaveTracksSelectedFlag | TimeSelectedFlag);	
   effects = Effect::GetEffects(ANALYZE_EFFECT | BUILTIN_EFFECT);
   if(effects->GetCount()){
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("Analyze", names, FN(OnAnalyzeEffect));
   }
   delete effects;

   effects = Effect::GetEffects(ANALYZE_EFFECT | PLUGIN_EFFECT);
   if (effects->GetCount()) {
      c->AddSeparator();
      names.Clear();
      for(i=0; i<effects->GetCount(); i++)
         names.Add((*effects)[i]->GetEffectName());
      c->AddItemList("AnalyzePlugin", names, FN(OnAnalyzePlugin), true);
   }
   delete effects;
   c->EndMenu();

   /* i18n-hint: The name of the Help menu */
   c->BeginMenu(_("&Help"));
   c->SetDefaultFlags(0, 0);
   c->AddItem("Help",           _("&Contents..."),             FN(OnHelp));
   /* i18n-hint: The option to read the installed help file */
   c->AddSeparator();   
   c->AddItem("About",          _("&About Audacity..."),          FN(OnAbout));

#if 0 // No Benchmark in stable release
   c->AddSeparator();   
   c->AddItem("Benchmark",      _("&Run Benchmark..."),           FN(OnBenchmark));
#endif 

   c->EndMenu();

#ifdef __WXMAC__
 #if (!((wxMAJOR_VERSION == 2) && (wxMINOR_VERSION <= 4)))
   wxGetApp().s_macHelpMenuTitleName = _("&Help");
 #endif
#endif

   ModifyExportMenus();

   SetMenuBar(menubar);

   c->SetDefaultFlags(0, 0);
   c->AddCommand("SelectTool",  _("Selection Tool\tF1"),          FN(OnSelectTool));
   c->AddCommand("EnvelopeTool",_("Envelope Tool\tF2"),           FN(OnEnvelopeTool));
   c->AddCommand("DrawTool",    _("Draw Tool\tF3"),               FN(OnDrawTool));
   c->AddCommand("ZoomTool",    _("Zoom Tool\tF4"),               FN(OnZoomTool));
   c->AddCommand("TimeShiftTool",_("Time Shift Tool\tF5"),        FN(OnTimeShiftTool));
   c->AddCommand("MultiTool",   _("Multi Tool\tF6"),              FN(OnMultiTool));

   c->AddCommand("NextTool",   _("Next Tool\tD"),                 FN(OnNextTool));
   c->AddCommand("PrevTool",   _("Previous Tool\tA"),             FN(OnPrevTool));

   c->AddCommand("Play/Stop",   _("Play/Stop\tSpacebar"),         FN(OnPlayStop));
   c->AddCommand("Stop",        _("Stop\tS"),                     FN(OnStop));
   c->AddCommand("Pause",       _("Pause\tP"),                    FN(OnPause));
   c->AddCommand("Record",      _("Record\tR"),                   FN(OnRecord));
   
   c->AddCommand("PlayOneSec",     _("Play One Second\t1"),       FN(OnPlayOneSecond));
   c->AddCommand("PlayToSelection",_("Play To Selection\tB"),       FN(OnPlayToSelection));
   c->AddCommand("PlayLooped",     _("Play Looped\tL"),           FN(OnPlayLooped));
   c->AddCommand("PlayLoopAlt",    _("Play Looped\tShift+Spacebar"), FN(OnPlayLooped));

   c->AddCommand("SkipStart",   _("Skip to Start\tHome"),         FN(OnSkipStart));
   c->AddCommand("SkipEnd",     _("Skip to End\tEnd"),            FN(OnSkipEnd));

   c->AddCommand("SelStart",    _("Selection to Start\tShift+Home"), FN(OnSelToStart));
   c->AddCommand("SelEnd",      _("Selection to End\tShift+End"),    FN(OnSelToEnd));

   c->AddCommand("DeleteKey",      _("DeleteKey\tBackspace"),           FN(OnDelete));
   c->SetCommandFlags("DeleteKey",
                      AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag);

   c->AddCommand("DeleteKey2",      _("DeleteKey2\tDelete"),           FN(OnDelete));
   c->SetCommandFlags("DeleteKey2",
                      AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag,
                      AudioIONotBusyFlag | TracksSelectedFlag | TimeSelectedFlag);

   c->AddCommand("CursorLeft",  _("Cursor Left\tLeft"),           FN(OnCursorLeft));
   c->AddCommand("CursorRight", _("Cursor Right\tRight"),         FN(OnCursorRight));
   c->AddCommand("SelExtLeft",  _("Selection Extend Left\tShift+Left"),     FN(OnSelExtendLeft));
   c->AddCommand("SelExtRight", _("Selection Extend Right\tShift+Right"),   FN(OnSelExtendRight));
   c->AddCommand("SelCntrLeft", _("Selection Contract Left\tCtrl+Shift+Right"),   FN(OnSelContractLeft));
   c->AddCommand("SelCntrRight",_("Selection Contract Right\tCtrl+Shift+Left"), FN(OnSelContractRight));

   mLastFlags = 0;
   mLastToolBarCheckSum = 0;

   mSel0save = 0;
   mSel1save = 0;
}

void AudacityProject::ModifyExportMenus()
{
   int format = ReadExportFormatPref();
   wxString pcmFormat = sf_header_shortname(format & SF_FORMAT_TYPEMASK);

   mCommandManager.Modify("Export",
                          wxString::Format(_("&Export As %s..."),
                                           (const char *)pcmFormat));
   mCommandManager.Modify("ExportSel",
                          wxString::Format(_("&Export Selection As %s..."),
                                           (const char *)pcmFormat));
}

void AudacityProject::ModifyUndoMenus()
{
   wxString desc;
   int cur = mUndoManager.GetCurrentState();

   if (mUndoManager.UndoAvailable()) {
      mUndoManager.GetShortDescription(cur, &desc);
      mCommandManager.Modify("Undo",
                             wxString::Format(_("&Undo %s"),
                                              (const char *)desc));
      mCommandManager.Enable("Undo", true);
   }
   else {
      mCommandManager.Modify("Undo", 
                             wxString::Format(_("Can't Undo")));
      mCommandManager.Enable("Undo", false);
   }

   if (mUndoManager.RedoAvailable()) {
      mUndoManager.GetShortDescription(cur+1, &desc);
      mCommandManager.Modify("Redo",
                             wxString::Format(_("&Redo %s"),
                                              (const char *)desc));
      mCommandManager.Enable("Redo", true);
   }
   else {
      mCommandManager.Modify("Redo",
                             wxString::Format(_("Can't Redo")));
      mCommandManager.Enable("Redo", false);
   }
}

void AudacityProject::RebuildMenuBar()
{
   wxMenuBar *menuBar = GetMenuBar();
   DetachMenuBar();
   delete menuBar;
   mCommandManager.PurgeData();
   CreateMenusAndCommands();
}

wxUint32 AudacityProject::GetUpdateFlags()
{
   // This method determines all of the flags that determine whether
   // certain menu items and commands should be enabled or disabled,
   // and returns them in a bitfield.  Note that if none of the flags
   // have changed, it's not necessary to even check for updates.
   wxUint32 flags = 0;

   if (GetAudioIOToken() == 0 ||
       !gAudioIO->IsAudioTokenActive(GetAudioIOToken()))
      flags |= AudioIONotBusyFlag;

   if (mViewInfo.sel1 > mViewInfo.sel0)
      flags |= TimeSelectedFlag;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      flags |= TracksExistFlag;
      if (t->GetKind() == Track::Label)
         flags |= LabelTracksExistFlag;
      if (t->GetSelected()) {
         flags |= TracksSelectedFlag;
         if (t->GetKind() == Track::Wave && t->GetLinked() == false)
            flags |= WaveTracksSelectedFlag;
      }
      t = iter.Next();
   }

   if (msClipLen > 0.0)
      flags |= ClipboardFlag;

   if (mUndoManager.UnsavedChanges())
      flags |= UnsavedChangesFlag;

   if (Effect::GetLastEffect() != NULL)
      flags |= HasLastEffectFlag;

   if (mUndoManager.UndoAvailable())
      flags |= UndoAvailableFlag;

   if (mUndoManager.RedoAvailable())
      flags |= RedoAvailableFlag;

   if (GetZoom() < gMaxZoom && (flags & TracksExistFlag))
      flags |= ZoomInAvailableFlag;

   if (GetZoom() > gMinZoom && (flags & TracksExistFlag))
      flags |= ZoomOutAvailableFlag;

   return flags;
}

int AudacityProject::GetToolBarChecksum()
{
   //Calculate the ToolBarCheckSum (uniquely specifies state of all toolbars):
   int toolBarCheckSum = 0;
   if (gControlToolBarStub->GetWindowedStatus())
      toolBarCheckSum += 1;
   if (gEditToolBarStub) {
      if (gEditToolBarStub->GetLoadedStatus()) {
         if(gEditToolBarStub->GetWindowedStatus())
            toolBarCheckSum += 2;
      }
   }
   if (gMixerToolBarStub) {
      if (gMixerToolBarStub->GetLoadedStatus()) {
         if(gMixerToolBarStub->GetWindowedStatus())
            toolBarCheckSum += 4;
      }
   }
   if (gMeterToolBarStub) {
      if (gMeterToolBarStub->GetLoadedStatus()) {
         if(gMeterToolBarStub->GetWindowedStatus())
            toolBarCheckSum += 8;
      }
   }

   return toolBarCheckSum;
}

void AudacityProject::ModifyToolbarMenus()
{
   if (gEditToolBarStub) {

     // Loaded or unloaded?
     mCommandManager.Enable("FloatEditTB", gEditToolBarStub->GetLoadedStatus());

     // Floating or docked?
     if (gEditToolBarStub->GetWindowedStatus())
        mCommandManager.Modify("FloatEditTB", _("Dock Edit Toolbar"));
     else
        mCommandManager.Modify("FloatEditTB", _("Float Edit Toolbar"));
   }
   else {
      mCommandManager.Enable("FloatEditTB", false);
   }   

   if (gMixerToolBarStub) {
     
     // Loaded or unloaded?
     mCommandManager.Enable("FloatMixerTB", gMixerToolBarStub->GetLoadedStatus());
     
     // Floating or docked?
     if (gMixerToolBarStub->GetWindowedStatus())
        mCommandManager.Modify("FloatMixerTB", _("Dock Mixer Toolbar"));
     else
        mCommandManager.Modify("FloatMixerTB", _("Float Mixer Toolbar"));
   }
   else {
      mCommandManager.Enable("FloatMixerTB", false);
   }   

   if (gMeterToolBarStub) {
     
     // Loaded or unloaded?
     mCommandManager.Enable("FloatMeterTB", gMeterToolBarStub->GetLoadedStatus());
     
     // Floating or docked?
     if (gMeterToolBarStub->GetWindowedStatus())
        mCommandManager.Modify("FloatMeterTB", _("Dock Meter Toolbar"));
     else
        mCommandManager.Modify("FloatMeterTB", _("Float Meter Toolbar"));
   }
   else {
      mCommandManager.Enable("FloatMeterTB", false);
   }   
}

void AudacityProject::UpdateMenus()
{
   if (this != GetActiveProject())
      return;

   if (gControlToolBarStub) {
      int toolBarCheckSum = GetToolBarChecksum();

      if (mLastToolBarCheckSum != toolBarCheckSum) {
         mLastToolBarCheckSum = toolBarCheckSum;
         ModifyToolbarMenus();
      }
   }

   // Return from this function if nothing's changed since
   // the last time we were here.
   wxUint32 flags = GetUpdateFlags();
   if (flags == mLastFlags)
      return;

   mLastFlags = flags;
   mCommandManager.EnableUsingFlags(flags, 0xFFFFFFFF);
   
   //Now, go through each toolbar, and and call EnableDisableButtons()
   unsigned int i;
   for (i = 0; i < mToolBarArray.GetCount(); i++) {
      mToolBarArray[i]->EnableDisableButtons();
   }

   //Now, do the same thing for the (possibly invisible) floating toolbars
   ToolBar *tb1 = gControlToolBarStub->GetToolBar();
   if (tb1)
      tb1->EnableDisableButtons();
   if(gEditToolBarStub) {
      tb1 = gEditToolBarStub->GetToolBar();
      if (tb1)
         tb1->EnableDisableButtons();
   }
}

//
// Tool selection commands
//

void AudacityProject::OnSelectTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(selectTool, true);
}

void AudacityProject::OnZoomTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(zoomTool, true);
}

void AudacityProject::OnEnvelopeTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(envelopeTool, true);
}

void AudacityProject::OnTimeShiftTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(slideTool, true);
}

void AudacityProject::OnDrawTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(drawTool, true);
}

void AudacityProject::OnMultiTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool(multiTool, true);
}


void AudacityProject::OnNextTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool((toolbar->GetCurrentTool()+1)%numTools, true);
}

void AudacityProject::OnPrevTool()
{
   ControlToolBar *toolbar = GetControlToolBar();
   if (toolbar)
      toolbar->SetCurrentTool((toolbar->GetCurrentTool()+(numTools-1))%numTools, true);
}


//
// Audio I/O Commands
//

// TODO: Should all these functions which involve
// the toolbar actually move into ControlToolBar?

/// MakeReadyToPlay stops whatever is currently playing 
/// and pops the play button up.  Then, if nothing is now
/// playing, it pushes the play button down and enables
/// the stop button.
bool AudacityProject::MakeReadyToPlay()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   // If this project is playing, stop playing
   if (gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->OnStop(evt);

      ::wxUsleep(100);
   }

   // If it didn't stop playing quickly, or if some other
   // project is playing, return
   if (gAudioIO->IsBusy())
      return false;

   toolbar->SetPlay(true);
   toolbar->SetStop(false);

   return true;
}

void AudacityProject::OnPlayOneSecond()
{
   if( !MakeReadyToPlay() )
      return;

   ControlToolBar *toolbar = GetControlToolBar();
   double pos = mTrackPanel->GetMostRecentXPos();
   mLastPlayMode = oneSecondPlay;
   toolbar->PlayPlayRegion(pos - 0.5, pos + 0.5);
}


/// The idea for this function (and first implementation)
/// was from Juhana Sadeharju.  The function plays the 
/// sound between the current mouse position and the 
/// nearest selection boundary.  This gives four possible 
/// play regions depending on where the current mouse 
/// position is relative to the left and right boundaries 
/// of the selection region.
void AudacityProject::OnPlayToSelection()
{
   if( !MakeReadyToPlay() )
      return;

   ControlToolBar *toolbar = GetControlToolBar();
   double pos = mTrackPanel->GetMostRecentXPos();

   double t0,t1;
   // check region between pointer and the nearest selection edge
   if (fabs(pos - mViewInfo.sel0) < fabs(pos - mViewInfo.sel1)) {
      t0 = t1 = mViewInfo.sel0;
   } else {
      t0 = t1 = mViewInfo.sel1;
   }
   if( pos < t1) 
      t0=pos;
   else
      t1=pos;

   // JKC: oneSecondPlay mode disables auto scrolling
   // On balance I think we should always do this in this function
   // since you are typically interested in the sound EXACTLY 
   // where the cursor is.
   // TODO: have 'playing attributes' such as 'with_autoscroll'
   // rather than modes, since that's how we're now using the modes.
   mLastPlayMode = oneSecondPlay;

   // An alternative, commented out below, is to disable autoscroll
   // only when playing a short region, less than or equal to a second.
//   mLastPlayMode = ((t1-t0) > 1.0) ? normalPlay : oneSecondPlay;

   toolbar->PlayPlayRegion(t0, t1);
}

void AudacityProject::OnPlayLooped()
{
   if( !MakeReadyToPlay() )
      return;

   // Now play in a loop
   // Will automatically set mLastPlayMode
   ControlToolBar *toolbar = GetControlToolBar();
   toolbar->PlayCurrentRegion(true);
}

void AudacityProject::OnPlayStop()
{
   ControlToolBar *toolbar = GetControlToolBar();

   //If busy, stop playing, make sure everything is unpaused.
   if (gAudioIO->IsStreamActive(GetAudioIOToken())) {
      toolbar->SetPlay(false);        //Pops
      toolbar->SetStop(true);         //Pushes stop down
      toolbar->StopPlaying();
   }
   else if (!gAudioIO->IsBusy()) {
      //Otherwise, start playing (assuming audio I/O isn't busy)
      toolbar->SetPlay(true);
      toolbar->SetStop(false);

      // Will automatically set mLastPlayMode
      toolbar->PlayCurrentRegion(false);
   }
}

void AudacityProject::OnStop()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   if (gAudioIO->IsStreamActive())
      toolbar->OnStop(evt);
}

void AudacityProject::OnPause()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnPause(evt);
}

void AudacityProject::OnRecord()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnRecord(evt);
}

void AudacityProject::OnSkipStart()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnRewind(evt);
}

void AudacityProject::OnSkipEnd()
{
   ControlToolBar *toolbar = GetControlToolBar();
   wxCommandEvent evt;

   toolbar->OnFF(evt);
}

void AudacityProject::OnSelToStart()
{
   mViewInfo.sel0 = 0;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelToEnd()
{
   SkipEnd(true);
}

void AudacityProject::OnCursorLeft()
{
   if (mViewInfo.sel0 == mViewInfo.sel1) {
      mViewInfo.sel0 -= 1/mViewInfo.zoom;
      mViewInfo.sel1 -= 1/mViewInfo.zoom;
      if (mViewInfo.sel0 < 0.0)
         mViewInfo.sel0 = 0.0;
      if (mViewInfo.sel1 < 0.0)
         mViewInfo.sel1 = 0.0;
   }
   else
      mViewInfo.sel1 = mViewInfo.sel0;

   mTrackPanel->Refresh(false);   
}

void AudacityProject::OnCursorRight()
{
   if (mViewInfo.sel0 == mViewInfo.sel1) {
      mViewInfo.sel0 += 1/mViewInfo.zoom;
      mViewInfo.sel1 += 1/mViewInfo.zoom;
   }
   else
      mViewInfo.sel0 = mViewInfo.sel1;

   mTrackPanel->Refresh(false);   
}

void AudacityProject::OnSelExtendLeft()
{
   mViewInfo.sel0 -= 1/mViewInfo.zoom;
   if (mViewInfo.sel0 < 0.0)
      mViewInfo.sel0 = 0.0;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelExtendRight()
{
   mViewInfo.sel1 += 1/mViewInfo.zoom;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelContractLeft()
{
   mViewInfo.sel0 += 1/mViewInfo.zoom;
   if (mViewInfo.sel0 > mViewInfo.sel1)
      mViewInfo.sel0 = mViewInfo.sel1;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelContractRight()
{
   mViewInfo.sel1 -= 1/mViewInfo.zoom;
   if (mViewInfo.sel1 < mViewInfo.sel0)
      mViewInfo.sel1 = mViewInfo.sel0;
   mTrackPanel->Refresh(false);
}

double AudacityProject::NearestZeroCrossing(double t0)
{
   int windowSize = (int)(GetRate() / 100);
   float *dist = new float[windowSize];
   int i, j;

   for(i=0; i<windowSize; i++)
      dist[i] = 0.0;

   TrackListIterator iter(mTracks);
   Track *track = iter.First();
   while (track) {
      if (!track->GetSelected() || track->GetKind() != (Track::Wave)) {
         track = iter.Next();
         continue;
      }
      WaveTrack *one = (WaveTrack *)track;
      int oneWindowSize = (int)(one->GetRate() / 100);
      float *oneDist = new float[oneWindowSize];
      longSampleCount s = one->TimeToLongSamples(t0);
      one->Get((samplePtr)oneDist, floatSample,
               s - oneWindowSize/2, oneWindowSize);

      // Start by penalizing downward motion.  We prefer upward
      // zero crossings.
      if (oneDist[1] - oneDist[0] < 0)
         oneDist[0] = oneDist[0]*6 + 0.3;
      for(i=1; i<oneWindowSize; i++)
         if (oneDist[i] - oneDist[i-1] < 0)
            oneDist[i] = oneDist[i]*6 + 0.3;

      for(i=0; i<oneWindowSize; i++)
         oneDist[i] = fabs(oneDist[i]);

      for(i=0; i<windowSize; i++) {
         if (windowSize != oneWindowSize)
            j = i * (oneWindowSize-1) / (windowSize-1);
         else
            j = i;

         dist[i] += oneDist[j];
      }
         
      track = iter.Next();
   }

   int argmin = windowSize/2; // Start at default pos in center
   float min = dist[argmin];
   for(i=0; i<windowSize; i++) {
      if (dist[i] < min) {
         argmin = i;
         min = dist[i];
      }
   }

   return t0 + (argmin - windowSize/2)/GetRate();
}

void AudacityProject::OnZeroCrossing()
{
   if (mViewInfo.sel0 == mViewInfo.sel1)
      mViewInfo.sel0 = mViewInfo.sel1 =
         NearestZeroCrossing(mViewInfo.sel0);
   else {
      mViewInfo.sel0 = NearestZeroCrossing(mViewInfo.sel0);
      mViewInfo.sel1 = NearestZeroCrossing(mViewInfo.sel1);

      if (mViewInfo.sel1 < mViewInfo.sel0)
         mViewInfo.sel1 = mViewInfo.sel0;
   }

   mTrackPanel->Refresh(false);
}

//
// Effect Menus
//

void AudacityProject::OnEffect(int type, int index)
{
   EffectArray *effects;
   Effect *f = NULL;

   effects = Effect::GetEffects(type);
   f = (*effects)[index];
   delete effects;

   if (!f)
      return;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   double prevEndTime = mTracks->GetEndTime();
   int count = 0;
   
   while (t) {
      if (t->GetSelected() && t->GetKind() == (Track::Wave))
         count++;
      t = iter.Next();
   }
   
   if (count == 0) {
      // No tracks were selected...
      if (f->GetEffectFlags() & INSERT_EFFECT) {
         // Create a new track for the generated audio...
         WaveTrack *newTrack = mTrackFactory->NewWaveTrack();
         mTracks->Add(newTrack);
         newTrack->SetSelected(true);
      }
      else {
         wxMessageBox(_("You must select a track first."));
         return;
      }
   }
   
   if (f->DoEffect(this, type, mTracks, mTrackFactory,
                   &mViewInfo.sel0, &mViewInfo.sel1)) {
      wxString longDesc = f->GetEffectDescription();
      wxString shortDesc = f->GetEffectName();

      if (shortDesc.Length() > 3 && shortDesc.Right(3)=="...")
         shortDesc = shortDesc.Left(shortDesc.Length()-3);

      PushState(longDesc, shortDesc);
      if (mTracks->GetEndTime() > prevEndTime)
         OnZoomFit();
      FixScrollbars();

      // Only remember a successful effect, don't rmemeber insert,
      // or analyze effects.
      if ((f->GetEffectFlags() & (INSERT_EFFECT | ANALYZE_EFFECT))==0) {
         Effect::SetLastEffect( type, index, f );
         mCommandManager.Modify("RepeatLastEffect",
            wxString::Format(_("Repeat %s\tCtrl+R"),
            (const char *)shortDesc));
      }
      
      mTrackPanel->Refresh(false);
   } else {
      // TODO: undo the effect if necessary?
   }

   
   return;
}

void AudacityProject::OnGenerateEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | INSERT_EFFECT, index);
}

void AudacityProject::OnGeneratePlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | INSERT_EFFECT, index);
}

void AudacityProject::OnRepeatLastEffect(int index)
{
   if( Effect::GetLastEffect()  != NULL )
   {
      // Setting the CONFIGURED_EFFECT bit prevents
      // prompting for parameters.
      OnEffect( 
         Effect::GetLastEffectType() | CONFIGURED_EFFECT,
         Effect::GetLastEffectIndex());
   }
}

void AudacityProject::OnProcessEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | PROCESS_EFFECT, index);
}

void AudacityProject::OnProcessPlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | PROCESS_EFFECT, index);
}

void AudacityProject::OnAnalyzeEffect(int index)
{
   OnEffect(BUILTIN_EFFECT | ANALYZE_EFFECT, index);
}

void AudacityProject::OnAnalyzePlugin(int index)
{
   OnEffect(PLUGIN_EFFECT | ANALYZE_EFFECT, index);
}

//
// File Menu
//

void AudacityProject::OnNew()
{
   CreateNewAudacityProject(gParentWindow);
}

void AudacityProject::OnOpen()
{
   ShowOpenDialog(this);
}

void AudacityProject::OnClose()
{
   Close();
}

void AudacityProject::OnSave()
{
   Save();
}

void AudacityProject::OnSaveAs()
{
   SaveAs();
}

void AudacityProject::OnExit()
{
   QuitAudacity();
}

void AudacityProject::OnExportLabels()
{
   Track *t;
   int numLabelTracks = 0;

   TrackListIterator iter(mTracks);

   t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label)
         numLabelTracks++;
      t = iter.Next();
   }

   if (numLabelTracks == 0) {
      wxMessageBox(_("There are no label tracks to export."));
      return;
   }

   wxString fName = _("labels.txt");

   fName = wxFileSelector(_("Export Labels As:"),
                          NULL,
                          fName,
                          "txt",
                          "*.txt", wxSAVE | wxOVERWRITE_PROMPT, this);

   if (fName == "")
      return;

   // Move existing files out of the way.  Otherwise wxTextFile will
   // append to (rather than replace) the current file.

   if (wxFileExists(FILENAME(fName))) {
#ifdef __WXGTK__
      wxString safetyFileName = fName + "~";
#else
      wxString safetyFileName = fName + ".bak";
#endif

      if (wxFileExists(FILENAME(safetyFileName)))
         wxRemoveFile(FILENAME(safetyFileName));

      wxRename(fName, safetyFileName);
   }

   wxTextFile f(FILENAME(fName));
#ifdef __WXMAC__
   wxFile *temp = new wxFile();
   temp->Create(FILENAME(fName));
   delete temp;
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Label)
         ((LabelTrack *) t)->Export(f);

      t = iter.Next();
   }

#ifdef __WXMAC__
   f.Write(wxTextFileType_Mac);
#else
   f.Write();
#endif
   f.Close();
}

void AudacityProject::OnExportMix()
{
   ::Export(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportSelection()
{
   ::Export(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportMP3Mix()
{
   gPrefs->Write("/FileFormats/LossyExportFormat", "MP3");
   ::ExportLossy(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportMP3Selection()
{
   gPrefs->Write("/FileFormats/LossyExportFormat", "MP3");
   ::ExportLossy(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportOggMix()
{
   gPrefs->Write("/FileFormats/LossyExportFormat", "OGG");
   ::ExportLossy(this, false, 0.0, mTracks->GetEndTime());
}

void AudacityProject::OnExportOggSelection()
{
   gPrefs->Write("/FileFormats/LossyExportFormat", "OGG");
   ::ExportLossy(this, true, mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnExportMultiple()
{
   ::ExportMultiple(this);
}

void AudacityProject::OnPreferences()
{
   PrefsDialog dialog(this /* parent */ );
   dialog.ShowModal();
}

//
// Printing
//

// Globals, so that we remember settings from session to session
wxPrintData *gPrintData = NULL;
wxPageSetupData *gPageSetupData = NULL;

class AudacityPrintout : public wxPrintout
{
 public:
   AudacityPrintout(wxString title,
                    TrackList *tracks):
      wxPrintout(title),
      mTracks(tracks)
   {
   }
   bool OnPrintPage(int page);
   bool HasPage(int page);
   bool OnBeginDocument(int startPage, int endPage);
   void GetPageInfo(int *minPage, int *maxPage,
                    int *selPageFrom, int *selPageTo);

 private:
   TrackList *mTracks;
};

bool AudacityPrintout::OnPrintPage(int page)
{
   wxDC *dc = GetDC();
   if (!dc)
      return false;

   int width, height;
   dc->GetSize(&width, &height);

   int rulerScreenHeight = 40;
   int screenTotalHeight = mTracks->GetHeight() + rulerScreenHeight;

   double scale = height / (double)screenTotalHeight;

   int rulerPageHeight = (int)(rulerScreenHeight * scale);
   Ruler ruler;
   ruler.SetBounds(0, 0, width, rulerPageHeight);
   ruler.SetOrientation(wxHORIZONTAL);
   ruler.SetRange(0.0, mTracks->GetEndTime());
   ruler.SetFormat(Ruler::TimeFormat);
   ruler.SetLabelEdges(true);
   ruler.Draw(*dc);

   TrackArtist artist;
   artist.SetBackgroundBrushes(*wxWHITE_BRUSH, *wxWHITE_BRUSH,
			       *wxWHITE_PEN, *wxWHITE_PEN);
   ViewInfo viewInfo;
   viewInfo.sel0 = viewInfo.sel1 = 0;
   viewInfo.vpos = 0;
   viewInfo.h = 0.0;
   viewInfo.screen = mTracks->GetEndTime() - viewInfo.h;
   viewInfo.total = viewInfo.screen;
   viewInfo.zoom = viewInfo.lastZoom = width / viewInfo.screen;
   int y = rulerPageHeight;

   TrackListIterator iter(mTracks);
   Track *n = iter.First();
   while (n) {
      wxRect r;
      r.x = 0;
      r.y = y;
      r.width = width;
      r.height = (int)(n->GetHeight() * scale);

      switch(n->GetKind()) {
      case Track::Wave:
         switch (((WaveTrack *)n)->GetDisplay()) {
         case WaveTrack::WaveformDisplay:
            artist.DrawWaveform((WaveTrack *)n, *dc, r,
                                &viewInfo, false, false, false, false, false);
            break;
         case WaveTrack::WaveformDBDisplay:
            artist.DrawWaveform((WaveTrack *)n, *dc, r,
                                &viewInfo, false, false, false, true, false);
            break;
         case WaveTrack::SpectrumDisplay:
            artist.DrawSpectrum((WaveTrack *)n, *dc, r, &viewInfo, false);
            break;
         case WaveTrack::PitchDisplay:
            artist.DrawSpectrum((WaveTrack *)n, *dc, r, &viewInfo, true);
            break;
         }
         break;
      case Track::Note:
         artist.DrawNoteTrack((NoteTrack *)n, *dc, r, &viewInfo);
         break;
      case Track::Label:
         artist.DrawLabelTrack((LabelTrack *)n, *dc, r, &viewInfo);
         break;
      case Track::Time:
         artist.DrawTimeTrack((TimeTrack *)n, *dc, r, &viewInfo);
         break;
      }

      dc->SetPen(*wxBLACK_PEN);
      dc->DrawLine(0, r.y, width, r.y);

      n = iter.Next();
      y += r.height;
   };

   return true;
}

bool AudacityPrintout::HasPage(int page)
{
   return (page==1);
}

bool AudacityPrintout::OnBeginDocument(int startPage, int endPage)
{
   return wxPrintout::OnBeginDocument(startPage, endPage);
}

void AudacityPrintout::GetPageInfo(int *minPage, int *maxPage,
                                   int *selPageFrom, int *selPageTo)
{
   *minPage = 1;
   *maxPage = 1;
   *selPageFrom = 1;
   *selPageTo = 1;
}

void AudacityProject::OnPageSetup()
{
   if (gPageSetupData == NULL)
      gPageSetupData = new wxPageSetupDialogData();
   if (gPrintData == NULL)
      gPrintData = new wxPrintData();

   (*gPageSetupData) = *gPrintData;

   wxPageSetupDialog pageSetupDialog(this, gPageSetupData);
   pageSetupDialog.ShowModal();

   (*gPrintData) = pageSetupDialog.GetPageSetupData().GetPrintData();
   (*gPageSetupData) = pageSetupDialog.GetPageSetupData();
}

void AudacityProject::OnPrint()
{
   if (gPageSetupData == NULL)
      gPageSetupData = new wxPageSetupDialogData();
   if (gPrintData == NULL)
      gPrintData = new wxPrintData();

   wxPrintDialogData printDialogData(*gPrintData);

   wxPrinter printer(&printDialogData);
   AudacityPrintout printout(GetName(), mTracks);
   if (!printer.Print(this, &printout, true)) {
      if (wxPrinter::GetLastError() == wxPRINTER_ERROR) {
         wxMessageBox(_("There was a problem printing."),
                      _("Print"), wxOK);
      }
      else {
         // Do nothing, the user cancelled...
      }
   }
   else {
      *gPrintData = printer.GetPrintDialogData().GetPrintData();
   }
}

//
// Edit Menu
//

void AudacityProject::OnUndo()
{
   if (!mUndoManager.UndoAvailable()) {
      wxMessageBox(_("Nothing to undo"));
      return;
   }

   TrackList *l = mUndoManager.Undo(&mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   FixScrollbars();
   mTrackPanel->Refresh(false);

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenus();   
}

void AudacityProject::OnRedo()
{
   if (!mUndoManager.RedoAvailable()) {
      wxMessageBox(_("Nothing to redo"));
      return;
   }

   TrackList *l = mUndoManager.Redo(&mViewInfo.sel0, &mViewInfo.sel1);
   PopState(l);

   FixScrollbars();
   mTrackPanel->Refresh(false);

   if (mHistoryWindow)
      mHistoryWindow->UpdateDisplay();

   ModifyUndoMenus();
}

void AudacityProject::OnHistory()
{
   if (mHistoryWindow)
      mHistoryWindow->Show(true);
   else {
      mHistoryWindow = new HistoryWindow(this, &mUndoManager);
      mHistoryWindow->Show(true);
   }
}


void AudacityProject::OnCut()
{
   ClearClipboard();

   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest;

   while (n) {
      if (n->GetSelected()) {
         dest = NULL;
         n->Cut(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            if (n->GetKind() == Track::Wave) {
               ((WaveTrack *)dest)->SetRate(((WaveTrack *)n)->GetRate());
            }
            msClipboard->Add(dest);
         }
      }
      n = iter.Next();
   }

   msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);
   msClipProject = this;

   mViewInfo.sel1 = mViewInfo.sel0;

   PushState(_("Cut to the clipboard"), _("Cut"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}



void AudacityProject::OnCopy()
{
   ClearClipboard();

   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest;

   while (n) {
      if (n->GetSelected()) {
         dest = NULL;
         n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest) {
            dest->SetChannel(n->GetChannel());
            dest->SetLinked(n->GetLinked());
            dest->SetName(n->GetName());
            if (n->GetKind() == Track::Wave) {
               ((WaveTrack *)dest)->SetRate(((WaveTrack *)n)->GetRate());
            }
            msClipboard->Add(dest);
         }
      }
      n = iter.Next();
   }

   msClipLen = (mViewInfo.sel1 - mViewInfo.sel0);
   msClipProject = this;
   
   //Make sure the menus/toolbar states get updated
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnPaste()
{
   // If nothing's selected, we just insert new tracks...so first
   // check to see if anything's selected
   
   TrackListIterator iter2(mTracks);
   Track *countTrack = iter2.First();
   int numSelected =0;
   while (countTrack) {
      if (countTrack->GetSelected())
         numSelected++;
      countTrack = iter2.Next();
   }
   
   if (numSelected == 0) {
      TrackListIterator clipIter(msClipboard);
      Track *c = clipIter.First();
      Track *n;

      while (c) {
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Lock();
         
         switch(c->GetKind()) {
         case Track::Wave: {
            WaveTrack *w = (WaveTrack *)c;
            n = mTrackFactory->NewWaveTrack(w->GetSampleFormat());
            ((WaveTrack *)n)->SetRate(w->GetRate());
            } break;

         case Track::Note:
            n = mTrackFactory->NewNoteTrack();
            break;

         case Track::Label:
            n = mTrackFactory->NewLabelTrack();
            break;
            
         case Track::Time:
            n = mTrackFactory->NewTimeTrack();
            break;

         default:
            c = clipIter.Next();
            continue;
         }

         n->SetLinked(c->GetLinked());
         n->SetChannel(c->GetChannel());
         n->SetName(c->GetName());

         n->Paste(0.0, c);
         mTracks->Add(n);
         n->SetSelected(true);         
         
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Unlock();
         
         c = clipIter.Next();
      }

      mViewInfo.sel0 = 0.0;
      mViewInfo.sel1 = msClipLen;
      
      PushState(_("Pasted from the clipboard"), _("Paste"));
      
      FixScrollbars();
      mTrackPanel->Refresh(false);

      return;
   }

   // Otherwise, paste into the selected tracks.

   if (mViewInfo.sel0 != mViewInfo.sel1)
      Clear();

   wxASSERT(mViewInfo.sel0 == mViewInfo.sel1);

   double tsel = mViewInfo.sel0;

   TrackListIterator iter(mTracks);
   TrackListIterator clipIter(msClipboard);

   Track *n = iter.First();
   Track *c = clipIter.First();

   while (n && c) {
      if (n->GetSelected()) {
         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Lock();

         n->Paste(tsel, c);

         if (msClipProject != this && c->GetKind() == Track::Wave)
            ((WaveTrack *) c)->Unlock();

         c = clipIter.Next();
      }

      n = iter.Next();
   }

   // TODO: What if we clicked past the end of the track?

   mViewInfo.sel0 = tsel;
   mViewInfo.sel1 = tsel + msClipLen;

   PushState(_("Pasted from the clipboard"), _("Paste"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnPasteOver()
{
   if(msClipLen>0.0)
   {
      mViewInfo.sel1=mViewInfo.sel0+msClipLen;
      OnPaste();
   }

   return;
}

void AudacityProject::OnTrim()
{
   if (mViewInfo.sel0 >= mViewInfo.sel1)
      return;

   TrackListIterator iter(mTracks);
   Track *n = iter.First();

   while (n) {
      if (n->GetSelected()) {
         //Delete the section before the left selector
         n->Clear(n->GetOffset(), mViewInfo.sel0);
         if (mViewInfo.sel0 > n->GetOffset())
            n->SetOffset(mViewInfo.sel0);

         //Delete the section after the right selector
         n->Clear(mViewInfo.sel1, n->GetEndTime());
      }
      n = iter.Next();
   }

   FixScrollbars();
   mTrackPanel->Refresh(false);
   PushState(_("Trim file to selection"), _("Trim"));
}



void AudacityProject::OnDelete()
{
   Clear();
}

void AudacityProject::OnSilence()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();

   while (n) {
      if (n->GetSelected())
         n->Silence(mViewInfo.sel0, mViewInfo.sel1);

      n = iter.Next();
   }

   PushState(wxString::
             Format(_("Silenced selected tracks for %.2f seconds at %.2f"),
                    mViewInfo.sel1 - mViewInfo.sel0, mViewInfo.sel0),
             _("Silence"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnDuplicate()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest;

   TrackList newTracks;

   while (n) {
      if (n->GetSelected()) {
         dest = NULL;
         n->Copy(mViewInfo.sel0, mViewInfo.sel1, &dest);
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(mViewInfo.sel0, n->GetOffset()));
            newTracks.Add(dest);
         }
      }
      n = iter.Next();
   }

   TrackListIterator nIter(&newTracks);
   n = nIter.First();
   while (n) {
      mTracks->Add(n);
      n = nIter.Next();
   }

   PushState(_("Duplicated"), _("Duplicate"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSplit()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *dest;

   TrackList newTracks;

   while (n) {
      if (n->GetSelected()) {
         double sel0 = mViewInfo.sel0;
         double sel1 = mViewInfo.sel1;

         dest = NULL;
         n->Copy(sel0, sel1, &dest);
         if (dest) {
            dest->Init(*n);
            dest->SetOffset(wxMax(sel0, n->GetOffset()));

            if (sel1 >= n->GetEndTime())
               n->Clear(sel0, sel1);
            else if (sel0 <= n->GetOffset()) {
               n->Clear(sel0, sel1);
               n->SetOffset(sel1);
            } else
               n->Silence(sel0, sel1);

            newTracks.Add(dest);
         }
      }
      n = iter.Next();
   }

   TrackListIterator nIter(&newTracks);
   n = nIter.First();
   while (n) {
      mTracks->Add(n);
      n = nIter.Next();
   }

   PushState(_("Split"), _("Split"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSplitLabels()
{
   TrackListIterator iter(mTracks);

   Track *n = iter.First();
   Track *srcRight = 0;
   Track *srcLeft = 0;
   bool stereo = false;
   LabelTrack *label = 0;

   while(n) {
      if(n->GetSelected()) {
         if(n->GetKind() == Track::Wave) {
            if(n->GetLinked() == true) {
               stereo = true;
               srcLeft = n;
               srcRight  = iter.Next();
            }
            else {
               srcRight = n;
               stereo = false;
            }
         }
         else if(n->GetKind() == Track::Label)
            label = (LabelTrack*)n;  // cast necessary to call LabelTrack specific methods
      }
      n = iter.Next();
   }

   // one new track for every label, from that label to the next
   
   TrackList newTracks;

   for(int i = 0; i < label->GetNumLabels(); i++) {
      wxString name = label->GetLabel(i)->title;
      double begin = label->GetLabel(i)->t;
      double end;

      // if on the last label, extend to the end of the wavetrack
      if(i == label->GetNumLabels() - 1) {
         if(stereo)
            end = wxMax(srcLeft->GetEndTime(), srcRight->GetEndTime());
         else
            end = srcLeft->GetEndTime();
      }
      else
         end = label->GetLabel(i+1)->t;

      Track *destLeft = 0;
      Track *destRight = 0;

      srcLeft->Copy(begin, end, &destLeft);
      if (destLeft) {
         destLeft->Init(*srcLeft);
         destLeft->SetOffset(wxMax(begin, srcLeft->GetOffset()));
         destLeft->SetName(name);
         
         mTracks->Add(destLeft);
      }

      if(stereo) {
         srcRight->Copy(begin, end, &destRight);
         if (destRight) {
            destRight->Init(*srcRight);
            destRight->SetOffset(wxMax(begin, srcRight->GetOffset()));
            destRight->SetName(name);
            
            mTracks->Add(destRight);
         }
         else if(destLeft)
            // account for possibility of a non-aligned linked track, which could
            // cause the left channel to be eligible for creating a new track,
            // but not the right.
            destLeft->SetLinked(false);
      }
   }

   PushState(_("Split at labels"), _("Split at labels"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectAll()
{
   TrackListIterator iter(mTracks);

   Track *t = iter.First();
   while (t) {
      t->SetSelected(true);
      t = iter.Next();
   }
   mViewInfo.sel0 = mTracks->GetMinOffset();
   mViewInfo.sel1 = mTracks->GetEndTime();

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectCursorEnd()
{
   double maxEndOffset = -1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }

      t = iter.Next();
   }

   mViewInfo.sel1 = maxEndOffset;

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnSelectStartCursor()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   mViewInfo.sel0 = minOffset;

   mTrackPanel->Refresh(false);
}

//
// View Menu
//

void AudacityProject::OnZoomIn()
{
   // DMM: Here's my attempt to get logical zooming behavior
   // when there's a selection that's currently at least
   // partially on-screen

   bool selectionIsOnscreen =
      (mViewInfo.sel0 < mViewInfo.h + mViewInfo.screen) &&
      (mViewInfo.sel1 > mViewInfo.h);

   bool selectionFillsScreen =
      (mViewInfo.sel0 < mViewInfo.h) &&
      (mViewInfo.sel1 > mViewInfo.h + mViewInfo.screen);
   
   if (selectionIsOnscreen && !selectionFillsScreen) {
      // Start with the center of the selection
      double selCenter = (mViewInfo.sel0 + mViewInfo.sel1) / 2;

      // If the selection center is off-screen, pick the
      // center of the part that is on-screen.
      if (selCenter < mViewInfo.h)
         selCenter = mViewInfo.h + (mViewInfo.sel1 - mViewInfo.h) / 2;
      if (selCenter > mViewInfo.h + mViewInfo.screen)
         selCenter = mViewInfo.h + mViewInfo.screen -
            (mViewInfo.h + mViewInfo.screen - mViewInfo.sel0) / 2;
         
      // Zoom in
      Zoom(mViewInfo.zoom *= 2.0);

      // Recenter on selCenter
      TP_ScrollWindow(selCenter - mViewInfo.screen / 2);
      return;
   }


   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;
   Zoom(mViewInfo.zoom *= 2.0);
   
   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   
   // MM: Commented this out because it was confusing users
   /*
   // make sure that the *right-hand* end of the selection is
   // no further *left* than 1/3 of the way across the screen
   if (mViewInfo.sel1 < newh + mViewInfo.screen / 3)
      newh = mViewInfo.sel1 - mViewInfo.screen / 3;

   // make sure that the *left-hand* end of the selection is
   // no further *right* than 2/3 of the way across the screen
   if (mViewInfo.sel0 > newh + mViewInfo.screen * 2 / 3)
      newh = mViewInfo.sel0 - mViewInfo.screen * 2 / 3;
   */

   TP_ScrollWindow(newh);
}

void AudacityProject::OnZoomOut()
{  
   //Zoom() may change these, so record original values:
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   Zoom(mViewInfo.zoom /= 2.0);

   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   // newh = (newh > 0) ? newh : 0;
   TP_ScrollWindow(newh);

}

static double OldZooms[2]={ 44100.0/512.0, 4410.0/512.0 };
void AudacityProject::OnZoomToggle()
{
   double origLeft = mViewInfo.h;
   double origWidth = mViewInfo.screen;

   float f;
   // look at percentage difference.  We add a small fudge factor
   // to avoid testing for zero divisor.
   f = mViewInfo.zoom / (OldZooms[0] + 0.0001f);
   // If old zoom is more than 10 percent different, use it.
   if( (0.90f > f) || (f >1.10) ){
      OldZooms[1]=OldZooms[0];
      OldZooms[0]=mViewInfo.zoom;
   }
   Zoom( OldZooms[1] );
   double newh = origLeft + (origWidth - mViewInfo.screen) / 2;
   TP_ScrollWindow(newh);
}


void AudacityProject::OnZoomNormal()
{
   Zoom(44100.0 / 512.0);
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnZoomFit()
{
   double len = mTracks->GetEndTime();

   if (len <= 0.0)
      return;

   int w, h;
   mTrackPanel->GetTracksUsableArea(&w, &h);
   w -= 10;

   Zoom(w / len);
   TP_ScrollWindow(0.0);
}

void AudacityProject::OnZoomFitV()
{
   int width, height, count;

   mTrackPanel->GetTracksUsableArea(&width, &height);

   height -= 28;

   count = 0;
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Wave)
         count++;
      else
         height -= t->GetHeight();

      t = iter.Next();
   }

   if (count == 0)
      return;

   height = height / count;

   if (height < 40)
      height = 40;

   TrackListIterator iter2(mTracks);
   t = iter2.First();
   while (t) {
      if (t->GetKind() == Track::Wave)
         t->SetHeight(height);
      t = iter2.Next();
   }

   mVsbar->SetThumbPosition(0);
   FixScrollbars();
   Refresh(false);
   ModifyState();
}

void AudacityProject::OnZoomSel()
{
   if (mViewInfo.sel1 <= mViewInfo.sel0)
      return;

   Zoom(mViewInfo.zoom * mViewInfo.screen / (mViewInfo.sel1 - mViewInfo.sel0)),
   TP_ScrollWindow(mViewInfo.sel0);
}

void AudacityProject::OnSelectionFormat(int index)
{
   mSelectionFormat = index;
   TP_DisplaySelection();
}

void AudacityProject::OnSnapOn()
{
   mSnapTo = 1;
   TP_DisplaySelection();
}

void AudacityProject::OnSnapOff()
{
   mSnapTo = 0;
   TP_DisplaySelection();
}

void AudacityProject::OnPlotSpectrum()
{
   int selcount = 0;
   int i;
   double rate = 0;
   sampleCount len = 0;
   float *buffer = NULL;
   bool warning = false;
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetSelected() && t->GetKind() == Track::Wave) {
         WaveTrack *track = (WaveTrack *)t;
         if (selcount==0) {
            rate = track->GetRate();
            longSampleCount start, end;
            start = track->TimeToLongSamples(mViewInfo.sel0);
            end = track->TimeToLongSamples(mViewInfo.sel1);
            len = (sampleCount)(end - start);
            if (len > 1048576) {
               warning = true;
               len = 1048576;
            }
            buffer = new float[len];
            track->Get((samplePtr)buffer, floatSample, start, len);
         }
         else {
            if (track->GetRate() != rate) {
               wxMessageBox(_("To plot the spectrum, all selected tracks must be the same sample rate."));
               delete[] buffer;
               return;
            }
            longSampleCount start;
            start = track->TimeToLongSamples(mViewInfo.sel0);
            float *buffer2 = new float[len];
            track->Get((samplePtr)buffer2, floatSample, start, len);
            for(i=0; i<len; i++)
               buffer[i] += buffer2[i];
            delete[] buffer2;
         }
         selcount++;
      }
      t = iter.Next();
   }
   
   if (selcount == 0)
      return;
   
   if (selcount > 1)
      for(i=0; i<len; i++)
         buffer[i] /= selcount;
   
   if (warning) {
      wxString msg;
      msg.Printf(_("Too much audio was selected.  Only the first %.1f seconds of audio will be analyzed."),
                          (len / rate));
      wxMessageBox(msg);
   }

   InitFreqWindow(gParentWindow);
   gFreqWindow->Plot(len, buffer, rate);
   gFreqWindow->Show(true);
   gFreqWindow->Raise();

   delete[] buffer;
}


void AudacityProject::OnFloatControlToolBar()
{
   if (gControlToolBarStub->GetWindowedStatus()) {

      gControlToolBarStub->HideWindowedToolBar();
      gControlToolBarStub->LoadAll();
   } else {
      gControlToolBarStub->ShowWindowedToolBar();
      gControlToolBarStub->UnloadAll();
   }
}


void AudacityProject::OnLoadEditToolBar()
{
   if (gEditToolBarStub) {
      if (gEditToolBarStub->GetLoadedStatus()) {

         //the toolbar is "loaded", meaning its visible either in the window
         //or floating

         gEditToolBarStub->SetLoadedStatus(false);
         gEditToolBarStub->HideWindowedToolBar();
         gEditToolBarStub->UnloadAll();

      } else {

         //the toolbar is not "loaded", meaning that although the stub exists, 
         //the toolbar is not visible either in a window or floating around
         gEditToolBarStub->SetLoadedStatus(true);

         if (gEditToolBarStub->GetWindowedStatus()) {
            //Make the floating toolbar appear
            gEditToolBarStub->ShowWindowedToolBar();
            gEditToolBarStub->LoadAll();
         } else {
            //Make it appear in all the windows
            gEditToolBarStub->LoadAll();
         }

      }
   } else {
      gEditToolBarStub = new ToolBarStub(gParentWindow, EditToolBarID);
      gEditToolBarStub->LoadAll();
   }
}


void AudacityProject::OnFloatEditToolBar()
{
   if (gEditToolBarStub) {

      if (gEditToolBarStub->GetWindowedStatus()) {

         gEditToolBarStub->HideWindowedToolBar();
         gEditToolBarStub->LoadAll();

      } else {

         gEditToolBarStub->ShowWindowedToolBar();
         gEditToolBarStub->UnloadAll();
      }
   }
}


void AudacityProject::OnLoadMixerToolBar()
{
   if (gMixerToolBarStub) {
      if (gMixerToolBarStub->GetLoadedStatus()) {

         //the toolbar is "loaded", meaning its visible either in the window
         //or floating

         gMixerToolBarStub->SetLoadedStatus(false);
         gMixerToolBarStub->HideWindowedToolBar();
         gMixerToolBarStub->UnloadAll();

      } else {

         //the toolbar is not "loaded", meaning that although the stub exists, 
         //the toolbar is not visible either in a window or floating around
         gMixerToolBarStub->SetLoadedStatus(true);

         if (gMixerToolBarStub->GetWindowedStatus()) {
            //Make the floating toolbar appear
            gMixerToolBarStub->ShowWindowedToolBar();
            gMixerToolBarStub->LoadAll();
         } else {
            //Make it appear in all the windows
            gMixerToolBarStub->LoadAll();
         }

      }
   } else {
      gMixerToolBarStub = new ToolBarStub(gParentWindow, MixerToolBarID);
      gMixerToolBarStub->LoadAll();
   }
}


void AudacityProject::OnFloatMixerToolBar()
{
   if (gMixerToolBarStub) {

      if (gMixerToolBarStub->GetWindowedStatus()) {

         gMixerToolBarStub->HideWindowedToolBar();
         gMixerToolBarStub->LoadAll();

      } else {

         gMixerToolBarStub->ShowWindowedToolBar();
         gMixerToolBarStub->UnloadAll();
      }
   }
}

void AudacityProject::OnLoadMeterToolBar()
{
   if (gMeterToolBarStub) {
      if (gMeterToolBarStub->GetLoadedStatus()) {

         //the toolbar is "loaded", meaning its visible either in the window
         //or floating

         gMeterToolBarStub->SetLoadedStatus(false);
         gMeterToolBarStub->HideWindowedToolBar();
         gMeterToolBarStub->UnloadAll();

      } else {

         //the toolbar is not "loaded", meaning that although the stub exists, 
         //the toolbar is not visible either in a window or floating around
         gMeterToolBarStub->SetLoadedStatus(true);

         if (gMeterToolBarStub->GetWindowedStatus()) {
            //Make the floating toolbar appear
            gMeterToolBarStub->ShowWindowedToolBar();
            gMeterToolBarStub->LoadAll();
         } else {
            //Make it appear in all the windows
            gMeterToolBarStub->LoadAll();
         }

      }
   } else {
      gMeterToolBarStub = new ToolBarStub(gParentWindow, MeterToolBarID);
      gMeterToolBarStub->LoadAll();
   }
}


void AudacityProject::OnFloatMeterToolBar()
{
   // Can't drag the Meter toolbar while Audio I/O is busy at all
   if (gAudioIO->IsStreamActive())
      return;

   if (gMeterToolBarStub) {

      if (gMeterToolBarStub->GetWindowedStatus()) {

         gMeterToolBarStub->HideWindowedToolBar();
         gMeterToolBarStub->LoadAll();

      } else {

         gMeterToolBarStub->ShowWindowedToolBar();
         gMeterToolBarStub->UnloadAll();
      }
   }
}



//
// Project Menu
//

void AudacityProject::OnImport()
{
   wxString path = gPrefs->Read("/DefaultOpenPath",FROMFILENAME(::wxGetCwd()));

   // TODO: Build the list of file types dynamically
   
   wxFileDialog dlog(this, _("Select one or more audio files..."),
                     path, "",
                     _("All files (*.*)|*.*|"
                       "WAV files (*.wav)|*.wav|"
                       "AIFF files (*.aif)|*.aif|"
                       "AU files (*.au)|*.au|"
                       "MP3 files (*.mp3)|*.mp3|"
                       "Ogg Vorbis files (*.ogg)|*.ogg|"
                       "List of Files (*.lof)|*.lof"),
                     wxOPEN | wxMULTIPLE);

   int result = dlog.ShowModal();

   if (result != wxID_OK)
      return;

   wxArrayString selectedFiles;
   unsigned int ff;

   dlog.GetPaths(selectedFiles);

   for(ff=0; ff<selectedFiles.GetCount(); ff++) {
      wxString fileName = selectedFiles[ff];

      path =::wxPathOnly(fileName);
      gPrefs->Write("/DefaultOpenPath", path);
      
      Import(fileName);
   }
}

void AudacityProject::OnImportLabels()
{
   wxString path = gPrefs->Read("/DefaultOpenPath",FROMFILENAME(::wxGetCwd()));

   wxString fileName =
       wxFileSelector(_("Select a text file containing labels..."),
                      path,     // Path
                      "",       // Name
                      ".txt",   // Extension
                      _("Text files (*.txt)|*.txt|" "All files (*.*)|*.*"),
                      0,        // Flags
                      this);    // Parent

   if (fileName != "") {
      path =::wxPathOnly(fileName);
      gPrefs->Write("/DefaultOpenPath", path);

      wxTextFile f;

      f.Open(fileName);
      if (!f.IsOpened()) {
         wxMessageBox(_("Could not open file: ") + fileName);
         return;
      }

      LabelTrack *newTrack = new LabelTrack(mDirManager);

      newTrack->Import(f);

      SelectNone();
      mTracks->Add(newTrack);
      newTrack->SetSelected(true);

      PushState(wxString::
                Format(_("Imported labels from '%s'"), fileName.c_str()),
                _("Import Labels"));

      FixScrollbars();
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnImportMIDI()
{
   wxString path = gPrefs->Read("/DefaultOpenPath",FROMFILENAME(::wxGetCwd()));

   wxString fileName = wxFileSelector(_("Select a MIDI file..."),
                                      path,     // Path
                                      "",       // Name
                                      "",       // Extension
                                      _("All files (*.*)|*.*|"
                                        "MIDI files (*.mid)|*.mid|"
                                        "Allegro files (*.gro)|*.gro"),
                                      0,        // Flags
                                      this);    // Parent

   if (fileName != "") {
      path =::wxPathOnly(fileName);
      gPrefs->Write("/DefaultOpenPath", path);

      NoteTrack *newTrack = new NoteTrack(mDirManager);

      if (::ImportMIDI(fileName, newTrack)) {

         SelectNone();
         mTracks->Add(newTrack);
         newTrack->SetSelected(true);

         PushState(wxString::Format(_("Imported MIDI from '%s'"),
                                    fileName.c_str()),
                   _("Import MIDI"));

         FixScrollbars();
         mTrackPanel->Refresh(false);
      }
   }
}

void AudacityProject::OnImportRaw()
{
   wxString path = gPrefs->Read("/DefaultOpenPath",FROMFILENAME(::wxGetCwd()));

   wxString fileName =
       wxFileSelector(_("Select any uncompressed audio file..."),
                      path,     // Path
                      "",       // Name
                      "",       // Extension
                      _("All files (*)|*"),
                      0,        // Flags
                      this);    // Parent

   if (fileName == "")
      return;

   path =::wxPathOnly(fileName);
   gPrefs->Write("/DefaultOpenPath", path);
   
   Track **newTracks;
   int numTracks;
   
   wxStartTimer();

   wxASSERT(!mImportProgressDialog);

   mImportingRaw = true;

   numTracks = ::ImportRaw(this, fileName, mTrackFactory, &newTracks,
                           AudacityProject::ImportProgressCallback,
                           this);

   if(mImportProgressDialog) {
      delete mImportProgressDialog;
      mImportProgressDialog = NULL;
   }

   mImportingRaw = false;
   
   if (numTracks <= 0)
      return;

   AddImportedTracks(fileName, newTracks, numTracks);
}

void AudacityProject::OnEditID3()
{
   if (mTags->ShowEditDialog(this, _("Edit ID3 Tags (for MP3 exporting)")))
      PushState(_("Edit ID3 tags"), _("Edit ID3 Tags"));
}

void AudacityProject::OnQuickMix()
{
   WaveTrack *newLeft = NULL;
   WaveTrack *newRight = NULL;

   if (::QuickMix(mTracks, mTrackFactory, mRate, mDefaultFormat, 0.0, 0.0,
                  &newLeft, &newRight)) {

      // Remove originals

      TrackListIterator iter(mTracks);
      Track *t = iter.First();

      while (t) {
         if (t->GetSelected()) {
            delete t;
            t = iter.RemoveCurrent();
         }
         else
            t = iter.Next();
      }

      // Add new tracks

      mTracks->Add(newLeft);
      if (newRight)
         mTracks->Add(newRight);

      PushState(_("Quick mix"), _("Quick mix"));

      FixScrollbars();
      mTrackPanel->Refresh(false);
   }
}

void AudacityProject::OnSelectionSave()
{
   mSel0save = mViewInfo.sel0;
   mSel1save = mViewInfo.sel1;
}

void AudacityProject::OnSelectionRestore()
{
   mViewInfo.sel0 = mSel0save;
   mViewInfo.sel1 = mSel1save;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackStart()
{
   double minOffset = 1000000.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         if (t->GetOffset() < minOffset)
            minOffset = t->GetOffset();
      }

      t = iter.Next();
   }

   if (minOffset < 0.0) minOffset = 0.0;
   mViewInfo.sel0 = minOffset;
   mViewInfo.sel1 = minOffset;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorTrackEnd()
{
   double maxEndOffset = -1000000.0;
   double thisEndOffset = 0.0;

   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         thisEndOffset = t->GetEndTime();
         if (thisEndOffset > maxEndOffset)
            maxEndOffset = thisEndOffset;
      }

      t = iter.Next();
   }

   mViewInfo.sel0 = maxEndOffset;
   mViewInfo.sel1 = maxEndOffset;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelStart()
{
   mViewInfo.sel1 = mViewInfo.sel0;
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnCursorSelEnd()
{
   mViewInfo.sel0 = mViewInfo.sel1;
   mTrackPanel->Refresh(false);
}

void AudacityProject::HandleAlign(int index, bool moveSel)
{
   TrackListIterator iter(mTracks);
   wxString action;
   double offset;
   double minOffset = 1000000000.0;
   double maxEndOffset = 0.0;
   double avgOffset = 0.0;
   int numSelected = 0;
   Track *t = iter.First();
   double delta = 0.0;
   double newPos = -1.0;

   while (t) {
      if (t->GetSelected()) {
         numSelected++;

         offset = t->GetOffset();
         avgOffset += offset;
         if (offset < minOffset)
            minOffset = offset;
         if (t->GetEndTime() > maxEndOffset)
            maxEndOffset = t->GetEndTime();
      }
      t = iter.Next();
   }

   avgOffset /= numSelected;

   switch(index) {
   case kAlignZero:
      delta = -minOffset;
      action = _("Aligned with zero");
      break;
   case kAlignCursor:
      delta = mViewInfo.sel0 - minOffset;
      action = _("Aligned cursor");
      break;
   case kAlignSelStart:
      delta = mViewInfo.sel0 - minOffset;
      action = _("Aligned with selection start");
      break;
   case kAlignSelEnd:
      delta = mViewInfo.sel1 - minOffset;
      action = _("Aligned with selection end");
      break;
   case kAlignEndCursor:
      delta = mViewInfo.sel0 - maxEndOffset;
      action = _("Aligned end with cursor");
      break;
   case kAlignEndSelStart:
      delta = mViewInfo.sel0 - maxEndOffset;
      action = _("Aligned end with selection start");
      break;
   case kAlignEndSelEnd:
      delta = mViewInfo.sel1 - maxEndOffset;
      action = _("Aligned end with selection end");
      break;
   case kAlign:
      newPos = avgOffset;
      action = _("Aligned");
      break;
   }

   if (newPos >= 0.0) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      
      while (t) {
         if (t->GetSelected()) {
            t->SetOffset(newPos);
         }
         t = iter.Next();
      }
   }

   if (delta != 0.0) {
      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      
      while (t) {
         if (t->GetSelected()) {
            t->SetOffset(t->GetOffset() + delta);
         }
         t = iter.Next();
      }
   }

   if (moveSel) {
      mViewInfo.sel0 += delta;
      mViewInfo.sel1 += delta;
   }

   PushState(action, _("Align"));

   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAlign(int index)
{
   HandleAlign(index, false);
}

void AudacityProject::OnAlignMoveSel(int index)
{
   HandleAlign(index, true);
}

void AudacityProject::OnNewWaveTrack()
{
   WaveTrack *t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetRate(mRate);
   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new audio track"), _("New Track"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewStereoTrack()
{
   WaveTrack *t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetChannel(Track::LeftChannel);
   t->SetRate(mRate);
   SelectNone();
   
   mTracks->Add(t);
   t->SetSelected(true);
   t->SetLinked (true);
   
   t = mTrackFactory->NewWaveTrack(mDefaultFormat);
   t->SetChannel(Track::RightChannel);
   t->SetRate(mRate);
   
   mTracks->Add(t);
   t->SetSelected (true);
   
   PushState(_("Created new stereo audio track"), _("New Track"));
   
   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewLabelTrack()
{
   LabelTrack *t = new LabelTrack(mDirManager);

   SelectNone();

   mTracks->Add(t);
   t->SetSelected(true);

   PushState(_("Created new label track"), _("New Track"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnNewTimeTrack()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();
   bool alreadyHaveTimeTrack = false;
   
   while (t)
      {
         if (t->GetKind() == Track::Time)
            {
               alreadyHaveTimeTrack = true;
               break;
            }
         t = iter.Next();
      }
   
   if( alreadyHaveTimeTrack )
      {
         wxString msg;
         msg.Printf(_("The version of Audacity you are using does not support multiple time tracks."));
         wxMessageBox(msg);
      }
   else
      {
         TimeTrack *t = new TimeTrack(mDirManager);

         SelectNone();
         mTracks->AddToHead(t);
         t->SetSelected(true);
         
         PushState(_("Created new time track"), _("New Track"));

         /*
         TrackListIterator iter(mTracks);
         for( Track *tr = iter.First(); (tr); tr = iter.Next() )
            tr->SetTimeTrack( t );
         */
         
         FixScrollbars();
         mTrackPanel->Refresh(false);
      }
}

void AudacityProject::DoAddLabel(double left, double right)
{
   TrackListIterator iter(mTracks);
   LabelTrack *lt = NULL;

   Track *t = iter.First();
   while (t && !lt) {
      if (t->GetKind() == Track::Label)
         lt = (LabelTrack *)t;
      else
         t = iter.Next();
   }

   if (!lt) {
      lt = new LabelTrack(mDirManager);
      mTracks->Add(lt);

   }
   SelectNone();
   lt->SetSelected(true);

   lt->AddLabel(left, right);

   PushState(_("Added label"), _("Label"));

   FixScrollbars();
   mTrackPanel->Refresh(false);
}

void AudacityProject::OnAddLabel()
{
   DoAddLabel(mViewInfo.sel0, mViewInfo.sel1);
}

void AudacityProject::OnAddLabelPlaying()
{
   if (GetAudioIOToken()>0 &&
       gAudioIO->IsStreamActive(GetAudioIOToken())) {
      double indicator = gAudioIO->GetStreamTime();
      DoAddLabel(indicator, indicator);
   }
}

void AudacityProject::OnRemoveTracks()
{
   TrackListIterator iter(mTracks);
   Track *t = iter.First();

   while (t) {
      if (t->GetSelected()) {
         delete t;
         t = iter.RemoveCurrent();
      }
      else
         t = iter.Next();
   }

   PushState(_("Removed audio track(s)"), _("Remove Track"));

   mTrackPanel->Refresh(false);
}

//
// Help Menu
//

void AudacityProject::OnAbout()
{
   AboutDialog dlog(this);
   dlog.ShowModal();
}

void AudacityProject::OnHelp()
{
   ::ShowHelp(this);
}

void AudacityProject::OnHelpIndex()
{
   ::ShowHelpIndex(this);
}

void AudacityProject::OnHelpSearch()
{
   ::SearchHelp(this);
}

void AudacityProject::OnBenchmark()
{
   ::RunBenchmark(this);
}

//

void AudacityProject::OnSeparator()
{

}

