/**********************************************************************

  Audacity: A Digital Audio Editor

  Menus.h

  Dominic Mazzoni

  All of the menu bar handling is part of the class AudacityProject,
  but the event handlers for all of the menu items have been moved
  to Menus.h and Menus.cpp for clarity.

**********************************************************************/

// These are all member functions of class AudacityProject.

void CommandManagerCallback(void *fptr);
void CommandManagerListCallback(void *fptr, int index);

void CreateMenusAndCommands();

void ModifyExportMenus();
void ModifyUndoMenus();
void ModifyToolbarMenus();

int GetToolBarChecksum();
wxUint32 GetUpdateFlags();

double NearestZeroCrossing(double t0);

void DoAddLabel(double left, double right);

// used in routines OnSelectionSave
// and OnSelectionRestore
double mSel0save;
double mSel1save;
 
        // Selecting a tool from the keyboard

void OnSelectTool();
void OnZoomTool();
void OnEnvelopeTool();
void OnTimeShiftTool();
void OnDrawTool();
void OnMultiTool();

void OnNextTool();
void OnPrevTool();

        // Audio I/O Commands

void OnStop();
void OnPause();
void OnRecord();
void OnSkipStart();
void OnSkipEnd();

        // Different posibilities for playing sound

bool MakeReadyToPlay(); // Helper function that sets button states etc.
void OnPlayStop();
void OnPlayOneSecond();
void OnPlayToSelection();
void OnPlayLooped();

        // Selection-Editing Commands

void OnCursorLeft();
void OnCursorRight();
void OnSelExtendLeft();
void OnSelExtendRight();
void OnSelContractLeft();
void OnSelContractRight();
void OnSelToStart();
void OnSelToEnd();

void OnZeroCrossing();

        // File Menu

void OnNew();
void OnOpen();
void OnClose();
void OnSave();
void OnSaveAs();

void OnExportMix();
void OnExportSelection();
void OnExportMP3Mix();
void OnExportMP3Selection();
void OnExportOggMix();
void OnExportOggSelection();
void OnExportMultiple();

void OnExportLabels();

void OnPreferences();

void OnPageSetup();
void OnPrint();

void OnExit();

        // Edit Menu

void OnUndo();
void OnRedo();
void OnHistory();

void OnCut();
void OnCopy();
void OnPaste();
void OnPasteOver();
void OnTrim();

void OnDelete();
void OnSilence();

void OnSplit();
void OnSplitLabels();
void OnDuplicate();

void OnSelectAll();
void OnSelectCursorEnd();
void OnSelectStartCursor();

        // View Menu

void OnZoomIn();
void OnZoomOut();
void OnZoomToggle();
void OnZoomNormal();
void OnZoomFit();
void OnZoomFitV();
void OnZoomSel();

void OnSelectionFormat(int index);
void OnSnapOn();
void OnSnapOff();

void OnPlotSpectrum();

void OnFloatControlToolBar();
void OnLoadEditToolBar();
void OnFloatEditToolBar();
void OnLoadMixerToolBar();
void OnFloatMixerToolBar();
void OnLoadMeterToolBar();
void OnFloatMeterToolBar();


        // Project Menu

void OnImport();
void OnImportLabels();
void OnImportMIDI();
void OnImportRaw();

void OnEditID3();

void OnQuickMix();

void OnSelectionSave();
void OnSelectionRestore();

void OnCursorTrackStart();
void OnCursorTrackEnd();
void OnCursorSelStart();
void OnCursorSelEnd();

void OnAlign(int index);
void OnAlignMoveSel(int index);
void HandleAlign(int index, bool moveSel);

void OnNewWaveTrack();
void OnNewStereoTrack();
void OnNewLabelTrack();
void OnNewTimeTrack();
void OnRemoveTracks();
void OnAddLabel();
void OnAddLabelPlaying();

        // Effect Menu

void OnEffect(int type, int index);
void OnGenerateEffect(int index);
void OnGeneratePlugin(int index);
void OnRepeatLastEffect(int index);
void OnProcessEffect(int index);
void OnProcessPlugin(int index);
void OnAnalyzeEffect(int index);
void OnAnalyzePlugin(int index);

        // Help Menu

void OnAbout();
void OnHelp();
void OnHelpIndex();
void OnHelpSearch();
void OnBenchmark();

       //

void OnSeparator();

