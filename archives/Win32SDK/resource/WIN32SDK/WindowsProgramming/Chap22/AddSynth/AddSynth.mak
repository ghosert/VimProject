# Microsoft Developer Studio Generated NMAKE File, Based on AddSynth.dsp
!IF "$(CFG)" == ""
CFG=AddSynth - Win32 Debug
!MESSAGE No configuration specified. Defaulting to AddSynth - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "AddSynth - Win32 Release" && "$(CFG)" != "AddSynth - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "AddSynth.mak" CFG="AddSynth - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "AddSynth - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "AddSynth - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "AddSynth - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\AddSynth.exe"


CLEAN :
	-@erase "$(INTDIR)\AddSynth.obj"
	-@erase "$(INTDIR)\AddSynth.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\AddSynth.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\AddSynth.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\AddSynth.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\AddSynth.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\AddSynth.pdb" /machine:I386 /out:"$(OUTDIR)\AddSynth.exe" 
LINK32_OBJS= \
	"$(INTDIR)\AddSynth.obj" \
	"$(INTDIR)\AddSynth.res"

"$(OUTDIR)\AddSynth.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "AddSynth - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\AddSynth.exe"


CLEAN :
	-@erase "$(INTDIR)\AddSynth.obj"
	-@erase "$(INTDIR)\AddSynth.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\AddSynth.exe"
	-@erase "$(OUTDIR)\AddSynth.ilk"
	-@erase "$(OUTDIR)\AddSynth.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\AddSynth.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\AddSynth.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\AddSynth.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\AddSynth.pdb" /debug /machine:I386 /out:"$(OUTDIR)\AddSynth.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\AddSynth.obj" \
	"$(INTDIR)\AddSynth.res"

"$(OUTDIR)\AddSynth.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("AddSynth.dep")
!INCLUDE "AddSynth.dep"
!ELSE 
!MESSAGE Warning: cannot find "AddSynth.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "AddSynth - Win32 Release" || "$(CFG)" == "AddSynth - Win32 Debug"
SOURCE=.\AddSynth.c

"$(INTDIR)\AddSynth.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\AddSynth.rc

"$(INTDIR)\AddSynth.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

