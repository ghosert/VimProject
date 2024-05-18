# Microsoft Developer Studio Generated NMAKE File, Based on Drum.dsp
!IF "$(CFG)" == ""
CFG=Drum - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Drum - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Drum - Win32 Release" && "$(CFG)" != "Drum - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Drum.mak" CFG="Drum - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Drum - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Drum - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "Drum - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\Drum.exe"


CLEAN :
	-@erase "$(INTDIR)\Drum.obj"
	-@erase "$(INTDIR)\Drum.res"
	-@erase "$(INTDIR)\DrumFile.obj"
	-@erase "$(INTDIR)\DrumTime.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\Drum.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /WX /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Drum.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Drum.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Drum.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Drum.pdb" /machine:I386 /out:"$(OUTDIR)\Drum.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Drum.obj" \
	"$(INTDIR)\DrumFile.obj" \
	"$(INTDIR)\DrumTime.obj" \
	"$(INTDIR)\Drum.res"

"$(OUTDIR)\Drum.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Drum - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\Drum.exe"


CLEAN :
	-@erase "$(INTDIR)\Drum.obj"
	-@erase "$(INTDIR)\Drum.res"
	-@erase "$(INTDIR)\DrumFile.obj"
	-@erase "$(INTDIR)\DrumTime.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\Drum.exe"
	-@erase "$(OUTDIR)\Drum.ilk"
	-@erase "$(OUTDIR)\Drum.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /WX /Gm /GX /ZI /Od /D "UNICODE" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Drum.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Drum.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Drum.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Drum.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Drum.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\Drum.obj" \
	"$(INTDIR)\DrumFile.obj" \
	"$(INTDIR)\DrumTime.obj" \
	"$(INTDIR)\Drum.res"

"$(OUTDIR)\Drum.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("Drum.dep")
!INCLUDE "Drum.dep"
!ELSE 
!MESSAGE Warning: cannot find "Drum.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Drum - Win32 Release" || "$(CFG)" == "Drum - Win32 Debug"
SOURCE=.\Drum.c

"$(INTDIR)\Drum.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\Drum.rc

"$(INTDIR)\Drum.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\DrumFile.c

"$(INTDIR)\DrumFile.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\DrumTime.c

"$(INTDIR)\DrumTime.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

