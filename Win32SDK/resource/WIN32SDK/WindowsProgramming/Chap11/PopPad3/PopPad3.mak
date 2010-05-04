# Microsoft Developer Studio Generated NMAKE File, Based on PopPad3.dsp
!IF "$(CFG)" == ""
CFG=PopPad3 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to PopPad3 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "PopPad3 - Win32 Release" && "$(CFG)" != "PopPad3 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PopPad3.mak" CFG="PopPad3 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PopPad3 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "PopPad3 - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "PopPad3 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\PopPad3.exe"


CLEAN :
	-@erase "$(INTDIR)\PopFile.obj"
	-@erase "$(INTDIR)\PopFind.obj"
	-@erase "$(INTDIR)\PopFont.obj"
	-@erase "$(INTDIR)\PopPad.obj"
	-@erase "$(INTDIR)\poppad.res"
	-@erase "$(INTDIR)\PopPrnt0.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\PopPad3.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\PopPad3.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\poppad.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\PopPad3.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\PopPad3.pdb" /machine:I386 /out:"$(OUTDIR)\PopPad3.exe" 
LINK32_OBJS= \
	"$(INTDIR)\PopFile.obj" \
	"$(INTDIR)\PopFind.obj" \
	"$(INTDIR)\PopFont.obj" \
	"$(INTDIR)\PopPad.obj" \
	"$(INTDIR)\PopPrnt0.obj" \
	"$(INTDIR)\poppad.res"

"$(OUTDIR)\PopPad3.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "PopPad3 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\PopPad3.exe"


CLEAN :
	-@erase "$(INTDIR)\PopFile.obj"
	-@erase "$(INTDIR)\PopFind.obj"
	-@erase "$(INTDIR)\PopFont.obj"
	-@erase "$(INTDIR)\PopPad.obj"
	-@erase "$(INTDIR)\poppad.res"
	-@erase "$(INTDIR)\PopPrnt0.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\PopPad3.exe"
	-@erase "$(OUTDIR)\PopPad3.ilk"
	-@erase "$(OUTDIR)\PopPad3.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /WX /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /D "_UNICODE" /Fp"$(INTDIR)\PopPad3.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\poppad.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\PopPad3.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\PopPad3.pdb" /debug /machine:I386 /out:"$(OUTDIR)\PopPad3.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\PopFile.obj" \
	"$(INTDIR)\PopFind.obj" \
	"$(INTDIR)\PopFont.obj" \
	"$(INTDIR)\PopPad.obj" \
	"$(INTDIR)\PopPrnt0.obj" \
	"$(INTDIR)\poppad.res"

"$(OUTDIR)\PopPad3.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("PopPad3.dep")
!INCLUDE "PopPad3.dep"
!ELSE 
!MESSAGE Warning: cannot find "PopPad3.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "PopPad3 - Win32 Release" || "$(CFG)" == "PopPad3 - Win32 Debug"
SOURCE=.\PopFile.c

"$(INTDIR)\PopFile.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\PopFind.c

"$(INTDIR)\PopFind.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\PopFont.c

"$(INTDIR)\PopFont.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\PopPad.c

"$(INTDIR)\PopPad.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\poppad.rc

"$(INTDIR)\poppad.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\PopPrnt0.c

"$(INTDIR)\PopPrnt0.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

