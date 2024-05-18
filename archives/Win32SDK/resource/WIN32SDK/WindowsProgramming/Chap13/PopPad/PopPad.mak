# Microsoft Developer Studio Generated NMAKE File, Based on PopPad.dsp
!IF "$(CFG)" == ""
CFG=PopPad - Win32 Debug
!MESSAGE No configuration specified. Defaulting to PopPad - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "PopPad - Win32 Release" && "$(CFG)" != "PopPad - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PopPad.mak" CFG="PopPad - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PopPad - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "PopPad - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "PopPad - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\PopPad.exe"


CLEAN :
	-@erase "$(INTDIR)\PopFile.obj"
	-@erase "$(INTDIR)\PopFind.obj"
	-@erase "$(INTDIR)\PopFont.obj"
	-@erase "$(INTDIR)\PopPad.obj"
	-@erase "$(INTDIR)\poppad.res"
	-@erase "$(INTDIR)\PopPrnt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\PopPad.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\PopPad.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\poppad.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\PopPad.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\PopPad.pdb" /machine:I386 /out:"$(OUTDIR)\PopPad.exe" 
LINK32_OBJS= \
	"$(INTDIR)\PopFile.obj" \
	"$(INTDIR)\PopFind.obj" \
	"$(INTDIR)\PopFont.obj" \
	"$(INTDIR)\PopPad.obj" \
	"$(INTDIR)\PopPrnt.obj" \
	"$(INTDIR)\poppad.res"

"$(OUTDIR)\PopPad.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "PopPad - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\PopPad.exe"


CLEAN :
	-@erase "$(INTDIR)\PopFile.obj"
	-@erase "$(INTDIR)\PopFind.obj"
	-@erase "$(INTDIR)\PopFont.obj"
	-@erase "$(INTDIR)\PopPad.obj"
	-@erase "$(INTDIR)\poppad.res"
	-@erase "$(INTDIR)\PopPrnt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\PopPad.exe"
	-@erase "$(OUTDIR)\PopPad.ilk"
	-@erase "$(OUTDIR)\PopPad.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /WX /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /D "_UNICODE" /Fp"$(INTDIR)\PopPad.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\poppad.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\PopPad.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\PopPad.pdb" /debug /machine:I386 /out:"$(OUTDIR)\PopPad.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\PopFile.obj" \
	"$(INTDIR)\PopFind.obj" \
	"$(INTDIR)\PopFont.obj" \
	"$(INTDIR)\PopPad.obj" \
	"$(INTDIR)\PopPrnt.obj" \
	"$(INTDIR)\poppad.res"

"$(OUTDIR)\PopPad.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("PopPad.dep")
!INCLUDE "PopPad.dep"
!ELSE 
!MESSAGE Warning: cannot find "PopPad.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "PopPad - Win32 Release" || "$(CFG)" == "PopPad - Win32 Debug"
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


SOURCE=.\PopPrnt.c

"$(INTDIR)\PopPrnt.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

