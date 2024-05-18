# Microsoft Developer Studio Generated NMAKE File, Based on Bounce.dsp
!IF "$(CFG)" == ""
CFG=Bounce - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Bounce - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Bounce - Win32 Release" && "$(CFG)" != "Bounce - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Bounce.mak" CFG="Bounce - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Bounce - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Bounce - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "Bounce - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\Bounce.exe"


CLEAN :
	-@erase "$(INTDIR)\Bounce.obj"
	-@erase "$(INTDIR)\PalAnim.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\Bounce.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Bounce.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Bounce.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Bounce.pdb" /machine:I386 /out:"$(OUTDIR)\Bounce.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Bounce.obj" \
	"$(INTDIR)\PalAnim.obj"

"$(OUTDIR)\Bounce.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Bounce - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\Bounce.exe"


CLEAN :
	-@erase "$(INTDIR)\Bounce.obj"
	-@erase "$(INTDIR)\PalAnim.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\Bounce.exe"
	-@erase "$(OUTDIR)\Bounce.ilk"
	-@erase "$(OUTDIR)\Bounce.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\Bounce.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Bounce.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Bounce.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Bounce.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\Bounce.obj" \
	"$(INTDIR)\PalAnim.obj"

"$(OUTDIR)\Bounce.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("Bounce.dep")
!INCLUDE "Bounce.dep"
!ELSE 
!MESSAGE Warning: cannot find "Bounce.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Bounce - Win32 Release" || "$(CFG)" == "Bounce - Win32 Debug"
SOURCE=.\Bounce.c

"$(INTDIR)\Bounce.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\PalAnim.c

"$(INTDIR)\PalAnim.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

