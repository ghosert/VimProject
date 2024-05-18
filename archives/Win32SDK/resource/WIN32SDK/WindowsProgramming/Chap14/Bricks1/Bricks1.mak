# Microsoft Developer Studio Generated NMAKE File, Based on Bricks1.dsp
!IF "$(CFG)" == ""
CFG=Bricks1 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Bricks1 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Bricks1 - Win32 Release" && "$(CFG)" != "Bricks1 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Bricks1.mak" CFG="Bricks1 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Bricks1 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Bricks1 - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "Bricks1 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\Bricks1.exe"


CLEAN :
	-@erase "$(INTDIR)\Bricks1.obj"
	-@erase "$(INTDIR)\Bricks1.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\Bricks1.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Bricks1.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Bricks1.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Bricks1.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Bricks1.pdb" /machine:I386 /out:"$(OUTDIR)\Bricks1.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Bricks1.obj" \
	"$(INTDIR)\Bricks1.res"

"$(OUTDIR)\Bricks1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Bricks1 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\Bricks1.exe"


CLEAN :
	-@erase "$(INTDIR)\Bricks1.obj"
	-@erase "$(INTDIR)\Bricks1.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\Bricks1.exe"
	-@erase "$(OUTDIR)\Bricks1.ilk"
	-@erase "$(OUTDIR)\Bricks1.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\Bricks1.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Bricks1.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Bricks1.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Bricks1.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Bricks1.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\Bricks1.obj" \
	"$(INTDIR)\Bricks1.res"

"$(OUTDIR)\Bricks1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("Bricks1.dep")
!INCLUDE "Bricks1.dep"
!ELSE 
!MESSAGE Warning: cannot find "Bricks1.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Bricks1 - Win32 Release" || "$(CFG)" == "Bricks1 - Win32 Debug"
SOURCE=.\Bricks1.c

"$(INTDIR)\Bricks1.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\Bricks1.rc

"$(INTDIR)\Bricks1.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

