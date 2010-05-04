# Microsoft Developer Studio Generated NMAKE File, Based on RndRctMT.dsp
!IF "$(CFG)" == ""
CFG=RndRctMT - Win32 Debug
!MESSAGE No configuration specified. Defaulting to RndRctMT - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "RndRctMT - Win32 Release" && "$(CFG)" != "RndRctMT - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "RndRctMT.mak" CFG="RndRctMT - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "RndRctMT - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "RndRctMT - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "RndRctMT - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\RndRctMT.exe"


CLEAN :
	-@erase "$(INTDIR)\RndRctMT.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\RndRctMT.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\RndRctMT.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\RndRctMT.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\RndRctMT.pdb" /machine:I386 /out:"$(OUTDIR)\RndRctMT.exe" 
LINK32_OBJS= \
	"$(INTDIR)\RndRctMT.obj"

"$(OUTDIR)\RndRctMT.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "RndRctMT - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\RndRctMT.exe"


CLEAN :
	-@erase "$(INTDIR)\RndRctMT.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\RndRctMT.exe"
	-@erase "$(OUTDIR)\RndRctMT.ilk"
	-@erase "$(OUTDIR)\RndRctMT.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\RndRctMT.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\RndRctMT.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\RndRctMT.pdb" /debug /machine:I386 /out:"$(OUTDIR)\RndRctMT.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\RndRctMT.obj"

"$(OUTDIR)\RndRctMT.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("RndRctMT.dep")
!INCLUDE "RndRctMT.dep"
!ELSE 
!MESSAGE Warning: cannot find "RndRctMT.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "RndRctMT - Win32 Release" || "$(CFG)" == "RndRctMT - Win32 Debug"
SOURCE=.\RndRctMT.c

"$(INTDIR)\RndRctMT.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

