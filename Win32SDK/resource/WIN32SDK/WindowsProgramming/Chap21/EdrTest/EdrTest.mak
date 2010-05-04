# Microsoft Developer Studio Generated NMAKE File, Based on EdrTest.dsp
!IF "$(CFG)" == ""
CFG=EdrTest - Win32 Debug
!MESSAGE No configuration specified. Defaulting to EdrTest - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "EdrTest - Win32 Release" && "$(CFG)" != "EdrTest - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "EdrTest.mak" CFG="EdrTest - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "EdrTest - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "EdrTest - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "EdrTest - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EdrTest.exe"

!ELSE 

ALL : "EdrLib - Win32 Release" "$(OUTDIR)\EdrTest.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"EdrLib - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\EdrTest.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\EdrTest.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\EdrTest.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EdrTest.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\EdrTest.pdb" /machine:I386 /out:"$(OUTDIR)\EdrTest.exe" 
LINK32_OBJS= \
	"$(INTDIR)\EdrTest.obj" \
	"$(OUTDIR)\EdrLib.lib"

"$(OUTDIR)\EdrTest.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "EdrTest - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\EdrTest.exe"

!ELSE 

ALL : "EdrLib - Win32 Debug" "$(OUTDIR)\EdrTest.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"EdrLib - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\EdrTest.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\EdrTest.exe"
	-@erase "$(OUTDIR)\EdrTest.ilk"
	-@erase "$(OUTDIR)\EdrTest.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\EdrTest.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\EdrTest.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\EdrTest.pdb" /debug /machine:I386 /out:"$(OUTDIR)\EdrTest.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\EdrTest.obj" \
	"$(OUTDIR)\EdrLib.lib"

"$(OUTDIR)\EdrTest.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("EdrTest.dep")
!INCLUDE "EdrTest.dep"
!ELSE 
!MESSAGE Warning: cannot find "EdrTest.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "EdrTest - Win32 Release" || "$(CFG)" == "EdrTest - Win32 Debug"

!IF  "$(CFG)" == "EdrTest - Win32 Release"

"EdrLib - Win32 Release" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\EdrLib.mak CFG="EdrLib - Win32 Release" 
   cd "."

"EdrLib - Win32 ReleaseCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\EdrLib.mak CFG="EdrLib - Win32 Release" RECURSE=1 CLEAN 
   cd "."

!ELSEIF  "$(CFG)" == "EdrTest - Win32 Debug"

"EdrLib - Win32 Debug" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\EdrLib.mak CFG="EdrLib - Win32 Debug" 
   cd "."

"EdrLib - Win32 DebugCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\EdrLib.mak CFG="EdrLib - Win32 Debug" RECURSE=1 CLEAN 
   cd "."

!ENDIF 

SOURCE=.\EdrTest.c

"$(INTDIR)\EdrTest.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

