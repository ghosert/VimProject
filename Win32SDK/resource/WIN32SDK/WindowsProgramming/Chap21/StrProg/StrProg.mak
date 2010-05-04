# Microsoft Developer Studio Generated NMAKE File, Based on StrProg.dsp
!IF "$(CFG)" == ""
CFG=StrProg - Win32 Debug
!MESSAGE No configuration specified. Defaulting to StrProg - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "StrProg - Win32 Release" && "$(CFG)" != "StrProg - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "StrProg.mak" CFG="StrProg - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "StrProg - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "StrProg - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "StrProg - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\StrProg.exe"

!ELSE 

ALL : "StrLib - Win32 Release" "$(OUTDIR)\StrProg.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"StrLib - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\StrProg.obj"
	-@erase "$(INTDIR)\StrProg.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\StrProg.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\StrProg.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\StrProg.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\StrProg.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\StrProg.pdb" /machine:I386 /out:"$(OUTDIR)\StrProg.exe" 
LINK32_OBJS= \
	"$(INTDIR)\StrProg.obj" \
	"$(INTDIR)\StrProg.res" \
	"$(OUTDIR)\StrLib.lib"

"$(OUTDIR)\StrProg.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "StrProg - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\StrProg.exe"

!ELSE 

ALL : "StrLib - Win32 Debug" "$(OUTDIR)\StrProg.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"StrLib - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\StrProg.obj"
	-@erase "$(INTDIR)\StrProg.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\StrProg.exe"
	-@erase "$(OUTDIR)\StrProg.ilk"
	-@erase "$(OUTDIR)\StrProg.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\StrProg.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\StrProg.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\StrProg.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\StrProg.pdb" /debug /machine:I386 /out:"$(OUTDIR)\StrProg.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\StrProg.obj" \
	"$(INTDIR)\StrProg.res" \
	"$(OUTDIR)\StrLib.lib"

"$(OUTDIR)\StrProg.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("StrProg.dep")
!INCLUDE "StrProg.dep"
!ELSE 
!MESSAGE Warning: cannot find "StrProg.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "StrProg - Win32 Release" || "$(CFG)" == "StrProg - Win32 Debug"

!IF  "$(CFG)" == "StrProg - Win32 Release"

"StrLib - Win32 Release" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\StrLib.mak CFG="StrLib - Win32 Release" 
   cd "."

"StrLib - Win32 ReleaseCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\StrLib.mak CFG="StrLib - Win32 Release" RECURSE=1 CLEAN 
   cd "."

!ELSEIF  "$(CFG)" == "StrProg - Win32 Debug"

"StrLib - Win32 Debug" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\StrLib.mak CFG="StrLib - Win32 Debug" 
   cd "."

"StrLib - Win32 DebugCLEAN" : 
   cd "."
   $(MAKE) /$(MAKEFLAGS) /F .\StrLib.mak CFG="StrLib - Win32 Debug" RECURSE=1 CLEAN 
   cd "."

!ENDIF 

SOURCE=.\StrProg.c

"$(INTDIR)\StrProg.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\StrProg.rc

"$(INTDIR)\StrProg.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

