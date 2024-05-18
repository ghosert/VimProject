# Microsoft Developer Studio Generated NMAKE File, Based on FormFeed.dsp
!IF "$(CFG)" == ""
CFG=FormFeed - Win32 Debug
!MESSAGE No configuration specified. Defaulting to FormFeed - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "FormFeed - Win32 Release" && "$(CFG)" != "FormFeed - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "FormFeed.mak" CFG="FormFeed - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "FormFeed - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "FormFeed - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "FormFeed - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\FormFeed.exe"


CLEAN :
	-@erase "$(INTDIR)\FormFeed.obj"
	-@erase "$(INTDIR)\GetPrnDC.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\FormFeed.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\FormFeed.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\FormFeed.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\FormFeed.pdb" /machine:I386 /out:"$(OUTDIR)\FormFeed.exe" 
LINK32_OBJS= \
	"$(INTDIR)\FormFeed.obj" \
	"$(INTDIR)\GetPrnDC.obj"

"$(OUTDIR)\FormFeed.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FormFeed - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\FormFeed.exe"


CLEAN :
	-@erase "$(INTDIR)\FormFeed.obj"
	-@erase "$(INTDIR)\GetPrnDC.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\FormFeed.exe"
	-@erase "$(OUTDIR)\FormFeed.ilk"
	-@erase "$(OUTDIR)\FormFeed.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\FormFeed.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\FormFeed.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\FormFeed.pdb" /debug /machine:I386 /out:"$(OUTDIR)\FormFeed.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\FormFeed.obj" \
	"$(INTDIR)\GetPrnDC.obj"

"$(OUTDIR)\FormFeed.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("FormFeed.dep")
!INCLUDE "FormFeed.dep"
!ELSE 
!MESSAGE Warning: cannot find "FormFeed.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "FormFeed - Win32 Release" || "$(CFG)" == "FormFeed - Win32 Debug"
SOURCE=.\FormFeed.c

"$(INTDIR)\FormFeed.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=..\Print1\GetPrnDC.c

"$(INTDIR)\GetPrnDC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

