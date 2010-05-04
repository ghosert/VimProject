# Microsoft Developer Studio Generated NMAKE File, Based on NoPopups.dsp
!IF "$(CFG)" == ""
CFG=NoPopups - Win32 Debug
!MESSAGE No configuration specified. Defaulting to NoPopups - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "NoPopups - Win32 Release" && "$(CFG)" != "NoPopups - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "NoPopups.mak" CFG="NoPopups - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "NoPopups - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "NoPopups - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "NoPopups - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\NoPopups.exe"


CLEAN :
	-@erase "$(INTDIR)\NoPopups.obj"
	-@erase "$(INTDIR)\NoPopups.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\NoPopups.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\NoPopups.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\NoPopups.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\NoPopups.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\NoPopups.pdb" /machine:I386 /out:"$(OUTDIR)\NoPopups.exe" 
LINK32_OBJS= \
	"$(INTDIR)\NoPopups.obj" \
	"$(INTDIR)\NoPopups.res"

"$(OUTDIR)\NoPopups.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "NoPopups - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\NoPopups.exe"


CLEAN :
	-@erase "$(INTDIR)\NoPopups.obj"
	-@erase "$(INTDIR)\NoPopups.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\NoPopups.exe"
	-@erase "$(OUTDIR)\NoPopups.ilk"
	-@erase "$(OUTDIR)\NoPopups.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\NoPopups.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\NoPopups.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\NoPopups.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\NoPopups.pdb" /debug /machine:I386 /out:"$(OUTDIR)\NoPopups.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\NoPopups.obj" \
	"$(INTDIR)\NoPopups.res"

"$(OUTDIR)\NoPopups.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("NoPopups.dep")
!INCLUDE "NoPopups.dep"
!ELSE 
!MESSAGE Warning: cannot find "NoPopups.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "NoPopups - Win32 Release" || "$(CFG)" == "NoPopups - Win32 Debug"
SOURCE=.\NoPopups.c

"$(INTDIR)\NoPopups.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\NoPopups.rc

"$(INTDIR)\NoPopups.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

