# Microsoft Developer Studio Generated NMAKE File, Based on DibHeads.dsp
!IF "$(CFG)" == ""
CFG=DibHeads - Win32 Debug
!MESSAGE No configuration specified. Defaulting to DibHeads - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "DibHeads - Win32 Release" && "$(CFG)" != "DibHeads - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "DibHeads.mak" CFG="DibHeads - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "DibHeads - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "DibHeads - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "DibHeads - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\DibHeads.exe"


CLEAN :
	-@erase "$(INTDIR)\DibHeads.obj"
	-@erase "$(INTDIR)\DibHeads.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\DibHeads.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\DibHeads.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\DibHeads.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\DibHeads.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\DibHeads.pdb" /machine:I386 /out:"$(OUTDIR)\DibHeads.exe" 
LINK32_OBJS= \
	"$(INTDIR)\DibHeads.obj" \
	"$(INTDIR)\DibHeads.res"

"$(OUTDIR)\DibHeads.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "DibHeads - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\DibHeads.exe"


CLEAN :
	-@erase "$(INTDIR)\DibHeads.obj"
	-@erase "$(INTDIR)\DibHeads.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\DibHeads.exe"
	-@erase "$(OUTDIR)\DibHeads.ilk"
	-@erase "$(OUTDIR)\DibHeads.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\DibHeads.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\DibHeads.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\DibHeads.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\DibHeads.pdb" /debug /machine:I386 /out:"$(OUTDIR)\DibHeads.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\DibHeads.obj" \
	"$(INTDIR)\DibHeads.res"

"$(OUTDIR)\DibHeads.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("DibHeads.dep")
!INCLUDE "DibHeads.dep"
!ELSE 
!MESSAGE Warning: cannot find "DibHeads.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "DibHeads - Win32 Release" || "$(CFG)" == "DibHeads - Win32 Debug"
SOURCE=.\DibHeads.c

"$(INTDIR)\DibHeads.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\DibHeads.rc

"$(INTDIR)\DibHeads.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

