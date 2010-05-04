# Microsoft Developer Studio Generated NMAKE File, Based on DevCaps2.dsp
!IF "$(CFG)" == ""
CFG=DevCaps2 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to DevCaps2 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "DevCaps2 - Win32 Release" && "$(CFG)" != "DevCaps2 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "DevCaps2.mak" CFG="DevCaps2 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "DevCaps2 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "DevCaps2 - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "DevCaps2 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\DevCaps2.exe"


CLEAN :
	-@erase "$(INTDIR)\DevCaps2.obj"
	-@erase "$(INTDIR)\DevCaps2.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\DevCaps2.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\DevCaps2.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\DevCaps2.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\DevCaps2.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\DevCaps2.pdb" /machine:I386 /out:"$(OUTDIR)\DevCaps2.exe" 
LINK32_OBJS= \
	"$(INTDIR)\DevCaps2.obj" \
	"$(INTDIR)\DevCaps2.res"

"$(OUTDIR)\DevCaps2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "DevCaps2 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\DevCaps2.exe"


CLEAN :
	-@erase "$(INTDIR)\DevCaps2.obj"
	-@erase "$(INTDIR)\DevCaps2.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\DevCaps2.exe"
	-@erase "$(OUTDIR)\DevCaps2.ilk"
	-@erase "$(OUTDIR)\DevCaps2.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\DevCaps2.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\DevCaps2.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\DevCaps2.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\DevCaps2.pdb" /debug /machine:I386 /out:"$(OUTDIR)\DevCaps2.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\DevCaps2.obj" \
	"$(INTDIR)\DevCaps2.res"

"$(OUTDIR)\DevCaps2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("DevCaps2.dep")
!INCLUDE "DevCaps2.dep"
!ELSE 
!MESSAGE Warning: cannot find "DevCaps2.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "DevCaps2 - Win32 Release" || "$(CFG)" == "DevCaps2 - Win32 Debug"
SOURCE=.\DevCaps2.c

"$(INTDIR)\DevCaps2.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\DevCaps2.rc

"$(INTDIR)\DevCaps2.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

