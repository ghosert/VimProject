# Microsoft Developer Studio Generated NMAKE File, Based on ShowDib8.dsp
!IF "$(CFG)" == ""
CFG=ShowDib8 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to ShowDib8 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "ShowDib8 - Win32 Release" && "$(CFG)" != "ShowDib8 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ShowDib8.mak" CFG="ShowDib8 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ShowDib8 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "ShowDib8 - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "ShowDib8 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\ShowDib8.exe"


CLEAN :
	-@erase "$(INTDIR)\PackeDib.obj"
	-@erase "$(INTDIR)\ShowDib8.obj"
	-@erase "$(INTDIR)\ShowDib8.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\ShowDib8.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\ShowDib8.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\ShowDib8.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ShowDib8.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\ShowDib8.pdb" /machine:I386 /out:"$(OUTDIR)\ShowDib8.exe" 
LINK32_OBJS= \
	"$(INTDIR)\PackeDib.obj" \
	"$(INTDIR)\ShowDib8.obj" \
	"$(INTDIR)\ShowDib8.res"

"$(OUTDIR)\ShowDib8.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "ShowDib8 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\ShowDib8.exe"


CLEAN :
	-@erase "$(INTDIR)\PackeDib.obj"
	-@erase "$(INTDIR)\ShowDib8.obj"
	-@erase "$(INTDIR)\ShowDib8.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\ShowDib8.exe"
	-@erase "$(OUTDIR)\ShowDib8.ilk"
	-@erase "$(OUTDIR)\ShowDib8.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\ShowDib8.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\ShowDib8.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ShowDib8.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\ShowDib8.pdb" /debug /machine:I386 /out:"$(OUTDIR)\ShowDib8.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\PackeDib.obj" \
	"$(INTDIR)\ShowDib8.obj" \
	"$(INTDIR)\ShowDib8.res"

"$(OUTDIR)\ShowDib8.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("ShowDib8.dep")
!INCLUDE "ShowDib8.dep"
!ELSE 
!MESSAGE Warning: cannot find "ShowDib8.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "ShowDib8 - Win32 Release" || "$(CFG)" == "ShowDib8 - Win32 Debug"
SOURCE=..\ShowDib3\PackeDib.c

"$(INTDIR)\PackeDib.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\ShowDib8.c

"$(INTDIR)\ShowDib8.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\ShowDib8.rc

"$(INTDIR)\ShowDib8.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

