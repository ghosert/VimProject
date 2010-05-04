# Microsoft Developer Studio Generated NMAKE File, Based on IconDemo.dsp
!IF "$(CFG)" == ""
CFG=IconDemo - Win32 Debug
!MESSAGE No configuration specified. Defaulting to IconDemo - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "IconDemo - Win32 Release" && "$(CFG)" != "IconDemo - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "IconDemo.mak" CFG="IconDemo - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "IconDemo - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "IconDemo - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "IconDemo - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\IconDemo.exe"


CLEAN :
	-@erase "$(INTDIR)\IconDemo.obj"
	-@erase "$(INTDIR)\IconDemo.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\IconDemo.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\IconDemo.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\IconDemo.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\IconDemo.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\IconDemo.pdb" /machine:I386 /out:"$(OUTDIR)\IconDemo.exe" 
LINK32_OBJS= \
	"$(INTDIR)\IconDemo.obj" \
	"$(INTDIR)\IconDemo.res"

"$(OUTDIR)\IconDemo.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "IconDemo - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\IconDemo.exe"


CLEAN :
	-@erase "$(INTDIR)\IconDemo.obj"
	-@erase "$(INTDIR)\IconDemo.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\IconDemo.exe"
	-@erase "$(OUTDIR)\IconDemo.ilk"
	-@erase "$(OUTDIR)\IconDemo.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\IconDemo.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\IconDemo.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\IconDemo.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\IconDemo.pdb" /debug /machine:I386 /out:"$(OUTDIR)\IconDemo.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\IconDemo.obj" \
	"$(INTDIR)\IconDemo.res"

"$(OUTDIR)\IconDemo.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("IconDemo.dep")
!INCLUDE "IconDemo.dep"
!ELSE 
!MESSAGE Warning: cannot find "IconDemo.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "IconDemo - Win32 Release" || "$(CFG)" == "IconDemo - Win32 Debug"
SOURCE=.\IconDemo.c

"$(INTDIR)\IconDemo.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\IconDemo.rc

"$(INTDIR)\IconDemo.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

