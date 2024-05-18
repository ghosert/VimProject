# Microsoft Developer Studio Generated NMAKE File, Based on MDIDemo.dsp
!IF "$(CFG)" == ""
CFG=MDIDemo - Win32 Debug
!MESSAGE No configuration specified. Defaulting to MDIDemo - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "MDIDemo - Win32 Release" && "$(CFG)" != "MDIDemo - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MDIDemo.mak" CFG="MDIDemo - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MDIDemo - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "MDIDemo - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "MDIDemo - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\MDIDemo.exe"


CLEAN :
	-@erase "$(INTDIR)\MDIDemo.obj"
	-@erase "$(INTDIR)\MDIDemo.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\MDIDemo.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\MDIDemo.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\MDIDemo.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MDIDemo.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\MDIDemo.pdb" /machine:I386 /out:"$(OUTDIR)\MDIDemo.exe" 
LINK32_OBJS= \
	"$(INTDIR)\MDIDemo.obj" \
	"$(INTDIR)\MDIDemo.res"

"$(OUTDIR)\MDIDemo.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "MDIDemo - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\MDIDemo.exe"


CLEAN :
	-@erase "$(INTDIR)\MDIDemo.obj"
	-@erase "$(INTDIR)\MDIDemo.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\MDIDemo.exe"
	-@erase "$(OUTDIR)\MDIDemo.ilk"
	-@erase "$(OUTDIR)\MDIDemo.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\MDIDemo.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\MDIDemo.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MDIDemo.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\MDIDemo.pdb" /debug /machine:I386 /out:"$(OUTDIR)\MDIDemo.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\MDIDemo.obj" \
	"$(INTDIR)\MDIDemo.res"

"$(OUTDIR)\MDIDemo.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("MDIDemo.dep")
!INCLUDE "MDIDemo.dep"
!ELSE 
!MESSAGE Warning: cannot find "MDIDemo.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "MDIDemo - Win32 Release" || "$(CFG)" == "MDIDemo - Win32 Debug"
SOURCE=.\MDIDemo.c

"$(INTDIR)\MDIDemo.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\MDIDemo.rc

"$(INTDIR)\MDIDemo.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

