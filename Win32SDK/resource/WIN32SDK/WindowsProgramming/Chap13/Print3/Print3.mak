# Microsoft Developer Studio Generated NMAKE File, Based on Print3.dsp
!IF "$(CFG)" == ""
CFG=Print3 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Print3 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Print3 - Win32 Release" && "$(CFG)" != "Print3 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Print3.mak" CFG="Print3 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Print3 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Print3 - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "Print3 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\Print3.exe"


CLEAN :
	-@erase "$(INTDIR)\GetPrnDC.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Print.res"
	-@erase "$(INTDIR)\Print3.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\Print3.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Print3.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Print.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Print3.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Print3.pdb" /machine:I386 /out:"$(OUTDIR)\Print3.exe" 
LINK32_OBJS= \
	"$(INTDIR)\GetPrnDC.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Print3.obj" \
	"$(INTDIR)\Print.res"

"$(OUTDIR)\Print3.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Print3 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\Print3.exe"


CLEAN :
	-@erase "$(INTDIR)\GetPrnDC.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Print.res"
	-@erase "$(INTDIR)\Print3.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\Print3.exe"
	-@erase "$(OUTDIR)\Print3.ilk"
	-@erase "$(OUTDIR)\Print3.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\Print3.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Print.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Print3.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Print3.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Print3.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\GetPrnDC.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Print3.obj" \
	"$(INTDIR)\Print.res"

"$(OUTDIR)\Print3.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("Print3.dep")
!INCLUDE "Print3.dep"
!ELSE 
!MESSAGE Warning: cannot find "Print3.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Print3 - Win32 Release" || "$(CFG)" == "Print3 - Win32 Debug"
SOURCE=..\Print1\GetPrnDC.c

"$(INTDIR)\GetPrnDC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\Print1\Print.c

"$(INTDIR)\Print.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\Print.rc

"$(INTDIR)\Print.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\Print3.c

"$(INTDIR)\Print3.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

