# Microsoft Developer Studio Generated NMAKE File, Based on Print1.dsp
!IF "$(CFG)" == ""
CFG=Print1 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Print1 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Print1 - Win32 Release" && "$(CFG)" != "Print1 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Print1.mak" CFG="Print1 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Print1 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Print1 - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "Print1 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\Print1.exe"


CLEAN :
	-@erase "$(INTDIR)\GetPrnDC.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Print1.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\Print1.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Print1.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Print1.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Print1.pdb" /machine:I386 /out:"$(OUTDIR)\Print1.exe" 
LINK32_OBJS= \
	"$(INTDIR)\GetPrnDC.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Print1.obj"

"$(OUTDIR)\Print1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Print1 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\Print1.exe"


CLEAN :
	-@erase "$(INTDIR)\GetPrnDC.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Print1.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\Print1.exe"
	-@erase "$(OUTDIR)\Print1.ilk"
	-@erase "$(OUTDIR)\Print1.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\Print1.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Print1.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Print1.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Print1.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\GetPrnDC.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Print1.obj"

"$(OUTDIR)\Print1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("Print1.dep")
!INCLUDE "Print1.dep"
!ELSE 
!MESSAGE Warning: cannot find "Print1.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Print1 - Win32 Release" || "$(CFG)" == "Print1 - Win32 Debug"
SOURCE=.\GetPrnDC.c

"$(INTDIR)\GetPrnDC.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\Print.c

"$(INTDIR)\Print.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\Print1.c

"$(INTDIR)\Print1.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

