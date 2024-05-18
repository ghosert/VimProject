# Microsoft Developer Studio Generated NMAKE File, Based on Dibble.dsp
!IF "$(CFG)" == ""
CFG=Dibble - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Dibble - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Dibble - Win32 Release" && "$(CFG)" != "Dibble - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Dibble.mak" CFG="Dibble - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Dibble - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Dibble - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "Dibble - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\Dibble.exe"


CLEAN :
	-@erase "$(INTDIR)\Dibble.obj"
	-@erase "$(INTDIR)\Dibble.res"
	-@erase "$(INTDIR)\DibConv.obj"
	-@erase "$(INTDIR)\DibHelp.obj"
	-@erase "$(INTDIR)\DibPal.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\Dibble.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /WX /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Dibble.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Dibble.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Dibble.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Dibble.pdb" /machine:I386 /out:"$(OUTDIR)\Dibble.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Dibble.obj" \
	"$(INTDIR)\DibConv.obj" \
	"$(INTDIR)\DibHelp.obj" \
	"$(INTDIR)\DibPal.obj" \
	"$(INTDIR)\Dibble.res"

"$(OUTDIR)\Dibble.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Dibble - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\Dibble.exe"


CLEAN :
	-@erase "$(INTDIR)\Dibble.obj"
	-@erase "$(INTDIR)\Dibble.res"
	-@erase "$(INTDIR)\DibConv.obj"
	-@erase "$(INTDIR)\DibHelp.obj"
	-@erase "$(INTDIR)\DibPal.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\Dibble.exe"
	-@erase "$(OUTDIR)\Dibble.ilk"
	-@erase "$(OUTDIR)\Dibble.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /WX /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\Dibble.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Dibble.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Dibble.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Dibble.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Dibble.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\Dibble.obj" \
	"$(INTDIR)\DibConv.obj" \
	"$(INTDIR)\DibHelp.obj" \
	"$(INTDIR)\DibPal.obj" \
	"$(INTDIR)\Dibble.res"

"$(OUTDIR)\Dibble.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("Dibble.dep")
!INCLUDE "Dibble.dep"
!ELSE 
!MESSAGE Warning: cannot find "Dibble.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Dibble - Win32 Release" || "$(CFG)" == "Dibble - Win32 Debug"
SOURCE=.\Dibble.c

"$(INTDIR)\Dibble.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\Dibble.rc

"$(INTDIR)\Dibble.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\DibConv.c

"$(INTDIR)\DibConv.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\DibHelp.c

"$(INTDIR)\DibHelp.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\DibPal.c

"$(INTDIR)\DibPal.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

