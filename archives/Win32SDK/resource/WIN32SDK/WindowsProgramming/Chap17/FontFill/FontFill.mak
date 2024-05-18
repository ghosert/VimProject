# Microsoft Developer Studio Generated NMAKE File, Based on FontFill.dsp
!IF "$(CFG)" == ""
CFG=FontFill - Win32 Debug
!MESSAGE No configuration specified. Defaulting to FontFill - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "FontFill - Win32 Release" && "$(CFG)" != "FontFill - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "FontFill.mak" CFG="FontFill - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "FontFill - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "FontFill - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "FontFill - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\FontFill.exe"


CLEAN :
	-@erase "$(INTDIR)\EzFont.obj"
	-@erase "$(INTDIR)\FontDemo.obj"
	-@erase "$(INTDIR)\FontDemo.res"
	-@erase "$(INTDIR)\FontFill.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\FontFill.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\FontFill.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FontDemo.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\FontFill.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\FontFill.pdb" /machine:I386 /out:"$(OUTDIR)\FontFill.exe" 
LINK32_OBJS= \
	"$(INTDIR)\EzFont.obj" \
	"$(INTDIR)\FontDemo.obj" \
	"$(INTDIR)\FontFill.obj" \
	"$(INTDIR)\FontDemo.res"

"$(OUTDIR)\FontFill.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FontFill - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\FontFill.exe"


CLEAN :
	-@erase "$(INTDIR)\EzFont.obj"
	-@erase "$(INTDIR)\FontDemo.obj"
	-@erase "$(INTDIR)\FontDemo.res"
	-@erase "$(INTDIR)\FontFill.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\FontFill.exe"
	-@erase "$(OUTDIR)\FontFill.ilk"
	-@erase "$(OUTDIR)\FontFill.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\FontFill.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FontDemo.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\FontFill.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\FontFill.pdb" /debug /machine:I386 /out:"$(OUTDIR)\FontFill.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\EzFont.obj" \
	"$(INTDIR)\FontDemo.obj" \
	"$(INTDIR)\FontFill.obj" \
	"$(INTDIR)\FontDemo.res"

"$(OUTDIR)\FontFill.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("FontFill.dep")
!INCLUDE "FontFill.dep"
!ELSE 
!MESSAGE Warning: cannot find "FontFill.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "FontFill - Win32 Release" || "$(CFG)" == "FontFill - Win32 Debug"
SOURCE=..\EzTest\EzFont.c

"$(INTDIR)\EzFont.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\EZTest\FontDemo.c

"$(INTDIR)\FontDemo.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\EZTest\FontDemo.rc

!IF  "$(CFG)" == "FontFill - Win32 Release"


"$(INTDIR)\FontDemo.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) /l 0x409 /fo"$(INTDIR)\FontDemo.res" /i "\ProgWin\Chap17\EZTest" /d "NDEBUG" $(SOURCE)


!ELSEIF  "$(CFG)" == "FontFill - Win32 Debug"


"$(INTDIR)\FontDemo.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) /l 0x409 /fo"$(INTDIR)\FontDemo.res" /i "\ProgWin\Chap17\EZTest" /d "_DEBUG" $(SOURCE)


!ENDIF 

SOURCE=.\FontFill.c

"$(INTDIR)\FontFill.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

