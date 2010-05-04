# Microsoft Developer Studio Generated NMAKE File, Based on FontClip.dsp
!IF "$(CFG)" == ""
CFG=FontClip - Win32 Debug
!MESSAGE No configuration specified. Defaulting to FontClip - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "FontClip - Win32 Release" && "$(CFG)" != "FontClip - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "FontClip.mak" CFG="FontClip - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "FontClip - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "FontClip - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "FontClip - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\FontClip.exe"


CLEAN :
	-@erase "$(INTDIR)\EzFont.obj"
	-@erase "$(INTDIR)\FontClip.obj"
	-@erase "$(INTDIR)\FontDemo.obj"
	-@erase "$(INTDIR)\FontDemo.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\FontClip.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\FontClip.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FontDemo.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\FontClip.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\FontClip.pdb" /machine:I386 /out:"$(OUTDIR)\FontClip.exe" 
LINK32_OBJS= \
	"$(INTDIR)\EzFont.obj" \
	"$(INTDIR)\FontClip.obj" \
	"$(INTDIR)\FontDemo.obj" \
	"$(INTDIR)\FontDemo.res"

"$(OUTDIR)\FontClip.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FontClip - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\FontClip.exe"


CLEAN :
	-@erase "$(INTDIR)\EzFont.obj"
	-@erase "$(INTDIR)\FontClip.obj"
	-@erase "$(INTDIR)\FontDemo.obj"
	-@erase "$(INTDIR)\FontDemo.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\FontClip.exe"
	-@erase "$(OUTDIR)\FontClip.ilk"
	-@erase "$(OUTDIR)\FontClip.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\FontClip.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FontDemo.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\FontClip.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\FontClip.pdb" /debug /machine:I386 /out:"$(OUTDIR)\FontClip.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\EzFont.obj" \
	"$(INTDIR)\FontClip.obj" \
	"$(INTDIR)\FontDemo.obj" \
	"$(INTDIR)\FontDemo.res"

"$(OUTDIR)\FontClip.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("FontClip.dep")
!INCLUDE "FontClip.dep"
!ELSE 
!MESSAGE Warning: cannot find "FontClip.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "FontClip - Win32 Release" || "$(CFG)" == "FontClip - Win32 Debug"
SOURCE=..\EzTest\EzFont.c

"$(INTDIR)\EzFont.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\FontClip.c

"$(INTDIR)\FontClip.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=..\EZTest\FontDemo.c

"$(INTDIR)\FontDemo.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\EZTest\FontDemo.rc

!IF  "$(CFG)" == "FontClip - Win32 Release"


"$(INTDIR)\FontDemo.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) /l 0x409 /fo"$(INTDIR)\FontDemo.res" /i "\ProgWin\Chap17\EZTest" /d "NDEBUG" $(SOURCE)


!ELSEIF  "$(CFG)" == "FontClip - Win32 Debug"


"$(INTDIR)\FontDemo.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) /l 0x409 /fo"$(INTDIR)\FontDemo.res" /i "\ProgWin\Chap17\EZTest" /d "_DEBUG" $(SOURCE)


!ENDIF 


!ENDIF 

