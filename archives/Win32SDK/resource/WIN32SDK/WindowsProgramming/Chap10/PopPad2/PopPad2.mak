# Microsoft Developer Studio Generated NMAKE File, Based on PopPad2.dsp
!IF "$(CFG)" == ""
CFG=PopPad2 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to PopPad2 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "PopPad2 - Win32 Release" && "$(CFG)" != "PopPad2 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PopPad2.mak" CFG="PopPad2 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PopPad2 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "PopPad2 - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "PopPad2 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\PopPad2.exe"


CLEAN :
	-@erase "$(INTDIR)\PopPad2.obj"
	-@erase "$(INTDIR)\PopPad2.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\PopPad2.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\PopPad2.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\PopPad2.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\PopPad2.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\PopPad2.pdb" /machine:I386 /out:"$(OUTDIR)\PopPad2.exe" 
LINK32_OBJS= \
	"$(INTDIR)\PopPad2.obj" \
	"$(INTDIR)\PopPad2.res"

"$(OUTDIR)\PopPad2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "PopPad2 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\PopPad2.exe"


CLEAN :
	-@erase "$(INTDIR)\PopPad2.obj"
	-@erase "$(INTDIR)\PopPad2.res"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\PopPad2.exe"
	-@erase "$(OUTDIR)\PopPad2.ilk"
	-@erase "$(OUTDIR)\PopPad2.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\PopPad2.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\PopPad2.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\PopPad2.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\PopPad2.pdb" /debug /machine:I386 /out:"$(OUTDIR)\PopPad2.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\PopPad2.obj" \
	"$(INTDIR)\PopPad2.res"

"$(OUTDIR)\PopPad2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("PopPad2.dep")
!INCLUDE "PopPad2.dep"
!ELSE 
!MESSAGE Warning: cannot find "PopPad2.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "PopPad2 - Win32 Release" || "$(CFG)" == "PopPad2 - Win32 Debug"
SOURCE=.\PopPad2.c

"$(INTDIR)\PopPad2.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\PopPad2.rc

"$(INTDIR)\PopPad2.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

