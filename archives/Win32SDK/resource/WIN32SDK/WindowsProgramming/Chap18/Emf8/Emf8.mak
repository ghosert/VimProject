# Microsoft Developer Studio Generated NMAKE File, Based on Emf8.dsp
!IF "$(CFG)" == ""
CFG=Emf8 - Win32 Debug
!MESSAGE No configuration specified. Defaulting to Emf8 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Emf8 - Win32 Release" && "$(CFG)" != "Emf8 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Emf8.mak" CFG="Emf8 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Emf8 - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Emf8 - Win32 Debug" (based on "Win32 (x86) Application")
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

!IF  "$(CFG)" == "Emf8 - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\Emf8.exe"


CLEAN :
	-@erase "$(INTDIR)\Emf.obj"
	-@erase "$(INTDIR)\Emf.res"
	-@erase "$(INTDIR)\Emf8.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\Emf8.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Fp"$(INTDIR)\Emf8.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Emf.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Emf8.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Emf8.pdb" /machine:I386 /out:"$(OUTDIR)\Emf8.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Emf.obj" \
	"$(INTDIR)\Emf8.obj" \
	"$(INTDIR)\Emf.res"

"$(OUTDIR)\Emf8.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Emf8 - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\Emf8.exe"


CLEAN :
	-@erase "$(INTDIR)\Emf.obj"
	-@erase "$(INTDIR)\Emf.res"
	-@erase "$(INTDIR)\Emf8.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\Emf8.exe"
	-@erase "$(OUTDIR)\Emf8.ilk"
	-@erase "$(OUTDIR)\Emf8.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "UNICODE" /Fp"$(INTDIR)\Emf8.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32 
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Emf.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Emf8.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Emf8.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Emf8.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\Emf.obj" \
	"$(INTDIR)\Emf8.obj" \
	"$(INTDIR)\Emf.res"

"$(OUTDIR)\Emf8.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("Emf8.dep")
!INCLUDE "Emf8.dep"
!ELSE 
!MESSAGE Warning: cannot find "Emf8.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "Emf8 - Win32 Release" || "$(CFG)" == "Emf8 - Win32 Debug"
SOURCE=.\Emf.c

"$(INTDIR)\Emf.obj" : $(SOURCE) "$(INTDIR)"


SOURCE=.\Emf.rc

"$(INTDIR)\Emf.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\Emf8.c

"$(INTDIR)\Emf8.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 

