# Microsoft Developer Studio Project File - Name="libnyquist" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libnyquist - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libnyquist.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libnyquist.mak" CFG="libnyquist - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libnyquist - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libnyquist - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libnyquist - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "." /I "..\xlisp" /I "..\nyqsrc" /I "..\cmt" /I "..\fft" /I "..\tran" /I "..\snd" /I "..\sys" /I ".." /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\libnyquist.lib"

!ELSEIF  "$(CFG)" == "libnyquist - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "." /I "..\xlisp" /I "..\nyqsrc" /I "..\cmt" /I "..\fft" /I "..\tran" /I "..\snd" /I "..\sys" /I ".." /D "_DEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /FD /GZ /c
# SUBTRACT CPP /YX
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\libnyquistd.lib"

!ENDIF 

# Begin Target

# Name "libnyquist - Win32 Release"
# Name "libnyquist - Win32 Debug"
# Begin Group "nyqsrc"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\nyqsrc\add.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\avg.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\compose.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\debug.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\downsample.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\falloc.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\ffilterkit.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\fft.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\handlers.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\inverse.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\local.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\multiread.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\multiseq.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\probe.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\resamp.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\resampv.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\samples.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\seqext.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\seqfnint.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\seqinterf.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\sndfail.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\sndfnint.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\sndmax.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\sndread.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\sndseq.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\sndwrite.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\sound.c
# End Source File
# Begin Source File

SOURCE=..\nyqsrc\stats.c
# End Source File
# End Group
# Begin Group "xlisp"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\xlisp\extern.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\path.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlbfun.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlcont.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xldbug.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xldmem.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xleval.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlfio.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlftab.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlglob.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlimage.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlinit.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlio.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlisp.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xljump.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xllist.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlmath.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlobj.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlpp.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlprin.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlread.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlstr.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlsubr.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlsym.c
# End Source File
# Begin Source File

SOURCE=..\xlisp\xlsys.c
# End Source File
# End Group
# Begin Group "cmt"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\cmt\cext.c
# End Source File
# Begin Source File

SOURCE=..\cmt\cleanup.c
# End Source File
# Begin Source File

SOURCE=..\cmt\cmdline.c
# End Source File
# Begin Source File

SOURCE=..\cmt\cmtcmd.c
# End Source File
# Begin Source File

SOURCE=..\cmt\mem.c
# End Source File
# Begin Source File

SOURCE=..\cmt\midifile.c
# End Source File
# Begin Source File

SOURCE=..\cmt\midifns.c
# End Source File
# Begin Source File

SOURCE=..\cmt\moxc.c
# End Source File
# Begin Source File

SOURCE=..\cmt\record.c
# End Source File
# Begin Source File

SOURCE=..\cmt\seq.c
# End Source File
# Begin Source File

SOURCE=..\cmt\seqmread.c
# End Source File
# Begin Source File

SOURCE=..\cmt\seqmwrite.c
# End Source File
# Begin Source File

SOURCE=..\cmt\seqread.c
# End Source File
# Begin Source File

SOURCE=..\cmt\seqwrite.c
# End Source File
# Begin Source File

SOURCE=..\cmt\tempomap.c
# End Source File
# Begin Source File

SOURCE=..\cmt\timebase.c
# End Source File
# Begin Source File

SOURCE=..\cmt\userio.c
# End Source File
# End Group
# Begin Group "fft"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\fft\fftn.c
# End Source File
# Begin Source File

SOURCE=..\fft\fftn.h
# End Source File
# End Group
# Begin Group "nyx"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\nyx\nyx.c
# End Source File
# End Group
# Begin Group "tran"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\tran\alpass.c
# End Source File
# Begin Source File

SOURCE=..\tran\amosc.c
# End Source File
# Begin Source File

SOURCE=..\tran\areson.c
# End Source File
# Begin Source File

SOURCE=..\tran\aresoncv.c
# End Source File
# Begin Source File

SOURCE=..\tran\aresonvc.c
# End Source File
# Begin Source File

SOURCE=..\tran\aresonvv.c
# End Source File
# Begin Source File

SOURCE=..\tran\atone.c
# End Source File
# Begin Source File

SOURCE=..\tran\atonev.c
# End Source File
# Begin Source File

SOURCE=..\tran\biquad.c
# End Source File
# Begin Source File

SOURCE=..\tran\buzz.c
# End Source File
# Begin Source File

SOURCE=..\tran\chase.c
# End Source File
# Begin Source File

SOURCE=..\tran\clip.c
# End Source File
# Begin Source File

SOURCE=..\tran\congen.c
# End Source File
# Begin Source File

SOURCE=..\tran\const.c
# End Source File
# Begin Source File

SOURCE=..\tran\convolve.c
# End Source File
# Begin Source File

SOURCE=..\tran\coterm.c
# End Source File
# Begin Source File

SOURCE=..\tran\delay.c
# End Source File
# Begin Source File

SOURCE=..\tran\delaycv.c
# End Source File
# Begin Source File

SOURCE=..\tran\exp.c
# End Source File
# Begin Source File

SOURCE=..\tran\fmosc.c
# End Source File
# Begin Source File

SOURCE=..\tran\follow.c
# End Source File
# Begin Source File

SOURCE=..\tran\fromarraystream.c
# End Source File
# Begin Source File

SOURCE=..\tran\fromobject.c
# End Source File
# Begin Source File

SOURCE=..\tran\gate.c
# End Source File
# Begin Source File

SOURCE=..\tran\ifft.c
# End Source File
# Begin Source File

SOURCE=..\tran\integrate.c
# End Source File
# Begin Source File

SOURCE=..\tran\log.c
# End Source File
# Begin Source File

SOURCE=..\tran\maxv.c
# End Source File
# Begin Source File

SOURCE=..\tran\offset.c
# End Source File
# Begin Source File

SOURCE=..\tran\oneshot.c
# End Source File
# Begin Source File

SOURCE=..\tran\osc.c
# End Source File
# Begin Source File

SOURCE=..\tran\partial.c
# End Source File
# Begin Source File

SOURCE=..\tran\pluck.c
# End Source File
# Begin Source File

SOURCE=..\tran\prod.c
# End Source File
# Begin Source File

SOURCE=..\tran\pwl.c
# End Source File
# Begin Source File

SOURCE=..\tran\quantize.c
# End Source File
# Begin Source File

SOURCE=..\tran\recip.c
# End Source File
# Begin Source File

SOURCE=..\tran\reson.c
# End Source File
# Begin Source File

SOURCE=..\tran\resoncv.c
# End Source File
# Begin Source File

SOURCE=..\tran\resonvc.c
# End Source File
# Begin Source File

SOURCE=..\tran\resonvv.c
# End Source File
# Begin Source File

SOURCE=..\tran\sampler.c
# End Source File
# Begin Source File

SOURCE=..\tran\scale.c
# End Source File
# Begin Source File

SOURCE=..\tran\shape.c
# End Source File
# Begin Source File

SOURCE=..\tran\sine.c
# End Source File
# Begin Source File

SOURCE=..\tran\siosc.c
# End Source File
# Begin Source File

SOURCE=..\tran\slope.c
# End Source File
# Begin Source File

SOURCE=..\tran\tapv.c
# End Source File
# Begin Source File

SOURCE=..\tran\tone.c
# End Source File
# Begin Source File

SOURCE=..\tran\tonev.c
# End Source File
# Begin Source File

SOURCE=..\tran\upsample.c
# End Source File
# Begin Source File

SOURCE=..\tran\white.c
# End Source File
# End Group
# Begin Group "snd"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\snd\audionone.c
# End Source File
# Begin Source File

SOURCE=..\snd\ieeecvt.c

!IF  "$(CFG)" == "libnyquist - Win32 Release"

!ELSEIF  "$(CFG)" == "libnyquist - Win32 Debug"

# ADD CPP /D "EXT"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\snd\snd.c
# End Source File
# Begin Source File

SOURCE=..\snd\sndcvt.c
# End Source File
# Begin Source File

SOURCE=..\snd\sndheader.c
# End Source File
# Begin Source File

SOURCE=..\snd\sndio.c
# End Source File
# Begin Source File

SOURCE=..\snd\sndwin32.c
# End Source File
# End Group
# Begin Group "sys"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\sys\term.c
# End Source File
# End Group
# Begin Source File

SOURCE=.\winfun.c
# End Source File
# End Target
# End Project
