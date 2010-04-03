# Nmake macros for building Windows 32-Bit apps

!include <Win32.Mak>

all: $(OUTDIR) $(OUTDIR)\audio-device.dll $(OUTDIR)\test-audio-device.exe

#----- If OUTDIR does not exist, then create directory
$(OUTDIR) :
    if not exist "$(OUTDIR)/$(NULL)" mkdir $(OUTDIR)

# Start comile and link dll lib.
$(OUTDIR)\audio-device.obj: audio-device.cpp
    $(cc) $(cflags) $(cvars) /D"AUDIO_DEVICE_EXPORTS" /WX /Fo"$(OUTDIR)\\" /Fd"$(OUTDIR)\\" audio-device.cpp

$(OUTDIR)\audio-device.dll: $(OUTDIR)\audio-device.obj
    $(link) $(conflags) /DLL -out:$(OUTDIR)\audio-device.dll $(OUTDIR)\audio-device.obj $(conlibs) Ole32.lib Quartz.lib

# Start comile and link test exe.
$(OUTDIR)\test-audio-device.obj: test-audio-device.cpp
    $(cc) $(cflags) $(cvars) /WX /Fo"$(OUTDIR)\\" /Fd"$(OUTDIR)\\" test-audio-device.cpp

$(OUTDIR)\test-audio-device.exe: $(OUTDIR)\test-audio-device.obj
    $(link) $(conflags) -out:$(OUTDIR)\test-audio-device.exe $(OUTDIR)\test-audio-device.obj $(conlibs) $(OUTDIR)\audio-device.lib
#--------------------- Clean Rule --------------------------------------------------------
# Rules for cleaning out those old files
clean:
        $(CLEANUP)
