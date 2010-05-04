# Add -g for adding debug information.
# Download MinGW GDB and add it to MinGW/bin file
main:
	gcc -c -o bin/HelloMsg.o src/HelloMsg.c
	gcc -o bin/HelloMsg bin/HelloMsg.o -mwindows
	gcc -c -o bin/Unicode.o src/Unicode.c
	gcc -o bin/Unicode bin/Unicode.o -mwindows
# problems here, resolve it later: gcc -c -o bin/UnicodeConsole.o src/UnicodeConsole.c -finput-charset=gb2312
# problems here, resolve it later: gcc -o bin/UnicodeConsole bin/UnicodeConsole.o
	gcc -c -o bin/WindowsAndMessage.o src/WindowsAndMessage.c
	gcc -o bin/WindowsAndMessage bin/WindowsAndMessage.o -mwindows -lwinmm
	gcc -c -o bin/TextOutWithScrollBar.o src/TextOutWithScrollBar.c
	gcc -o bin/TextOutWithScrollBar bin/TextOutWithScrollBar.o -mwindows
	gcc -c -o bin/DevCaps.o src/DevCaps.c
	gcc -o bin/DevCaps bin/DevCaps.o -mwindows
	gcc -c -o bin/SineWave.o src/SineWave.c
	gcc -o bin/SineWave bin/SineWave.o -mwindows
	gcc -c -o bin/LineDemo.o src/LineDemo.c
	gcc -o bin/LineDemo bin/LineDemo.o -mwindows
	gcc -c -o bin/Bezier.o src/Bezier.c
	gcc -o bin/Bezier bin/Bezier.o -mwindows
	gcc -c -o bin/Polygon.o src/Polygon.c
	gcc -o bin/Polygon bin/Polygon.o -mwindows
	gcc -c -o bin/RandRect.o src/RandRect.c
	gcc -o bin/RandRect bin/RandRect.o -mwindows
	gcc -c -o bin/Clover.o src/Clover.c
	gcc -o bin/Clover bin/Clover.o -mwindows
	gcc -c -o bin/TextOutWithScrollBarAndKeyBoard.o src/TextOutWithScrollBarAndKeyBoard.c
	gcc -o bin/TextOutWithScrollBarAndKeyBoard bin/TextOutWithScrollBarAndKeyBoard.o -mwindows
	gcc -c -o bin/KeyView1.o src/KeyView1.c
	gcc -o bin/KeyView1 bin/KeyView1.o -mwindows
	gcc -c -o bin/StockFont.o src/StockFont.c
	gcc -o bin/StockFont bin/StockFont.o -mwindows
	gcc -c -o bin/typer.o src/typer.c
	gcc -o bin/typer bin/typer.o -mwindows
	gcc -c -o bin/Connect.o src/Connect.c
	gcc -o bin/Connect bin/Connect.o -mwindows
	gcc -c -o bin/CheckWithKeyboard.o src/CheckWithKeyboard.c
	gcc -o bin/CheckWithKeyboard bin/CheckWithKeyboard.o -mwindows
	gcc -c -o bin/CheckWithChildWindows.o src/CheckWithChildWindows.c
	gcc -o bin/CheckWithChildWindows bin/CheckWithChildWindows.o -mwindows
	gcc -c -o bin/BlokOut2.o src/BlokOut2.c
	gcc -o bin/BlokOut2 bin/BlokOut2.o -mwindows
	gcc -c -o bin/Beeper.o src/Beeper.c
	gcc -o bin/Beeper bin/Beeper.o -mwindows
	gcc -c -o bin/DigClock.o src/DigClock.c
	gcc -o bin/DigClock bin/DigClock.o -mwindows
	gcc -c -o bin/Clock.o src/Clock.c
	gcc -o bin/Clock bin/Clock.o -mwindows
	gcc -c -o bin/WhatClr.o src/WhatClr.c
	gcc -o bin/WhatClr bin/WhatClr.o -mwindows
	gcc -c -o bin/BtnLook.o src/BtnLook.c
	gcc -o bin/BtnLook bin/BtnLook.o -mwindows
	gcc -c -o bin/OwnDraw.o src/OwnDraw.c
	gcc -o bin/OwnDraw bin/OwnDraw.o -mwindows
	gcc -c -o bin/Colors.o src/Colors.c
	gcc -o bin/Colors bin/Colors.o -mwindows
	gcc -c -o bin/PopPAD1.o src/PopPAD1.c
	gcc -o bin/PopPAD1 bin/PopPAD1.o -mwindows
	gcc -c -o bin/Environ.o src/Environ.c
	gcc -o bin/Environ bin/Environ.o -mwindows
	gcc -c -o bin/Head.o src/Head.c
	gcc -o bin/Head bin/Head.o -mwindows
# BELOW IS THE CODE FOR MULTI-TRHEAD
	gcc -c -o bin/RndRctMt.o src/RndRctMt.c
	gcc -o bin/RndRctMt bin/RndRctMt.o -mwindows
	gcc -c -o bin/Multi2.o src/Multi2.c
	gcc -o bin/Multi2 bin/Multi2.o -mwindows
	gcc -c -o bin/BigJob1.o src/BigJob1.c
	gcc -o bin/BigJob1 bin/BigJob1.o -mwindows
	gcc -c -o bin/BigJob2.o src/BigJob2.c
	gcc -o bin/BigJob2 bin/BigJob2.o -mwindows
# BELOW IS THE CODE FOR DYNAMIC LINK LIBRARY
# In Linux .o means .obj, .so means .dll which is used for dynamic link, .a means output .lib in windows which is used for static link
# The difference is that .so can be used to dynamic link in linux directly while in windows you should have import .lib for dll to do the same thing.
#
# NOTICE:
# So for windows, there will be .obj, import .lib (dynamic link), output .lib (static link), .dll files
# for Linux, there will be .o, .so (dynamic link), .a (static link) files
# for MingW gcc, there are several ways to compile:
# 1. use .a(carried with MingW) as windows import/output .lib to compile windows exe.
# 2. use .lib file carried with Windows SDK directly, assume there is a xxx.lib. 'gcc -o ... -L./lib -lxxx'
# 3. use .dll to produce .a in step 1, and then use this .a to compile. pexports.exe in the $VIM/util/ while dllool is carried with MingW. I'v put a dlltoa.bat in $VIM\util for convenience.
#    pexports js32.dll > js32.def
#    dlltool --dllname js32.dll --def js32.def --output-lib libjsdll32.a
#(For step3, you may have to change .def file after you produce it. Eg: When you 'gcc -o ... -L./lib -ljsdll32' It tell u: undefined reference to 'PlaySoundA@12',
# you then modify .def file to add '@12' after PlaySoundA and run dlltool command above once again to reproduce libjsdll32.a file.)
#
# Eventually, the following four lines (exclude comments) are the way about how to write dynamic link for gcc win32 by using mingw.
	gcc -c -o bin/edrlib.o src/edrlib.c
	gcc -o bin/edrlib.dll bin/edrlib.o -shared -lgdi32 -Wl,--out-implib,bin/libedrlibdll.a
# For linux gcc -o bin/libedrlib.so bin/edrlib.o -shared
	gcc -c -o bin/edrtest.o src/edrtest.c
	gcc -o bin/edrtest bin/edrtest.o -mwindows -L./bin -ledrlibdll
# For linux gcc -o bin/edrtest bin/edrtest.o -L./bin -ledrlib [means libedrlib.so here]
	gcc -c -o bin/strlib.o src/strlib.c
	gcc -o bin/strlib.dll bin/strlib.o -shared -Wl,--out-implib,bin/libstrlibdll.a
	gcc -c -o bin/strprog.o src/strprog.c
	windres -o bin/strprogres.o src/strprog.rc
	gcc -o bin/strprog bin/strprog.o bin/strprogres.o -mwindows -L./bin -lstrlibdll
# BELOW IS THE CODE FOR PURE RESOURCE OF DYNAMIC LINK LIBRARY, SO THE DLL WILL BE LOADED BY LoadLibrary (TEXT ("BITLIB.DLL"))
# WE DO NOT NEED import lib file libbitlibdll.a ANY MORE.
	gcc -c -o bin/bitlib.o src/bitlib.c
	windres -o bin/bitlibres.o src/bitlib.rc
# BELOW, WE DO NOT NEED import lib file libbitlibdll.a ANY MORE BECAUSE OF DYNAMIC LOADING BY USING LoadLibrary (TEXT ("BITLIB.DLL"))
	gcc -o bin/bitlib.dll bin/bitlib.o bin/bitlibres.o -shared
	gcc -c -o bin/showbit.o src/showbit.c
	gcc -o bin/showbit bin/showbit.o -mwindows
clean:
	del bin\HelloMsg.o bin\HelloMsg.exe
	del bin\Unicode.o bin\Unicode.exe
	del bin\UnicodeConsole.o bin\UnicodeConsole.exe
	del bin\WindowsAndMessage.o bin\WindowsAndMessage.exe
	del bin\TextOutWithScrollBar.o bin\TextOutWithScrollBar.exe
	del bin\DevCaps.o bin\DevCaps.exe
	del bin\SineWave.o bin\SineWave.exe
	del bin\LineDemo.o bin\LineDemo.exe
	del bin\Bezier.o bin\Bezier.exe
	del bin\Polygon.o bin\Polygon.exe
	del bin\RandRect.o bin\RandRect.exe
	del bin\Clover.o bin\Clover.exe
	del bin\TextOutWithScrollBarAndKeyBoard.o bin\TextOutWithScrollBarAndKeyBoard.exe
	del bin\KeyView1.o bin\KeyView1.exe
	del bin\StockFont.o bin\StockFont.exe
	del bin\typer.o bin\typer.exe
	del bin\Connect.o bin\Connect.exe
	del bin\CheckWithKeyboard.o bin\CheckWithKeyboard.exe
	del bin\CheckWithChildWindows.o bin\CheckWithChildWindows.exe
	del bin\BlokOut2.o bin\BlokOut2.exe
	del bin\Beeper.o bin\Beeper.exe
	del bin\DigClock.o bin\DigClock.exe
	del bin\Clock.o bin\Clock.exe
	del bin\WhatClr.o bin\WhatClr.exe
	del bin\BtnLook.o bin\BtnLook.exe
	del bin\OwnDraw.o bin\OwnDraw.exe
	del bin\Colors.o bin\Colors.exe
	del bin\PopPAD1.o bin\PopPAD1.exe
	del bin\Environ.o bin\Environ.exe
	del bin\Head.o bin\Head.exe
	del bin\RndRctMt.o bin\RndRctMt.exe
	del bin\Multi2.o bin\Multi2.exe
	del bin\BigJob1.o bin\BigJob1.exe
	del bin\BigJob2.o bin\BigJob2.exe
	del bin\edrlib.o bin\edrlib.dll bin\libedrlibdll.a
	del bin\edrtest.o bin\edrtest.exe
	del bin\strlib.o bin\strlib.dll bin\libstrlibdll.a
	del bin\strprog.o bin\strprogres.o bin\strprog.exe
	del bin\bitlib.o bin\bitlib.dll bin\bitlibres.o
	del bin\showbit.o bin\showbit.exe
all:
