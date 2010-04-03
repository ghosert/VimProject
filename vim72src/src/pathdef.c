/* pathdef.c */ 
#include "vim.h" 
char_u *default_vim_dir = (char_u *)""; 
char_u *default_vimruntime_dir = (char_u *)""; 
char_u *all_cflags = (char_u *)"gcc -Iproto -DWIN32 -DWINVER=0x0400 -D_WIN32_WINNT=0x0400 -DHAVE_PATHDEF -DFEAT_BIG -DHAVE_GETTEXT -DHAVE_LOCALE_H -DDYNAMIC_GETTEXT -DFEAT_CSCOPE -DFEAT_NETBEANS_INTG -DFEAT_GUI_W32 -DFEAT_CLIPBOARD -DFEAT_MBYTE -DFEAT_MBYTE_IME -DDYNAMIC_IME -DDYNAMIC_ICONV -pipe -w -march=i386 -Wall -DFEAT_PYTHON -I D:/Python26/include -O3 -fomit-frame-pointer -freg-struct-return -s"; 
char_u *all_lflags = (char_u *)"gcc -Iproto -DWIN32 -DWINVER=0x0400 -D_WIN32_WINNT=0x0400 -DHAVE_PATHDEF -DFEAT_BIG -DHAVE_GETTEXT -DHAVE_LOCALE_H -DDYNAMIC_GETTEXT -DFEAT_CSCOPE -DFEAT_NETBEANS_INTG -DFEAT_GUI_W32 -DFEAT_CLIPBOARD -DFEAT_MBYTE -DFEAT_MBYTE_IME -DDYNAMIC_IME -DDYNAMIC_ICONV -pipe -w -march=i386 -Wall -DFEAT_PYTHON -I D:/Python26/include -O3 -fomit-frame-pointer -freg-struct-return -s -mwindows -o gvim.exe -lkernel32 -luser32 -lgdi32 -ladvapi32 -lcomdlg32 -lcomctl32 -lversion -lwsock32 -lole32 -luuid   -LD:/Python26/libs -lpython26 "; 
char_u *compiled_user = (char_u *)"Jiawei Zhang"; 
char_u *compiled_sys = (char_u *)"LOADTREND-GP91H"; 
