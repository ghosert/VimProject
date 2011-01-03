vim72src folder in VimProject is the source of vim 72, re-build it to support python or other features. You can get the source fom http://www.vim.org/download.php

Use mingw and change the file under vim72src/src/Make_ming.mak to open the python option.
1. uncomment #PYTHON=c:/python20
2. DYNAMIC_PYTHON=no
3. PYTHON_VER=22

run the command below in console:

make -f Make_ming.mak gvim.exe

rename $VIM\vim72\gvim.exe to gvim_default.exe
copy vim72src/src/gvim.exe to $VIM\vim72\

test line
