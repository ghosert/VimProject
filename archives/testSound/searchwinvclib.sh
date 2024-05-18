#!/bin/sh
# Author: Jiawei Zhang
# This script is used to check the .a file under MinGW/lib to find out which .a files should be used in gcc compile
# For example "gcc -o hellowin hellowin.o -mwindows" will lead to a undefined reference PlaySoundA error.
# You can type "./searchlib.sh PlaySoundA" to find out the lib is named libwinmm.a should be used.
# so you can modify your gcc command like "gcc -o hellowin hellowin.o -mwindows -lwinmm", then the compile will be passed.

PATHS="/cygdrive/e/VimProject/MSSDK/VC/LIB/"
if [ $# -eq 1 ]; then
    KEYWORD=$1
else
    echo "Please input the undefined reference name:"
    read KEYWORD
fi
echo "searching the path $PATHS ......"
cd $PATHS
STRINGS=`ls *`
for STRING in $STRINGS
    do
        nm $STRING 2>/dev/null | grep $KEYWORD -i
        error=`echo $?`
        if [ $error = "0" ]; then
            echo $STRING
        fi
    done
