__________________________________________________________________

                       Companion Disc Readme
                                for

                Programming Windows, Fifth Edition


                        by Charles Petzold

            Copyright (c) 1999 by Microsoft Corporation
          Portions copyright (c) 1999 by Charles Petzold
                        All Rights Reserved
___________________________________________________________________


README CONTENTS
 - WHAT'S ON THIS CD?
 - HOW TO USE THE CD
 - SUPPORT INFORMATION


WHAT'S ON THIS CD?
------------------
This CD contains the following items:

 - Example programs
 - Electronic version of the book
 - Microsoft Internet Explorer 4


HOW TO USE THE CD
-----------------

Example programs
----------------
This CD contains both the source code for every program discussed
in the book as well as executable files. You can open the source
files in the Microsoft Visual C++ environment or in your favorite
editor.

For each sample program, there are two different executable files,
one located in the RELEASE subdirectory and the other in the DEBUG
subdirectory. The executable in the DEBUG directory is intended to
be used for debugging. Generally, these executables have been
created with the default settings in Microsoft Visual C++ 6 with
one major exception: aside from other compiler and linker flags,
the executable in the DEBUG directory has been compiled with the
UNICODE identifier defined. In most cases, this means that the
executable in the DEBUG directory will not run under Windows 95 or
Windows 98; it will run under Windows NT only. The executable in
the RELEASE directory will run under Windows 95, Windows 98, or
Windows NT. The UNICODE identifier is discussed in Chapter 2 of the
book.

Although the executables are located in the RELEASE and DEBUG
subdirectories, some programs use data files that are located in
the parent directory, which is the directory that has the same name
as the program and which contains the program source code. Thus,
this parent directory must be the default directory when the
program is run or the program may not work correctly. If you always
run the programs from the Visual C++ environment, you won't have
any problems. You can also run the programs from the MS-DOS command
line from the parent directory like so:

    DEBUG\progname

or:

    RELEASE\progname

However, if you run the programs from Windows Explorer, the
programs won't be able to find their data files.

Some of the programs in Chapter 18 create files that are used by
other programs in the chapter. Thus, these programs must be run in
a specific order.

Some programs use features that are new with Windows 98 and Windows
NT 5. At the time of the creation of this CD, the Windows header
files included with Visual C++ 6 and distributed via MSDN and the
Microsoft web site did not assume Windows 98 development as a
default. Thus, to use Windows 98 features, a #define statement must
appear before the #include statement for the Windows header files,
like this:

    #define WINVER 0x0500
    #include <windows.h>

This #define statement is included in the appropriate programs on
the CD but is not shown in the program listings in the book.

Electronic version of book
--------------------------
To install the electronic version of this book:

1. Choose Run from the Start menu.
2. Type D:\EBook\Setup.exe (where D is your CD-ROM drive letter),
   and press Enter.
3. Follow the prompts on your screen. The setup program will offer
   to install Internet Explorer 4 for you if it is not found on
   your system. Internet Explorer is required to view the contents
   of the electronic book.

Microsoft Internet Explorer 4
-----------------------------
To install Microsoft Internet Explorer 4 without installing the
electronic version of this book:

1. Choose Run from the Start menu.
2. Type D:\EBook\IE4Setup\ie4setup.exe (where D is your CD-ROM
   drive letter), and press Enter.
3. Follow the prompts on your screen.


SUPPORT INFORMATION
-------------------
Every effort has been made to ensure the accuracy of the book
and the contents of this companion disc. Microsoft Press
provides corrections for books through the World Wide Web at

    http://mspress.microsoft.com/mspress/support/

If you have comments, questions, or ideas regarding the book or
this companion disc, please send them to Microsoft Press via 
e-mail to:

    mspinput@microsoft.com

or via postal mail to:

    Microsoft Press
    Attn: Programming Windows 5th ed. Editor
    One Microsoft Way
    Redmond, WA  98052-6399

Please note that product support is not offered through the
above addresses. 
