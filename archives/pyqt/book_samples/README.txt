Rapid GUI Programming with Python and Qt:
The Definitive Guide to PyQt Programming
by Mark Summerfield

ISBN: 0132354187

    IMPORTANT NOTE 

    These examples included in this archive are _not_ the ones shown in
    the book. The book's examples use Python 2.5/PyQt 4.3 and are
    available separately from the same web page,
    www.qtrac.eu/pyqtbook.html.
    The examples in this archive use Python 2.6/PyQt 4.4 and make use of
    the Python 3.0 features that are available in Python 2.6. Also, some
    of the examples include small improvements and all of them import
    the PyQt libraries using the form "from PyQt4.QtCore import (Qt,
    SIGNAL)" etc.---i.e., importing each item by name---rather than the
    "from PyQt4.QtCore import *" form used in the book's examples.

All the example programs and modules are copyright (c) Qtrac Ltd. 2008.
They are free software: you can redistribute them and/or modify them
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 2 of the License, or version 3
of the License, or (at your option) any later version. They are provided
for educational purposes and are distributed in the hope that they will
be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public Licenses (in files gpl-2.0.txt and gpl-3.0.txt) for more
details.

Two helper programs are provided: mkpyqt.py (a console application), and
makepyqt.pyw (a GUI application). These programs both do the same thing:
They run pyuic4, pyrcc4, pylupdate4, and lrelease with the correct
command line arguments. In some cases you may need to set the path to
pyuic4 (pyuic4.bat on Windows), and possibly to the other programs as
well. For mkpyqt.py this means editing the file itself (the paths are in
variables near the top); for makepyqt.pyw, click "More->Tool paths" and
set the paths there. The use of these programs is described in Chapter 7.

All the book's examples are designed to be educational, and many are
also designed to be useful. I hope that you find them helpful, and are
perhaps able to use some of them as starting points for your own
projects.

For commercial licenses visit http://www.trolltech.com for Qt, and
http://www.riverbankcomputing.co.uk for PyQt. Python itself can be
obtained from http://www.python.org and can be used for both commercial
and non-commercial purposes free of charge. See Appendix A for
information on obtaining and installing the necessary software.

Most of the icons are from KDE (The `K' Desktop Environment), and come
under KDE's LGPL license. (Visit http:///www.kde.org for more information.)

STOP PRESS

Chapter 3 describes an "OrderedDict". Unfortunately this name is
incorrect, it should have been called "SortedDict". (In Python mapping
terminology "ordered" means "order of insertion" and "sorted" means
"order of key"---I had used the C++ terminology.) I have kept the
wrongly named ordereddict.py module in the archive---after all, it works
fine---but also provided a correctly named SortedDict.py module that has
the same behavior, and that ought to be used instead. I have put a more
versatile (but theoretically slower) sorteddict module, with a different
API, on PyPI: http://pypi.python.org/pypi/sorteddict

Chapter 4. I've now added a new example, currency2.pyw that has one
extra line (to include Canadian dollars) and one line different (to sort
currency names case-insensitively) compared to currency.pyw. I've also
done a small theoretical improvement to the code.

Chapter 9 shows an SDI text editor (sditexteditor.pyw) that has a Window
menu in every main window with the list of all the application's
windows. This application's Window menu works on the basis of window
titles. But window titles may not be unique. For this reason I have now
added a new version (sditexteditor2.pyw) that has more sophisticated
updateWindowMenu() and raiseWindow() methods that use each window's
unique id() rather than their possibly non-unique window title.

Chapter 13 shows a PythonHighlighter (color syntax highlighting) class
in the pythoneditor.pyw and pythoneditor_ans.pyw applications. A
slightly more sophisticated version of this class is in my Sandbox
application. Sandbox is available from:
http://pypi.python.org/pypi/Sandbox
I have now added pythoneditor2.pyw which is a copy of
pythoneditor_ans.pyw but with the PythonHighlighter replaced with one
that uses similar logic as the one used in Sandbox since this works
better in more corner cases (but is slower).
I've also added printing2.pyw which has a bugfix and some tiny
improvements for both HTML and QPainter printing.
