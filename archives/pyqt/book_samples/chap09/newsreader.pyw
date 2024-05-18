#!/usr/bin/env python
# Copyright (c) 2008 Qtrac Ltd. All rights reserved.
# This program or module is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 2 of the License, or
# version 3 of the License, or (at your option) any later version. It is
# provided for educational purposes and is distributed in the hope that
# it will be useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
# the GNU General Public License for more details.

from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals
from future_builtins import *

import sys
from PyQt4.QtCore import (QSettings, QVariant, Qt, SIGNAL)
from PyQt4.QtGui import (QAction, QApplication, QIcon, QListWidget,
        QMainWindow, QSplitter, QTextBrowser)
import qrc_resources


__version__ = "1.0.0"


class MainWindow(QMainWindow):

    def __init__(self, parent=None):
        super(MainWindow, self).__init__(parent)
        self.groupsList = QListWidget()
        self.messagesList = QListWidget()
        self.messageView = QTextBrowser()

        self.messageSplitter = QSplitter(Qt.Vertical)
        self.messageSplitter.addWidget(self.messagesList)
        self.messageSplitter.addWidget(self.messageView)
        self.mainSplitter = QSplitter(Qt.Horizontal)
        self.mainSplitter.addWidget(self.groupsList)
        self.mainSplitter.addWidget(self.messageSplitter)
        self.setCentralWidget(self.mainSplitter)

        self.mainSplitter.setStretchFactor(0, 1)
        self.mainSplitter.setStretchFactor(1, 3)
        self.messageSplitter.setStretchFactor(0, 1)
        self.messageSplitter.setStretchFactor(1, 2)

        self.createMenusAndToolbars()

        settings = QSettings()
        self.restoreGeometry(
                settings.value("MainWindow/Geometry").toByteArray())
        self.restoreState(
                settings.value("MainWindow/State").toByteArray())
        self.messageSplitter.restoreState(
                settings.value("MessageSplitter").toByteArray())
        self.mainSplitter.restoreState(
                settings.value("MainSplitter").toByteArray())

        status = self.statusBar()
        status.setSizeGripEnabled(False)
        status.showMessage("Ready", 5000)
        self.setWindowTitle("News Reader")
        self.generateFakeData()


    def createMenusAndToolbars(self):
        fileMenu = self.menuBar().addMenu("&File")
        fileToolbar = self.addToolBar("File")
        fileToolbar.setObjectName("FileToolbar")
        for icon, text in (("new", "&New..."), ("open", "&Open..."),
                ("save", "&Save"), ("save", "Save &As..."),
                (None, None), ("quit", "&Quit")):
            if icon is None:
                fileMenu.addSeparator()
            else:
                action = QAction(QIcon(":/file{0}.png".format(icon)),
                                 text, self)
                if icon == "quit":
                    self.connect(action, SIGNAL("triggered()"),
                                 self.close)
                elif text != "Save &As...":
                    fileToolbar.addAction(action)
                fileMenu.addAction(action)

        editMenu = self.menuBar().addMenu("&Edit")
        editToolbar = self.addToolBar("Edit")
        editToolbar.setObjectName("EditToolbar")
        for icon, text in (("add", "&Add..."), ("edit", "&Edit..."),
                           ("delete", "&Remove")):
            action = QAction(QIcon(":/edit{0}.png".format(icon)),
                             text, self)
            editToolbar.addAction(action)
            editMenu.addAction(action)


    def closeEvent(self, event):
        if self.okToContinue():
            settings = QSettings()
            settings.setValue("MainWindow/Geometry",
                              QVariant(self.saveGeometry()))
            settings.setValue("MainWindow/State",
                              QVariant(self.saveState()))
            settings.setValue("MessageSplitter",
                    QVariant(self.messageSplitter.saveState()))
            settings.setValue("MainSplitter",
                    QVariant(self.mainSplitter.saveState()))
        else:
            event.ignore()


    def okToContinue(self):
        return True

    
    def generateFakeData(self):
        for group in ("ada", "apl", "asm.*", "asm370", "awk", "basic.*",
                "beta", "c.*", "c++.*", "clarion", "clipper.*", "clos",
                "clu", "cobol", "dylan", "eiffel", "forth.*",
                "fortran.*", "functional", "haskell", "hermes", "icon",
                "idl", "idl-pvwave", "java.*", "javascript", "labview",
                "limbo", "lisp.*", "logo", "misc", "ml.*", "modula2",
                "modula3", "mumps", "oberon", "objective-c", "pascal.*",
                "perl.*", "php.*", "pl1", "pop", "postscript",
                "prograph", "prolog", "python.*", "rexx.*", "ruby",
                "sathe", "scheme.*", "sigplan", "smalltalk.*", "tcl.*",
                "verilog", "vhdl", "visual.*", "vrml"):
            self.groupsList.addItem("comp.lang.{0}".format(group))
        for topic, author in (
                ("ANN: Einf\u00FChrung in die Programmierung mit Python",
                 "Ian Ozsvald",),
                ("SQLObject 0.7.3", "Oleg Broytmann",),
                ("ANN: Pyrex 0.9.5.1", "greg",),
                ("ANN: gozerbot IRC and JABBER bot", "bthate",),
                ("Extended deadline: CfP IEEE Software Special Issue on "
                 "Rapid Application Development with Dynamically Typed "
                 "Languages", "Laurence Tratt",),
                ("ANN: New python software community website in Chinese, "
                 "PythonNet.com", "Wenshan Du",),
                ("ANN: Plex 1.1.5 (Repost)", "greg",),
                ("ANN: Pyrex 0.9.5", "greg",),
                ("ftputil 2.2.1", "Stefan Schwarzer",),
                ("FlightFeather Social Networking Platform 0.3.1",
                 "George Belotsky",),
                ("OSCON 2007 Call for Participation Ends Soon",
                 "Kevin Altis",),
                ("ANN: tl.googlepagerank", "Thomas Lotze",),
                ("Dejavu 1.5.0RC1", "Robert Brewer",),
                ("PyCon: one week left for hotel registration",
                 "A.M. Kuchling",),
                ("FlightFeather Social Networking Platform 0.3.0",
                "George Belotsky",),
                ("SQLObject 0.8.0b2", "Oleg Broytmann",),
                ("SQLObject 0.7.3b1", "Oleg Broytmann",),
                ("ANN: Updated TkTreectrl wrapper module", "klappnase",),
                ("PyPy Trillke Sprints Feb/March 2007", "holger krekel",),
                ("wxPython 2.8.1.1", "Robin Dunn",),
                ("Movable Python 2.0.0 Final Available", "Fuzzyman",),
                ("ANN: Phebe 0.1.1", "Thomas Lotze",),
                ("Exception #03. Python seminar in Kiev city (Ukraine).",
                 "Mkdir",),
                ("FlightFeather Social Networking Platform 0.2.8",
                "George Belotsky",),
                ("ANN: Python Installation", "Ian Ozsvald",),
                ("ANN: pyGame Basics", "Ian Ozsvald",),
                ("PythonTidy 1.10", "Chuck Rhode",),
                ("Shed Skin Optimizing Python-to-C++ Compiler 0.0.10",
                 "Mark Dufour",),
                ("ANN : Karrigell 2.3.3", "Pierre Quentel",),
                ("ANN: amplee 0.4.0", "Sylvain Hellegouarch")):
            self.messagesList.addItem("{0} from {1}".format(topic, author))
        self.messageView.setHtml("""<table bgcolor=yellow>
<tr><td>Groups:</td><td>comp.lang.python.announce</td></tr>
<tr><td>From:</td><td>"Fuzzyman" &lt;fuzzy...@gmail.com&gt;</td></tr>
<tr><td>Subject:</td><td><b>[ANN] Movable Python 2.0.0 Final
Available</b></td></tr>
</table>

<h3>Movable Python 2.0.0 Final</h3>
<p>
<a href="http://www.voidspace.org.uk/python/movpy/">
http://www.voidspace.org.uk/python/movpy/</a>
is now available.

<p>
The new binaries are available for download from the groups page :

<p>Movable Python Groups Page
<a href="http://voidspace.tradebit.com/groups.php">
http://voidspace.tradebit.com/groups.php</a>
<p>
Binaries are uploaded for Python 2.2.3, 2.3.5, 2.4.4, 2.5 and the
MegaPack
<a href="http://www.voidspace.org.uk/python/movpy/megapack.html">
http://www.voidspace.org.uk/python/movpy/megapack.html</a>.
<p>
There is also a fresh version of the free demo, based on Python 2.3.5:

<p>Movable Python Trial Version
<a href="http://voidspace.tradebit.com/files.php/7007">
http://voidspace.tradebit.com/files.php/7007</a>

<h3>What is Movable Python</h3>

<p>
<b><i>Movable Python</i></b> is a distribution of Python, for Windows,
that can run without being installed. It means you can carry a full
development environment round on a USB stick.

<p>
It is also useful for testing programs with a 'clean Python install',
and testing programs with multiple versions of Python.

<p>
The GUI program launcher makes it a useful programmers tool, including
features like :

<ul>
<li>Log the output of all Python scripts run
<li>Enable Psyco for all scripts
<li>Quick Launch buttons for commonly used programs
<li>Configure multiple interpreters for use from a single interface
</ul>
<p>
It comes with the Pythonwin IDE and works fine with other IDEs like
SPE
<a href="http://developer.berlios.de/projects/python/">
http://developer.berlios.de/projects/python/</a>.
<p>
Plus many other features and bundled libraries.""")


def main():
    app = QApplication(sys.argv)
    app.setOrganizationName("Qtrac Ltd.")
    app.setOrganizationDomain("qtrac.eu")
    app.setApplicationName("News Reader")
    form = MainWindow()
    form.show()
    app.exec_()


main()

