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
from PyQt4.QtCore import (QFile, QFileInfo, QIODevice, QString,
        QTextStream, Qt, SIGNAL)
from PyQt4.QtGui import (QAction, QApplication, QFileDialog, QIcon,
        QKeySequence, QMainWindow, QMessageBox, QTextEdit)
import qrc_resources


__version__ = "1.0.0"


class MainWindow(QMainWindow):

    NextId = 1
    Instances = set()

    def __init__(self, filename=QString(), parent=None):
        super(MainWindow, self).__init__(parent)
        self.setAttribute(Qt.WA_DeleteOnClose)
        MainWindow.Instances.add(self)

        self.editor = QTextEdit()
        self.setCentralWidget(self.editor)

        fileNewAction = self.createAction("&New", self.fileNew,
                QKeySequence.New, "filenew", "Create a text file")
        fileOpenAction = self.createAction("&Open...", self.fileOpen,
                QKeySequence.Open, "fileopen",
                "Open an existing text file")
        fileSaveAction = self.createAction("&Save", self.fileSave,
                QKeySequence.Save, "filesave", "Save the text")
        fileSaveAsAction = self.createAction("Save &As...",
                self.fileSaveAs, icon="filesaveas",
                tip="Save the text using a new filename")
        fileSaveAllAction = self.createAction("Save A&ll",
                self.fileSaveAll, icon="filesave",
                tip="Save all the files")
        fileCloseAction = self.createAction("&Close", self.close,
                QKeySequence.Close, "fileclose",
                "Close this text editor")
        fileQuitAction = self.createAction("&Quit", self.fileQuit,
                "Ctrl+Q", "filequit", "Close the application")
        editCopyAction = self.createAction("&Copy", self.editor.copy,
                QKeySequence.Copy, "editcopy",
                "Copy text to the clipboard")
        editCutAction = self.createAction("Cu&t", self.editor.cut,
                QKeySequence.Cut, "editcut",
                "Cut text to the clipboard")
        editPasteAction = self.createAction("&Paste",
                self.editor.paste, QKeySequence.Paste, "editpaste",
                "Paste in the clipboard's text")

        fileMenu = self.menuBar().addMenu("&File")
        self.addActions(fileMenu, (fileNewAction, fileOpenAction,
                fileSaveAction, fileSaveAsAction, fileSaveAllAction,
                None, fileCloseAction, fileQuitAction))
        editMenu = self.menuBar().addMenu("&Edit")
        self.addActions(editMenu, (editCopyAction, editCutAction,
                                   editPasteAction))

        self.windowMenu = self.menuBar().addMenu("&Window")
        self.connect(self.windowMenu, SIGNAL("aboutToShow()"),
                     self.updateWindowMenu)

        fileToolbar = self.addToolBar("File")
        fileToolbar.setObjectName("FileToolbar")
        self.addActions(fileToolbar, (fileNewAction, fileOpenAction,
                                      fileSaveAction))
        editToolbar = self.addToolBar("Edit")
        editToolbar.setObjectName("EditToolbar")
        self.addActions(editToolbar, (editCopyAction, editCutAction,
                                      editPasteAction))

        self.connect(self, SIGNAL("destroyed(QObject*)"),
                     MainWindow.updateInstances)

        status = self.statusBar()
        status.setSizeGripEnabled(False)
        status.showMessage("Ready", 5000)

        self.resize(500, 600)

        self.filename = filename
        if self.filename.isEmpty():
            self.filename = QString("Unnamed-{0}.txt".format(
                                    MainWindow.NextId))
            MainWindow.NextId += 1
            self.editor.document().setModified(False)
            self.setWindowTitle("SDI Text Editor - {0}".format(
                                self.filename))
        else:
            self.loadFile()


    @staticmethod
    def updateInstances(qobj):
        MainWindow.Instances = (set([window for window
                in MainWindow.Instances if isAlive(window)]))


    def createAction(self, text, slot=None, shortcut=None, icon=None,
                     tip=None, checkable=False, signal="triggered()"):
        action = QAction(text, self)
        if icon is not None:
            action.setIcon(QIcon(":/{0}.png".format(icon)))
        if shortcut is not None:
            action.setShortcut(shortcut)
        if tip is not None:
            action.setToolTip(tip)
            action.setStatusTip(tip)
        if slot is not None:
            self.connect(action, SIGNAL(signal), slot)
        if checkable:
            action.setCheckable(True)
        return action


    def addActions(self, target, actions):
        for action in actions:
            if action is None:
                target.addSeparator()
            else:
                target.addAction(action)


    def closeEvent(self, event):
        if (self.editor.document().isModified() and
            QMessageBox.question(self,
                "SDI Text Editor - Unsaved Changes",
                "Save unsaved changes in {0}?".format(self.filename),
                QMessageBox.Yes|QMessageBox.No) ==
                QMessageBox.Yes):
            self.fileSave()


    def fileQuit(self):
        QApplication.closeAllWindows()


    def fileNew(self):
        MainWindow().show()


    def fileOpen(self):
        filename = QFileDialog.getOpenFileName(self,
                "SDI Text Editor -- Open File")
        if not filename.isEmpty():
            if (not self.editor.document().isModified() and
                self.filename.startsWith("Unnamed")):
                self.filename = filename
                self.loadFile()
            else:
                MainWindow(filename).show()


    def loadFile(self):
        fh = None
        try:
            fh = QFile(self.filename)
            if not fh.open(QIODevice.ReadOnly):
                raise IOError, unicode(fh.errorString())
            stream = QTextStream(fh)
            stream.setCodec("UTF-8")
            self.editor.setPlainText(stream.readAll())
            self.editor.document().setModified(False)
        except (IOError, OSError), e:
            QMessageBox.warning(self,
                    "SDI Text Editor -- Load Error",
                    "Failed to load {0}: {1}".format(self.filename, e))
        finally:
            if fh is not None:
                fh.close()
        self.editor.document().setModified(False)
        self.setWindowTitle("SDI Text Editor - {0}".format(
                QFileInfo(self.filename).fileName()))


    def fileSave(self):
        if self.filename.startsWith("Unnamed"):
            return self.fileSaveAs()
        fh = None
        try:
            fh = QFile(self.filename)
            if not fh.open(QIODevice.WriteOnly):
                raise IOError, unicode(fh.errorString())
            stream = QTextStream(fh)
            stream.setCodec("UTF-8")
            stream << self.editor.toPlainText()
            self.editor.document().setModified(False)
        except (IOError, OSError), e:
            QMessageBox.warning(self,
                    "SDI Text Editor -- Save Error",
                    "Failed to save {0}: {1}".format(self.filename, e))
            return False
        finally:
            if fh is not None:
                fh.close()
        return True


    def fileSaveAs(self):
        filename = QFileDialog.getSaveFileName(self,
                "SDI Text Editor -- Save File As", self.filename,
                "SDI Text files (*.txt *.*)")
        if not filename.isEmpty():
            self.filename = filename
            self.setWindowTitle("SDI Text Editor - {0}".format(
                    QFileInfo(self.filename).fileName()))
            return self.fileSave()
        return False


    def fileSaveAll(self):
        count = 0
        for window in MainWindow.Instances:
            if (isAlive(window) and
                window.editor.document().isModified()):
                if window.fileSave():
                    count += 1
        self.statusBar().showMessage("Saved {0} of {1} files".format(
                count, len(MainWindow.Instances)), 5000)


    def updateWindowMenu(self):
        self.windowMenu.clear()
        for window in MainWindow.Instances:
            if isAlive(window):
                self.windowMenu.addAction(window.windowTitle(),
                        self.raiseWindow)


    def raiseWindow(self):
        action = self.sender()
        if not isinstance(action, QAction):
            return
        for window in MainWindow.Instances:
            if (isAlive(window) and
                window.windowTitle() == action.text()):
                window.activateWindow()
                window.raise_()
                break


def isAlive(qobj):
    import sip
    try:
        sip.unwrapinstance(qobj)
    except RuntimeError:
        return False
    return True


app = QApplication(sys.argv)
app.setWindowIcon(QIcon(":/icon.png"))
MainWindow().show()
app.exec_()
