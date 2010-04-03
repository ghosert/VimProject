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

import platform
import sys
from PyQt4.QtCore import (PYQT_VERSION_STR, QFile, QFileInfo, QSettings,
        QT_VERSION_STR, QTimer, QVariant, Qt, SIGNAL)
from PyQt4.QtGui import (QAction, QApplication, QFileDialog, QIcon,
        QKeySequence, QMainWindow, QMessageBox, QShortcut, QTableWidget,
        QTableWidgetItem)
import addeditmoviedlg
import moviedata
import qrc_resources


__version__ = "1.0.0"


class MainWindow(QMainWindow):

    def __init__(self, parent=None):
        super(MainWindow, self).__init__(parent)

        self.movies = moviedata.MovieContainer()
        self.table = QTableWidget()
        self.setCentralWidget(self.table)
        status = self.statusBar()
        status.setSizeGripEnabled(False)
        status.showMessage("Ready", 5000)

        fileNewAction = self.createAction("&New...", self.fileNew,
                QKeySequence.New, "filenew",
                "Create a movie data file")
        fileOpenAction = self.createAction("&Open...", self.fileOpen,
                QKeySequence.Open, "fileopen",
                "Open an existing  movie data file")
        fileSaveAction = self.createAction("&Save", self.fileSave,
                QKeySequence.Save, "filesave", "Save the movie data")
        fileSaveAsAction = self.createAction("Save &As...",
                self.fileSaveAs, icon="filesaveas",
                tip="Save the movie data using a new name")
        fileImportDOMAction = self.createAction(
                "&Import from XML (DOM)...", self.fileImportDOM,
                tip="Import the movie data from an XML file")
        fileImportSAXAction = self.createAction(
                "I&mport from XML (SAX)...", self.fileImportSAX,
                tip="Import the movie data from an XML file")
        fileExportXmlAction = self.createAction(
                "E&xport as XML...", self.fileExportXml,
                tip="Export the movie data to an XML file")
        fileQuitAction = self.createAction("&Quit", self.close,
                "Ctrl+Q", "filequit", "Close the application")
        editAddAction = self.createAction("&Add...", self.editAdd,
                "Ctrl+A", "editadd", "Add data about a movie")
        editEditAction = self.createAction("&Edit...", self.editEdit,
                "Ctrl+E", "editedit", "Edit the current movie's data")
        editRemoveAction = self.createAction("&Remove...",
                self.editRemove, "Del", "editdelete",
                "Remove a movie's data")
        helpAboutAction = self.createAction("&About", self.helpAbout,
                tip="About the application")

        fileMenu = self.menuBar().addMenu("&File")
        self.addActions(fileMenu, (fileNewAction, fileOpenAction,
                fileSaveAction, fileSaveAsAction, None,
                fileImportDOMAction, fileImportSAXAction,
                fileExportXmlAction, None, fileQuitAction))
        editMenu = self.menuBar().addMenu("&Edit")
        self.addActions(editMenu, (editAddAction, editEditAction,
                editRemoveAction))
        helpMenu = self.menuBar().addMenu("&Help")
        self.addActions(helpMenu, (helpAboutAction,))

        fileToolbar = self.addToolBar("File")
        fileToolbar.setObjectName("FileToolBar")
        self.addActions(fileToolbar, (fileNewAction, fileOpenAction,
                                      fileSaveAsAction))
        editToolbar = self.addToolBar("Edit")
        editToolbar.setObjectName("EditToolBar")
        self.addActions(editToolbar, (editAddAction, editEditAction,
                                      editRemoveAction))

        self.connect(self.table,
                SIGNAL("itemDoubleClicked(QTableWidgetItem*)"),
                self.editEdit)
        QShortcut(QKeySequence("Return"), self.table, self.editEdit)

        settings = QSettings()
        self.restoreGeometry(
                settings.value("MainWindow/Geometry").toByteArray())
        self.restoreState(settings.value("MainWindow/State").toByteArray())
        
        self.setWindowTitle("My Movies")
        QTimer.singleShot(0, self.loadInitialFile)


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
        if self.okToContinue():
            settings = QSettings()
            settings.setValue("LastFile",
                              QVariant(self.movies.filename()))
            settings.setValue("MainWindow/Geometry",
                              QVariant(self.saveGeometry()))
            settings.setValue("MainWindow/State",
                              QVariant(self.saveState()))
        else:
            event.ignore()


    def okToContinue(self):
        if self.movies.isDirty():
            reply = QMessageBox.question(self,
                    "My Movies - Unsaved Changes",
                    "Save unsaved changes?",
                    QMessageBox.Yes|QMessageBox.No|QMessageBox.Cancel)
            if reply == QMessageBox.Cancel:
                return False
            elif reply == QMessageBox.Yes:
                return self.fileSave()
        return True


    def loadInitialFile(self):
        settings = QSettings()
        fname = settings.value("LastFile").toString()
        if fname and QFile.exists(fname):
            ok, msg = self.movies.load(fname)
            self.statusBar().showMessage(msg, 5000)
        self.updateTable()


    def updateTable(self, current=None):
        self.table.clear()
        self.table.setRowCount(len(self.movies))
        self.table.setColumnCount(5)
        self.table.setHorizontalHeaderLabels(["Title", "Year", "Mins",
                "Acquired", "Notes"])
        self.table.setAlternatingRowColors(True)
        self.table.setEditTriggers(QTableWidget.NoEditTriggers)
        self.table.setSelectionBehavior(QTableWidget.SelectRows)
        self.table.setSelectionMode(QTableWidget.SingleSelection)
        selected = None
        for row, movie in enumerate(self.movies):
            item = QTableWidgetItem(movie.title)
            if current is not None and current == id(movie):
                selected = item
            item.setData(Qt.UserRole,
                         QVariant(long(id(movie))))
            self.table.setItem(row, 0, item)
            year = movie.year
            if year != movie.UNKNOWNYEAR:
                item = QTableWidgetItem("{0}".format(year))
                item.setTextAlignment(Qt.AlignCenter)
                self.table.setItem(row, 1, item)
            minutes = movie.minutes
            if minutes != movie.UNKNOWNMINUTES:
                item = QTableWidgetItem("{0}".format(minutes))
                item.setTextAlignment(Qt.AlignRight|Qt.AlignVCenter)
                self.table.setItem(row, 2, item)
            item = QTableWidgetItem(movie.acquired.toString(
                                    moviedata.DATEFORMAT))
            item.setTextAlignment(Qt.AlignRight|
                                  Qt.AlignVCenter)
            self.table.setItem(row, 3, item)
            notes = movie.notes
            if notes.length() > 40:
                notes = notes.left(39) + "..."
            self.table.setItem(row, 4, QTableWidgetItem(notes))
        self.table.resizeColumnsToContents()
        if selected is not None:
            selected.setSelected(True)
            self.table.setCurrentItem(selected)
            self.table.scrollToItem(selected)
        

    def fileNew(self):
        if not self.okToContinue():
            return
        self.movies.clear()
        self.statusBar().clearMessage()
        self.updateTable()


    def fileOpen(self):
        if not self.okToContinue():
            return
        path = (QFileInfo(self.movies.filename()).path()
                if not self.movies.filename().isEmpty() else ".")
        fname = QFileDialog.getOpenFileName(self,
                "My Movies - Load Movie Data", path,
                "My Movies data files ({0})".format(self.movies.formats()))
        if not fname.isEmpty():
            ok, msg = self.movies.load(fname)
            self.statusBar().showMessage(msg, 5000)
            self.updateTable()


    def fileSave(self):
        if self.movies.filename().isEmpty():
            return self.fileSaveAs()
        else:
            ok, msg = self.movies.save()
            self.statusBar().showMessage(msg, 5000)
            return ok


    def fileSaveAs(self):
        fname = (self.movies.filename()
                 if not self.movies.filename().isEmpty() else ".")
        fname = QFileDialog.getSaveFileName(self,
                "My Movies - Save Movie Data", fname,
                "My Movies data files ({0})".format(self.movies.formats()))
        if not fname.isEmpty():
            if not fname.contains("."):
                fname += ".mqb"
            ok, msg = self.movies.save(fname)
            self.statusBar().showMessage(msg, 5000)
            return ok
        return False


    def fileImportDOM(self):
        self.fileImport("dom")


    def fileImportSAX(self):
        self.fileImport("sax")


    def fileImport(self, format):
        if not self.okToContinue():
            return
        path = (QFileInfo(self.movies.filename()).path()
                if not self.movies.filename().isEmpty() else ".")
        fname = QFileDialog.getOpenFileName(self,
                "My Movies - Import Movie Data", path,
                "My Movies XML files (*.xml)")
        if not fname.isEmpty():
            if format == "dom":
                ok, msg = self.movies.importDOM(fname)
            else:
                ok, msg = self.movies.importSAX(fname)
            self.statusBar().showMessage(msg, 5000)
            self.updateTable()


    def fileExportXml(self):
        fname = self.movies.filename()
        if fname.isEmpty():
            fname = "."
        else:
            i = fname.lastIndexOf(".")
            if i > 0:
                fname = fname.left(i)
            fname += ".xml"
        fname = QFileDialog.getSaveFileName(self,
                "My Movies - Export Movie Data", fname,
                "My Movies XML files (*.xml)")
        if not fname.isEmpty():
            if not fname.contains("."):
                fname += ".xml"
            ok, msg = self.movies.exportXml(fname)
            self.statusBar().showMessage(msg, 5000)


    def editAdd(self):
        form = addeditmoviedlg.AddEditMovieDlg(self.movies, None,
                                               self)
        if form.exec_():
            self.updateTable(id(form.movie))


    def editEdit(self):
        movie = self.currentMovie()
        if movie is not None:
            form = addeditmoviedlg.AddEditMovieDlg(self.movies,
                                                   movie, self)
            if form.exec_():
                self.updateTable(id(movie))


    def editRemove(self):
        movie = self.currentMovie()
        if movie is not None:
            year = (" {0}".format(movie.year)
                    if movie.year != movie.UNKNOWNYEAR else "")
            if (QMessageBox.question(self,
                    "My Movies - Delete Movie",
                    "Delete Movie `{0}' {1}?".format(
                    movie.title, year),
                    QMessageBox.Yes|QMessageBox.No) ==
                QMessageBox.Yes):
                self.movies.delete(movie)
                self.updateTable()


    def currentMovie(self):
        row = self.table.currentRow()
        if row > -1:
            item = self.table.item(row, 0)
            id = item.data(Qt.UserRole).toLongLong()[0]
            return self.movies.movieFromId(id)
        return None


    def helpAbout(self):
        QMessageBox.about(self, "My Movies - About",
                """<b>My Movies</b> v {0}
                <p>Copyright &copy; 2008 Qtrac Ltd. 
                All rights reserved.
                <p>This application can be used to view some basic
                information about movies and to load and save the 
                movie data in a variety of custom file formats.
                <p>Python {1} - Qt {2} - PyQt {3} on {4}""".format(
                __version__, platform.python_version(),
                QT_VERSION_STR, PYQT_VERSION_STR,
                platform.system()))


def main():
    app = QApplication(sys.argv)
    app.setOrganizationName("Qtrac Ltd.")
    app.setOrganizationDomain("qtrac.eu")
    app.setApplicationName("My Movies")
    app.setWindowIcon(QIcon(":/icon.png"))
    form = MainWindow()
    form.show()
    app.exec_()


main()

