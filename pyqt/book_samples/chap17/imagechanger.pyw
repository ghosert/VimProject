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

import os
import platform
import sys
from PyQt4.QtCore import (PYQT_VERSION_STR, QFile, QFileInfo, QLocale,
        QSettings, QString, QT_VERSION_STR, QTimer, QTranslator,
        QVariant, Qt, SIGNAL)
from PyQt4.QtGui import (QAction, QActionGroup, QApplication,
        QDockWidget, QFileDialog, QFrame, QIcon, QImage, QImageReader,
        QImageWriter, QInputDialog, QKeySequence, QLabel, QListWidget,
        QMainWindow, QMessageBox, QPainter, QPixmap, QPrintDialog,
        QPrinter, QSpinBox)
import helpform
import newimagedlg
import resizedlg
import qrc_resources


__version__ = "1.0.0"


class MainWindow(QMainWindow):

    def __init__(self, parent=None):
        super(MainWindow, self).__init__(parent)

        self.image = QImage()
        self.dirty = False
        self.filename = None
        self.mirroredvertically = False
        self.mirroredhorizontally = False

        self.imageLabel = QLabel()
        self.imageLabel.setMinimumSize(200, 200)
        self.imageLabel.setAlignment(Qt.AlignCenter)
        self.imageLabel.setContextMenuPolicy(Qt.ActionsContextMenu)
        self.setCentralWidget(self.imageLabel)

        logDockWidget = QDockWidget("Log", self)
        logDockWidget.setObjectName("LogDockWidget")
        logDockWidget.setAllowedAreas(Qt.LeftDockWidgetArea|
                                      Qt.RightDockWidgetArea)
        self.listWidget = QListWidget()
        logDockWidget.setWidget(self.listWidget)
        self.addDockWidget(Qt.RightDockWidgetArea, logDockWidget)

        self.printer = None

        self.sizeLabel = QLabel()
        self.sizeLabel.setFrameStyle(QFrame.StyledPanel|QFrame.Sunken)
        status = self.statusBar()
        status.setSizeGripEnabled(False)
        status.addPermanentWidget(self.sizeLabel)
        self.statusBar().showMessage(self.tr("Ready"), 5000)

        fileNewAction = self.createAction(self.tr("&New..."),
                self.fileNew, QKeySequence.New, "filenew",
                self.tr("Create an image file"))
        fileOpenAction = self.createAction(self.tr("&Open..."),
                self.fileOpen, QKeySequence.Open, "fileopen",
                self.tr("Open an existing image file"))
        fileSaveAction = self.createAction(self.tr("&Save"),
                self.fileSave, QKeySequence.Save, "filesave",
                self.tr("Save the image"))
        fileSaveAsAction = self.createAction(self.tr("Save &As..."),
                self.fileSaveAs, icon="filesaveas",
                tip=self.tr("Save the image using a new name"))
        filePrintAction = self.createAction(self.tr("&Print"),
                self.filePrint, QKeySequence.Print, "fileprint",
                self.tr("Print the image"))
        fileQuitAction = self.createAction(self.tr("&Quit"),
                self.close, self.tr("Ctrl+Q"), "filequit",
                self.tr("Close the application"))
        editInvertAction = self.createAction(self.tr("&Invert"),
                self.editInvert, self.tr("Ctrl+I"), "editinvert",
                self.tr("Invert the image's colors"), True,
                "toggled(bool)")
        editSwapRedAndBlueAction = self.createAction(
                self.tr("Sw&ap Red and Blue"),
                self.editSwapRedAndBlue, self.tr("Ctrl+A"), "editswap",
                self.tr("Swap the image's red and blue "
                        "color components"), True, "toggled(bool)")
        editZoomAction = self.createAction(self.tr("&Zoom..."),
                self.editZoom, self.tr("Alt+Z"), "editzoom",
                self.tr("Zoom the image"))
        editResizeAction = self.createAction(self.tr("&Resize..."),
                self.editResize, self.tr("Ctrl+R"), "editresize",
                self.tr("Resize the image"))
        mirrorGroup = QActionGroup(self)
        editUnMirrorAction = self.createAction(self.tr("&Unmirror"),
                self.editUnMirror, self.tr("Ctrl+U"), "editunmirror",
                self.tr("Unmirror the image"), True, "toggled(bool)")
        mirrorGroup.addAction(editUnMirrorAction)
        editMirrorHorizontalAction = self.createAction(
                self.tr("Mirror &Horizontally"),
                self.editMirrorHorizontal, self.tr("Ctrl+H"),
                "editmirrorhoriz",
                self.tr("Horizontally mirror the image"), True,
                "toggled(bool)")
        mirrorGroup.addAction(editMirrorHorizontalAction)
        editMirrorVerticalAction = self.createAction(
                self.tr("Mirror &Vertically"), self.editMirrorVertical,
                self.tr("Ctrl+V"), "editmirrorvert",
                self.tr("Vertically mirror the image"), True,
                "toggled(bool)")
        mirrorGroup.addAction(editMirrorVerticalAction)
        editUnMirrorAction.setChecked(True)
        helpAboutAction = self.createAction(
                self.tr("&About Image Changer"), self.helpAbout)
        helpHelpAction = self.createAction(self.tr("&Help"),
                self.helpHelp, QKeySequence.HelpContents)

        self.fileMenu = self.menuBar().addMenu(self.tr("&File"))
        self.fileMenuActions = (fileNewAction, fileOpenAction,
                fileSaveAction, fileSaveAsAction, None,
                filePrintAction, fileQuitAction)
        self.connect(self.fileMenu, SIGNAL("aboutToShow()"),
                     self.updateFileMenu)
        editMenu = self.menuBar().addMenu(self.tr("&Edit"))
        self.addActions(editMenu, (editInvertAction,
                editSwapRedAndBlueAction, editZoomAction,
                editResizeAction))
        mirrorMenu = editMenu.addMenu(QIcon(":/editmirror.png"),
                self.tr("&Mirror"))
        self.addActions(mirrorMenu, (editUnMirrorAction,
                editMirrorHorizontalAction, editMirrorVerticalAction))
        helpMenu = self.menuBar().addMenu(self.tr("&Help"))
        self.addActions(helpMenu, (helpAboutAction, helpHelpAction))

        fileToolbar = self.addToolBar("File")
        fileToolbar.setObjectName("FileToolBar")
        self.addActions(fileToolbar, (fileNewAction, fileOpenAction,
                                      fileSaveAsAction))
        editToolbar = self.addToolBar("Edit")
        editToolbar.setObjectName("EditToolBar")
        self.addActions(editToolbar, (editInvertAction,
                editSwapRedAndBlueAction, editUnMirrorAction,
                editMirrorVerticalAction, editMirrorHorizontalAction))
        self.zoomSpinBox = QSpinBox()
        self.zoomSpinBox.setRange(1, 400)
        self.zoomSpinBox.setSuffix(" %")
        self.zoomSpinBox.setValue(100)
        self.zoomSpinBox.setToolTip(self.tr("Zoom the image"))
        self.zoomSpinBox.setStatusTip(self.zoomSpinBox.toolTip())
        self.zoomSpinBox.setFocusPolicy(Qt.NoFocus)
        self.connect(self.zoomSpinBox,
                     SIGNAL("valueChanged(int)"), self.showImage)
        editToolbar.addWidget(self.zoomSpinBox)

        self.addActions(self.imageLabel, (editInvertAction,
                editSwapRedAndBlueAction, editUnMirrorAction,
                editMirrorVerticalAction, editMirrorHorizontalAction))

        self.resetableActions = ((editInvertAction, False),
                                 (editSwapRedAndBlueAction, False),
                                 (editUnMirrorAction, True))

        settings = QSettings()
        self.recentFiles = settings.value("RecentFiles").toStringList()
        self.restoreGeometry(
                settings.value("MainWindow/Geometry").toByteArray())
        self.restoreState(
                settings.value("MainWindow/State").toByteArray())
        
        self.setWindowTitle(self.tr("Image Changer"))
        self.updateFileMenu()
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
            filename = (QVariant(QString(self.filename))
                    if self.filename is not None else QVariant())
            settings.setValue("LastFile", filename)
            recentFiles = (QVariant(self.recentFiles)
                           if self.recentFiles else QVariant())
            settings.setValue("RecentFiles", recentFiles)
            settings.setValue("MainWindow/Geometry",
                              QVariant(self.saveGeometry()))
            settings.setValue("MainWindow/State",
                              QVariant(self.saveState()))
        else:
            event.ignore()


    def okToContinue(self):
        if self.dirty:
            reply = QMessageBox.question(self,
                            self.tr("Image Changer - Unsaved Changes"),
                            self.tr("Save unsaved changes?"),
                            QMessageBox.Yes|QMessageBox.No|
                            QMessageBox.Cancel)
            if reply == QMessageBox.Cancel:
                return False
            elif reply == QMessageBox.Yes:
                return self.fileSave()
        return True


    def loadInitialFile(self):
        settings = QSettings()
        fname = unicode(settings.value("LastFile").toString())
        if fname and QFile.exists(fname):
            self.loadFile(fname)


    def updateStatus(self, message):
        self.statusBar().showMessage(message, 5000)
        self.listWidget.addItem(message)
        if self.filename is not None:
            self.setWindowTitle(self.tr("Image Changer - %1[*]").arg(
                    QFileInfo(self.filename).fileName()))
        elif not self.image.isNull():
            self.setWindowTitle(self.tr("Image Changer - Unnamed[*]"))
        else:
            self.setWindowTitle(self.tr("Image Changer[*]"))
        self.setWindowModified(self.dirty)


    def updateFileMenu(self):
        self.fileMenu.clear()
        self.addActions(self.fileMenu, self.fileMenuActions[:-1])
        current = (QString(self.filename)
                   if self.filename is not None else None)
        recentFiles = []
        for fname in self.recentFiles:
            if fname != current and QFile.exists(fname):
                recentFiles.append(fname)
        if recentFiles:
            self.fileMenu.addSeparator()
            for i, fname in enumerate(recentFiles):
                action = QAction(QIcon(":/icon.png"),
                        "&{0} {1}".format(i + 1,
                        QFileInfo(fname).fileName()), self)
                action.setData(QVariant(fname))
                self.connect(action, SIGNAL("triggered()"), self.loadFile)
                self.fileMenu.addAction(action)
        self.fileMenu.addSeparator()
        self.fileMenu.addAction(self.fileMenuActions[-1])


    def fileNew(self):
        if not self.okToContinue():
            return
        dialog = newimagedlg.NewImageDlg(self)
        if dialog.exec_():
            self.addRecentFile(self.filename)
            self.image = QImage()
            for action, check in self.resetableActions:
                action.setChecked(check)
            self.image = dialog.image()
            self.filename = None
            self.dirty = True
            self.showImage()
            self.sizeLabel.setText("{0} x {1}".format(self.image.width(),
                                                      self.image.height()))
            self.updateStatus(self.tr("Created new image"))


    def fileOpen(self):
        if not self.okToContinue():
            return
        dir = (os.path.dirname(self.filename)
               if self.filename is not None else ".")
        formats = ["*.{0}".format(unicode(format).lower()) for format in
                   QImageReader.supportedImageFormats()]
        fname = unicode(QFileDialog.getOpenFileName(self,
                    self.tr("Image Changer - Choose Image"), dir,
                    self.tr("Image files (%1)").arg(" ".join(formats))))
        if fname:
            self.loadFile(fname)


    def loadFile(self, fname=None):
        if fname is None:
            action = self.sender()
            if isinstance(action, QAction):
                fname = unicode(action.data().toString())
                if not self.okToContinue():
                    return
            else:
                return
        if fname:
            self.filename = None
            image = QImage(fname)
            if image.isNull():
                message = self.tr("Failed to read %1").arg(fname)
            else:
                self.addRecentFile(fname)
                self.image = QImage()
                for action, check in self.resetableActions:
                    action.setChecked(check)
                self.image = image
                self.filename = fname
                self.showImage()
                self.dirty = False
                self.sizeLabel.setText("{0} x {1}".format(
                            image.width(), image.height()))
                message = self.tr("Loaded %1").arg(QFileInfo(
                        self.filename).fileName())
            self.updateStatus(message)


    def addRecentFile(self, fname):
        if fname is None:
            return
        if not self.recentFiles.contains(fname):
            self.recentFiles.prepend(QString(fname))
            while self.recentFiles.count() > 9:
                self.recentFiles.takeLast()


    def fileSave(self):
        if self.image.isNull():
            return True
        if self.filename is None:
            return self.fileSaveAs()
        else:
            if self.image.save(self.filename, None):
                self.updateStatus(self.tr("Saved as %1").arg(
                        self.filename))
                self.dirty = False
                return True
            else:
                self.updateStatus(self.tr("Failed to save %1").arg(
                        self.filename))
                return False


    def fileSaveAs(self):
        if self.image.isNull():
            return True
        fname = self.filename if self.filename is not None else "."
        formats = ["*.%s" % unicode(format).lower() for format in
                   QImageWriter.supportedImageFormats()]
        fname = unicode(QFileDialog.getSaveFileName(self,
                        self.tr("Image Changer - Save Image"), fname,
                        self.tr("Image files (%1)").arg(
                                " ".join(formats))))
        if fname:
            if "." not in fname:
                fname += ".png"
            self.addRecentFile(fname)
            self.filename = fname
            return self.fileSave()
        return False


    def filePrint(self):
        if self.image.isNull():
            return
        if self.printer is None:
            self.printer = QPrinter(QPrinter.HighResolution)
            self.printer.setPageSize(QPrinter.Letter)
        form = QPrintDialog(self.printer, self)
        if form.exec_():
            painter = QPainter(self.printer)
            rect = painter.viewport()
            size = self.image.size()
            size.scale(rect.size(), Qt.KeepAspectRatio)
            painter.setViewport(rect.x(), rect.y(), size.width(),
                                size.height())
            painter.drawImage(0, 0, self.image)


    def editInvert(self, on):
        if self.image.isNull():
            return
        self.image.invertPixels()
        self.showImage()
        self.dirty = True
        self.updateStatus(self.tr("Inverted") if on else
                          self.tr("Uninverted"))


    def editSwapRedAndBlue(self, on):
        if self.image.isNull():
            return
        self.image = self.image.rgbSwapped()
        self.showImage()
        self.dirty = True
        self.updateStatus(self.tr("Swapped Red and Blue") if on else
                          self.tr("Unswapped Red and Blue"))


    def editUnMirror(self, on):
        if self.image.isNull():
            return
        if self.mirroredhorizontally:
            self.editMirrorHorizontal(False)
        if self.mirroredvertically:
            self.editMirrorVertical(False)


    def editMirrorHorizontal(self, on):
        if self.image.isNull():
            return
        self.image = self.image.mirrored(True, False)
        self.showImage()
        self.mirroredhorizontally = not self.mirroredhorizontally
        self.dirty = True
        self.updateStatus(self.tr("Mirrored Horizontally") if on else
                          self.tr("Unmirrored Horizontally"))


    def editMirrorVertical(self, on):
        if self.image.isNull():
            return
        self.image = self.image.mirrored(False, True)
        self.showImage()
        self.mirroredvertically = not self.mirroredvertically
        self.dirty = True
        self.updateStatus(self.tr("Mirrored Vertically") if on else
                          self.tr("Unmirrored Vertically"))


    def editZoom(self):
        if self.image.isNull():
            return
        percent, ok = QInputDialog.getInteger(self,
                self.tr("Image Changer - Zoom"),
                self.tr("Percent:"), self.zoomSpinBox.value(), 1, 400)
        if ok:
            self.zoomSpinBox.setValue(percent)


    def editResize(self):
        if self.image.isNull():
            return
        form = resizedlg.ResizeDlg(self.image.width(),
                                   self.image.height(), self)
        if form.exec_():
            width, height = form.result()
            if (width == self.image.width() and
                height == self.image.height()):
                self.statusBar().showMessage(
                        self.tr("Resized to the same size"), 5000)
            else:
                self.image = self.image.scaled(width, height)
                self.showImage()
                self.dirty = True
                size = "{0} x {1}".format(self.image.width(),
                                          self.image.height())
                self.sizeLabel.setText(size)
                self.updateStatus(self.tr("Resized to %1").arg(size))


    def showImage(self, percent=None):
        if self.image.isNull():
            return
        if percent is None:
            percent = self.zoomSpinBox.value()
        factor = percent / 100.0
        width = self.image.width() * factor
        height = self.image.height() * factor
        image = self.image.scaled(width, height, Qt.KeepAspectRatio)
        self.imageLabel.setPixmap(QPixmap.fromImage(image))


    def helpAbout(self):
        QMessageBox.about(self,
                self.tr("About Image Changer"),
                (self.tr("""<b>Image Changer</b> v %1
                <p>Copyright &copy; 2007 Qtrac Ltd. 
                All rights reserved.
                <p>This application can be used to perform
                simple image manipulations.
                <p>Python %2 - Qt %3 - PyQt %4 
                on %5""").arg(__version__)
                .arg(platform.python_version()).arg(QT_VERSION_STR)
                .arg(PYQT_VERSION_STR).arg(platform.system())))


    def helpHelp(self):
        form = helpform.HelpForm("index.html", self)
        form.show()


def main():
    app = QApplication(sys.argv)

    # Linux and Mac Terminal run:
    # $ LANG=fr ./imagechanger.pyw
    locale = QLocale.system().name()
    qtTranslator = QTranslator()
    if qtTranslator.load("qt_" + locale, ":/"):
        app.installTranslator(qtTranslator)
    appTranslator = QTranslator()
    if appTranslator.load("imagechanger_" + locale, ":/"):
        app.installTranslator(appTranslator)

    app.setOrganizationName("Qtrac Ltd.")
    app.setOrganizationDomain("qtrac.eu")
    app.setApplicationName(app.translate("main", "Image Changer"))
    app.setWindowIcon(QIcon(":/icon.png"))
    form = MainWindow()
    form.show()
    app.exec_()


main()

