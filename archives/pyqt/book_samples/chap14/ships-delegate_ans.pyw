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
from PyQt4.QtCore import (QFile, QIODevice, QRegExp, QString,
        QTextStream, QTimer, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QDialog, QFileDialog,
    QHBoxLayout, QLabel, QMessageBox, QPushButton, QSplitter,
    QTableView, QVBoxLayout, QWidget)
import ships_ans as ships

MAC = True
try:
    from PyQt4.QtGui import qt_mac_set_native_menubar
except ImportError:
    MAC = False


class MainForm(QDialog):

    def __init__(self, parent=None):
        super(MainForm, self).__init__(parent)

        self.model = ships.ShipTableModel(QString("ships.dat"))
        tableLabel1 = QLabel("Table &1")
        self.tableView1 = QTableView()
        tableLabel1.setBuddy(self.tableView1)
        self.tableView1.setModel(self.model)
        self.tableView1.setItemDelegate(ships.ShipDelegate(self))
        tableLabel2 = QLabel("Table &2")
        self.tableView2 = QTableView()
        tableLabel2.setBuddy(self.tableView2)
        self.tableView2.setModel(self.model)
        self.tableView2.setItemDelegate(ships.ShipDelegate(self))

        addShipButton = QPushButton("&Add Ship")
        removeShipButton = QPushButton("&Remove Ship")
        exportButton = QPushButton("E&xport...")
        quitButton = QPushButton("&Quit")
        if not MAC:
            addShipButton.setFocusPolicy(Qt.NoFocus)
            removeShipButton.setFocusPolicy(Qt.NoFocus)
            exportButton.setFocusPolicy(Qt.NoFocus)
            quitButton.setFocusPolicy(Qt.NoFocus)

        buttonLayout = QHBoxLayout()
        buttonLayout.addWidget(addShipButton)
        buttonLayout.addWidget(removeShipButton)
        buttonLayout.addWidget(exportButton)
        buttonLayout.addStretch()
        buttonLayout.addWidget(quitButton)
        splitter = QSplitter(Qt.Horizontal)
        vbox = QVBoxLayout()
        vbox.addWidget(tableLabel1)
        vbox.addWidget(self.tableView1)
        widget = QWidget()
        widget.setLayout(vbox)
        splitter.addWidget(widget)
        vbox = QVBoxLayout()
        vbox.addWidget(tableLabel2)
        vbox.addWidget(self.tableView2)
        widget = QWidget()
        widget.setLayout(vbox)
        splitter.addWidget(widget)
        layout = QVBoxLayout()
        layout.addWidget(splitter)
        layout.addLayout(buttonLayout)
        self.setLayout(layout)

        for tableView in (self.tableView1, self.tableView2):
            header = tableView.horizontalHeader()
            self.connect(header, SIGNAL("sectionClicked(int)"),
                         self.sortTable)
        self.connect(addShipButton, SIGNAL("clicked()"), self.addShip)
        self.connect(removeShipButton, SIGNAL("clicked()"),
                     self.removeShip)
        self.connect(exportButton, SIGNAL("clicked()"), self.export)
        self.connect(quitButton, SIGNAL("clicked()"), self.accept)

        self.setWindowTitle("Ships (delegate)")
        QTimer.singleShot(0, self.initialLoad)


    def initialLoad(self):
        if not QFile.exists(self.model.filename):
            for ship in ships.generateFakeShips():
                self.model.ships.append(ship)
                self.model.owners.add(unicode(ship.owner))
                self.model.countries.add(unicode(ship.country))
            self.model.reset()
            self.model.dirty = False
        else:
            try:
                self.model.load()
            except IOError, e:
                QMessageBox.warning(self, "Ships - Error",
                        "Failed to load: {0}".format(e))
        self.model.sortByName()
        self.resizeColumns()


    def resizeColumns(self):
        self.tableView1.resizeColumnsToContents()
        self.tableView2.resizeColumnsToContents()


    def reject(self):
        self.accept()


    def accept(self):
        if (self.model.dirty and
            QMessageBox.question(self, "Ships - Save?",
                    "Save unsaved changes?",
                    QMessageBox.Yes|QMessageBox.No) ==
                    QMessageBox.Yes):
            try:
                self.model.save()
            except IOError, e:
                QMessageBox.warning(self, "Ships - Error",
                        "Failed to save: {0}".format(e))
        QDialog.accept(self)

    
    def sortTable(self, section):
        if section in (ships.OWNER, ships.COUNTRY):
            self.model.sortByCountryOwner()
        elif section == ships.TEU:
            self.model.sortByTEU()
        else:
            self.model.sortByName()
        self.resizeColumns()


    def addShip(self):
        row = self.model.rowCount()
        self.model.insertRows(row)
        index = self.model.index(row, 0)
        tableView = self.tableView1
        if self.tableView2.hasFocus():
            tableView = self.tableView2
        tableView.setFocus()
        tableView.setCurrentIndex(index)
        tableView.edit(index)


    def removeShip(self):
        tableView = self.tableView1
        if self.tableView2.hasFocus():
            tableView = self.tableView2
        index = tableView.currentIndex()
        if not index.isValid():
            return
        row = index.row()
        name = self.model.data(
                    self.model.index(row, ships.NAME)).toString()
        owner = self.model.data(
                    self.model.index(row, ships.OWNER)).toString()
        country = self.model.data(
                    self.model.index(row, ships.COUNTRY)).toString()
        if (QMessageBox.question(self, "Ships - Remove", 
                (QString("Remove %1 of %2/%3?").arg(name).arg(owner)
                         .arg(country)),
                QMessageBox.Yes|QMessageBox.No) ==
                QMessageBox.No):
            return
        self.model.removeRows(row)
        self.resizeColumns()


    def export(self):
        filename = unicode(QFileDialog.getSaveFileName(self,
                "Ships - Choose Export File", ".", "Export files (*.txt)"))
        if not filename:
            return
        htmlTags = QRegExp(r"<[^>]+>")
        htmlTags.setMinimal(True)
        nonDigits = QRegExp("[., ]")
        self.model.sortByCountryOwner()
        fh = None
        try:
            fh = QFile(filename)
            if not fh.open(QIODevice.WriteOnly):
                raise IOError, unicode(fh.errorString())
            stream = QTextStream(fh)
            stream.setCodec("UTF-8")
            for row in range(self.model.rowCount()):
                name = self.model.data(
                        self.model.index(row, ships.NAME)).toString()
                owner = self.model.data(
                        self.model.index(row, ships.OWNER)).toString()
                country = self.model.data(
                        self.model.index(row, ships.COUNTRY)).toString()
                teu = self.model.data(
                        self.model.index(row, ships.TEU)).toString()
                teu = teu.replace(nonDigits, "").toInt()[0]
                description = self.model.data(
                        (self.model.index(row, ships.DESCRIPTION))
                                .toString())
                description = description.replace(htmlTags, "")
                stream << name << "|" << owner << "|" << country \
                       << "|" << teu << "|" << description << "\n"
        except (IOError, OSError), e:
            QMessageBox.warning(self, "Ships - Error",
                    "Failed to export: {0}".format(e))
        finally:
            if fh:
                fh.close()
        QMessageBox.warning(self, "Ships - Export",
                "Successfully exported ship to {0}".format(filename))


app = QApplication(sys.argv)
form = MainForm()
form.show()
app.exec_()

