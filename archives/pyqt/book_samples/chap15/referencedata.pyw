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
import sys
from PyQt4.QtCore import (QFile, QString, QVariant, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QDialog, QDialogButtonBox, QMenu,
        QMessageBox, QTableView, QVBoxLayout)
from PyQt4.QtSql import (QSqlDatabase, QSqlQuery, QSqlTableModel)

MAC = True
try:
    from PyQt4.QtGui import qt_mac_set_native_menubar
except ImportError:
    MAC = False

ID, CATEGORY, SHORTDESC, LONGDESC = range(4)


class ReferenceDataDlg(QDialog):

    def __init__(self, parent=None):
        super(ReferenceDataDlg, self).__init__(parent)

        self.model = QSqlTableModel(self)
        self.model.setTable("reference")
        self.model.setSort(ID, Qt.AscendingOrder)
        self.model.setHeaderData(ID, Qt.Horizontal, QVariant("ID"))
        self.model.setHeaderData(CATEGORY, Qt.Horizontal,
                QVariant("Category"))
        self.model.setHeaderData(SHORTDESC, Qt.Horizontal,
                QVariant("Short Desc."))
        self.model.setHeaderData(LONGDESC, Qt.Horizontal,
                QVariant("Long Desc."))
        self.model.select()

        self.view = QTableView()
        self.view.setModel(self.model)
        self.view.setSelectionMode(QTableView.SingleSelection)
        self.view.setSelectionBehavior(QTableView.SelectRows)
        self.view.setColumnHidden(ID, True)
        self.view.resizeColumnsToContents()

        buttonBox = QDialogButtonBox()
        addButton = buttonBox.addButton("&Add",
                QDialogButtonBox.ActionRole)
        deleteButton = buttonBox.addButton("&Delete",
                QDialogButtonBox.ActionRole)
        sortButton = buttonBox.addButton("&Sort",
                QDialogButtonBox.ActionRole)
        if not MAC:
            addButton.setFocusPolicy(Qt.NoFocus)
            deleteButton.setFocusPolicy(Qt.NoFocus)
            sortButton.setFocusPolicy(Qt.NoFocus)

        menu = QMenu(self)
        sortByCategoryAction = menu.addAction("Sort by &Category")
        sortByDescriptionAction = menu.addAction("Sort by &Description")
        sortByIDAction = menu.addAction("Sort by &ID")
        sortButton.setMenu(menu)
        closeButton = buttonBox.addButton(QDialogButtonBox.Close)

        layout = QVBoxLayout()
        layout.addWidget(self.view)
        layout.addWidget(buttonBox)
        self.setLayout(layout)

        self.connect(addButton, SIGNAL("clicked()"), self.addRecord)
        self.connect(deleteButton, SIGNAL("clicked()"),
                     self.deleteRecord)
        self.connect(sortByCategoryAction, SIGNAL("triggered()"),
                     lambda: self.sort(CATEGORY))
        self.connect(sortByDescriptionAction, SIGNAL("triggered()"),
                     lambda: self.sort(SHORTDESC))
        self.connect(sortByIDAction, SIGNAL("triggered()"),
                     lambda: self.sort(ID))
        self.connect(closeButton, SIGNAL("clicked()"), self.accept)

        self.setWindowTitle("Reference Data")


    def addRecord(self):
        row = self.model.rowCount()
        self.model.insertRow(row)
        index = self.model.index(row, CATEGORY)
        self.view.setCurrentIndex(index)
        self.view.edit(index)


    def deleteRecord(self):
        index = self.view.currentIndex()
        if not index.isValid():
            return
        record = self.model.record(index.row())
        category = record.value(CATEGORY).toString()
        desc = record.value(SHORTDESC).toString()
        if (QMessageBox.question(self, "Reference Data",
                QString("Delete %1 from category %2?")
                .arg(desc).arg(category),
                QMessageBox.Yes|QMessageBox.No) ==
                QMessageBox.No):
            return
        self.model.removeRow(index.row())
        self.model.submitAll()


    def sort(self, column):
        self.model.setSort(column, Qt.AscendingOrder)
        self.model.select()


def main():
    app = QApplication(sys.argv)

    filename = os.path.join(os.path.dirname(__file__), "reference.db")
    create = not QFile.exists(filename)

    db = QSqlDatabase.addDatabase("QSQLITE")
    db.setDatabaseName(filename)
    if not db.open():
        QMessageBox.warning(None, "Reference Data",
            QString("Database Error: %1").arg(db.lastError().text()))
        sys.exit(1)

    if create:
        query = QSqlQuery()
        query.exec_("""CREATE TABLE reference (
                id INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,
                category VARCHAR(30) NOT NULL,
                shortdesc VARCHAR(20) NOT NULL,
                longdesc VARCHAR(80))""")

    form = ReferenceDataDlg()
    form.show()
    sys.exit(app.exec_())


main()

