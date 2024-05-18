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
from PyQt4.QtCore import (QStringList, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QDialog, QHBoxLayout,
        QInputDialog, QLineEdit, QListWidget, QMessageBox, QPushButton,
        QVBoxLayout)

MAC = True
try:
    from PyQt4.QtGui import qt_mac_set_native_menubar
except ImportError:
    MAC = False


class StringListDlg(QDialog):

    def __init__(self, name, stringlist=None, parent=None):
        super(StringListDlg, self).__init__(parent)

        self.name = name

        self.listWidget = QListWidget()
        if stringlist is not None:
            self.listWidget.addItems(stringlist)
            self.listWidget.setCurrentRow(0)
        buttonLayout = QVBoxLayout()
        for text, slot in (("&Add...", self.add),
                           ("&Edit...", self.edit),
                           ("&Remove...", self.remove),
                           ("&Up", self.up),
                           ("&Down", self.down),
                           ("&Sort", self.listWidget.sortItems),
                           ("Close", self.accept)):
            button = QPushButton(text)
            if not MAC:
                button.setFocusPolicy(Qt.NoFocus)
            if text == "Close":
                buttonLayout.addStretch()
            buttonLayout.addWidget(button)
            self.connect(button, SIGNAL("clicked()"), slot)
        layout = QHBoxLayout()
        layout.addWidget(self.listWidget)
        layout.addLayout(buttonLayout)
        self.setLayout(layout)
        self.setWindowTitle("Edit {0} List".format(self.name))


    def add(self):
        row = self.listWidget.currentRow()
        title = "Add {0}".format(self.name)
        string, ok = QInputDialog.getText(self, title, title)
        if ok and not string.isEmpty():
            self.listWidget.insertItem(row, string)


    def edit(self):
        row = self.listWidget.currentRow()
        item = self.listWidget.item(row)
        if item is not None:
            title = "Edit {0}".format(self.name)
            string, ok = QInputDialog.getText(self, title, title,
                    QLineEdit.Normal, item.text())
            if ok and not string.isEmpty():
                item.setText(string)


    def remove(self):
        row = self.listWidget.currentRow()
        item = self.listWidget.item(row)
        if item is None:
            return
        reply = QMessageBox.question(self, "Remove {0}".format(
                self.name), "Remove {0} `{1}'?".format(
                self.name, unicode(item.text())),
                QMessageBox.Yes|QMessageBox.No)
        if reply == QMessageBox.Yes:
            item = self.listWidget.takeItem(row)
            del item


    def up(self):
        row = self.listWidget.currentRow()
        if row >= 1:
            item = self.listWidget.takeItem(row)
            self.listWidget.insertItem(row - 1, item)
            self.listWidget.setCurrentItem(item)


    def down(self):
        row = self.listWidget.currentRow()
        if row < self.listWidget.count() - 1:
            item = self.listWidget.takeItem(row)
            self.listWidget.insertItem(row + 1, item)
            self.listWidget.setCurrentItem(item)


    def reject(self):
        self.accept()


    def accept(self):
        self.stringlist = QStringList()
        for row in range(self.listWidget.count()):
            self.stringlist.append(self.listWidget.item(row).text())
        self.emit(SIGNAL("acceptedList(QStringList)"), self.stringlist)
        QDialog.accept(self)


if __name__ == "__main__":
    fruit = ["Banana", "Apple", "Elderberry", "Clementine", "Fig",
        "Guava", "Mango", "Honeydew Melon", "Date", "Watermelon",
        "Tangerine", "Ugli Fruit", "Juniperberry", "Kiwi", "Lemon",
        "Nectarine", "Plum", "Raspberry", "Strawberry", "Orange"]
    app = QApplication(sys.argv)
    form = StringListDlg("Fruit", fruit)
    form.exec_()
    print("\n".join([unicode(x) for x in form.stringlist]))

