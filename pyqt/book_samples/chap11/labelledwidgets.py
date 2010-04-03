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
from PyQt4.QtCore import (QString, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QBoxLayout, QDialog,
        QDialogButtonBox, QGridLayout, QLabel, QLineEdit, QTextEdit,
        QVBoxLayout, QWidget)

LEFT, ABOVE = range(2)

class LabelledLineEdit(QWidget):

    def __init__(self, labelText=QString(), position=LEFT,
                 parent=None):
        super(LabelledLineEdit, self).__init__(parent)
        self.label = QLabel(labelText)
        self.lineEdit = QLineEdit()
        self.label.setBuddy(self.lineEdit)
        layout = QBoxLayout(QBoxLayout.LeftToRight
                if position == LEFT else QBoxLayout.TopToBottom)
        layout.addWidget(self.label)
        layout.addWidget(self.lineEdit)
        self.setLayout(layout)


class LabelledTextEdit(QWidget):

    def __init__(self, labelText=QString(), position=LEFT,
                 parent=None):
        super(LabelledTextEdit, self).__init__(parent)
        self.label = QLabel(labelText)
        self.textEdit = QTextEdit()
        self.label.setBuddy(self.textEdit)
        layout = QBoxLayout(QBoxLayout.LeftToRight
                if position == LEFT else QBoxLayout.TopToBottom)
        layout.addWidget(self.label)
        layout.addWidget(self.textEdit)
        self.setLayout(layout)


class Dialog(QDialog):

    def __init__(self, address=None, parent=None):
        super(Dialog, self).__init__(parent)

        self.street = LabelledLineEdit("&Street:")
        self.city = LabelledLineEdit("&City:")
        self.state = LabelledLineEdit("St&ate:")
        self.zipcode = LabelledLineEdit("&Zipcode:")
        self.notes = LabelledTextEdit("&Notes:", ABOVE)
        if address is not None:
            self.street.lineEdit.setText(address.get("street", QString()))
            self.city.lineEdit.setText(address.get("city", QString()))
            self.state.lineEdit.setText(address.get("state", QString()))
            self.zipcode.lineEdit.setText(address.get("zipcode",
                                          QString()))
            self.notes.textEdit.setPlainText(address.get("notes",
                                             QString()))
        buttonBox = QDialogButtonBox(QDialogButtonBox.Ok|
                                     QDialogButtonBox.Cancel)

        grid = QGridLayout()
        grid.addWidget(self.street, 0, 0)
        grid.addWidget(self.city, 0, 1)
        grid.addWidget(self.state, 1, 0)
        grid.addWidget(self.zipcode, 1, 1)
        grid.addWidget(self.notes, 2, 0, 1, 2)
        layout = QVBoxLayout()
        layout.addLayout(grid)
        layout.addWidget(buttonBox)
        self.setLayout(layout)
        
        self.connect(buttonBox, SIGNAL("accepted()"), self.accept)
        self.connect(buttonBox, SIGNAL("rejected()"), self.reject)

        self.setWindowTitle("Labelled Widgets")


if __name__ == "__main__":
    fakeAddress = dict(street="3200 Mount Vernon Memorial Highway",
                       city="Mount Vernon", state="Virginia",
                       zipcode="22121")
    app = QApplication(sys.argv)
    form = Dialog(fakeAddress)
    form.show()
    app.exec_()
    print("Street:", unicode(form.street.lineEdit.text()))
    print("City:", unicode(form.city.lineEdit.text()))
    print("State:", unicode(form.state.lineEdit.text()))
    print("Notes:")
    print(unicode(form.notes.textEdit.toPlainText()))

