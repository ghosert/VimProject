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
from PyQt4.QtCore import (QVariant, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QComboBox, QDialog,
        QDialogButtonBox, QGridLayout, QLabel, QLineEdit, QVBoxLayout)


class ContactDlg(QDialog):

    StyleSheet = """
QComboBox { color: darkblue; }
QLineEdit { color: darkgreen; }
QLineEdit[mandatory="true"] {
    background-color: rgb(255, 255, 127);
    color: darkblue;
}
"""

    def __init__(self, parent=None):
        super(ContactDlg, self).__init__(parent)

        forenameLabel = QLabel("&Forename:")
        self.forenameEdit = QLineEdit()
        forenameLabel.setBuddy(self.forenameEdit)
        surnameLabel = QLabel("&Surname:")
        self.surnameEdit = QLineEdit()
        surnameLabel.setBuddy(self.surnameEdit)
        categoryLabel = QLabel("&Category:")
        self.categoryComboBox = QComboBox()
        categoryLabel.setBuddy(self.categoryComboBox)
        self.categoryComboBox.addItems(["Business", "Domestic",
                                        "Personal"])
        companyLabel = QLabel("C&ompany:")
        self.companyEdit = QLineEdit()
        companyLabel.setBuddy(self.companyEdit)
        addressLabel = QLabel("A&ddress:")
        self.addressEdit = QLineEdit()
        addressLabel.setBuddy(self.addressEdit)
        phoneLabel = QLabel("&Phone:")
        self.phoneEdit = QLineEdit()
        phoneLabel.setBuddy(self.phoneEdit)
        mobileLabel = QLabel("&Mobile:")
        self.mobileEdit = QLineEdit()
        mobileLabel.setBuddy(self.mobileEdit)
        faxLabel = QLabel("Fa&x:")
        self.faxEdit = QLineEdit()
        faxLabel.setBuddy(self.faxEdit)
        emailLabel = QLabel("&Email:")
        self.emailEdit = QLineEdit()
        emailLabel.setBuddy(self.emailEdit)
        self.buttonBox = QDialogButtonBox(QDialogButtonBox.Ok|
                                          QDialogButtonBox.Cancel)
        addButton = self.buttonBox.button(QDialogButtonBox.Ok)
        addButton.setText("&Add")
        addButton.setEnabled(False)

        grid = QGridLayout()
        grid.addWidget(forenameLabel, 0, 0)
        grid.addWidget(self.forenameEdit, 0, 1)
        grid.addWidget(surnameLabel, 0, 2)
        grid.addWidget(self.surnameEdit, 0, 3)
        grid.addWidget(categoryLabel, 1, 0)
        grid.addWidget(self.categoryComboBox, 1, 1)
        grid.addWidget(companyLabel, 1, 2)
        grid.addWidget(self.companyEdit, 1, 3)
        grid.addWidget(addressLabel, 2, 0)
        grid.addWidget(self.addressEdit, 2, 1, 1, 3)
        grid.addWidget(phoneLabel, 3, 0)
        grid.addWidget(self.phoneEdit, 3, 1)
        grid.addWidget(mobileLabel, 3, 2)
        grid.addWidget(self.mobileEdit, 3, 3)
        grid.addWidget(faxLabel, 4, 0)
        grid.addWidget(self.faxEdit, 4, 1)
        grid.addWidget(emailLabel, 4, 2)
        grid.addWidget(self.emailEdit, 4, 3)
        layout = QVBoxLayout()
        layout.addLayout(grid)
        layout.addWidget(self.buttonBox)
        self.setLayout(layout)

        self.lineedits = (self.forenameEdit, self.surnameEdit,
                self.companyEdit, self.phoneEdit, self.emailEdit)
        for lineEdit in self.lineedits:
            lineEdit.setProperty("mandatory", QVariant(True))
            self.connect(lineEdit, SIGNAL("textEdited(QString)"),
                         self.updateUi)
        self.connect(self.categoryComboBox, SIGNAL("activated(int)"),
                     self.updateUi)

        self.connect(self.buttonBox, SIGNAL("accepted()"), self.accept)
        self.connect(self.buttonBox, SIGNAL("rejected()"), self.reject)

        self.setStyleSheet(ContactDlg.StyleSheet)
        self.setWindowTitle("Add Contact")


    def updateUi(self):
        mandatory = self.companyEdit.property("mandatory").toBool()
        if self.categoryComboBox.currentText() == "Business":
            if not mandatory:
                self.companyEdit.setProperty("mandatory", QVariant(True))
        elif mandatory:
            self.companyEdit.setProperty("mandatory", QVariant(False))
        if (mandatory !=
            self.companyEdit.property("mandatory").toBool()):
            self.setStyleSheet(ContactDlg.StyleSheet)
        enable = True
        for lineEdit in self.lineedits:
            if (lineEdit.property("mandatory").toBool() and
                lineEdit.text().isEmpty()):
                enable = False
                break
        self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(enable)


if __name__ == "__main__":
    app = QApplication(sys.argv)
    form = ContactDlg()
    form.show()
    app.exec_()
