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
from PyQt4.QtCore import (Qt, SIGNAL, SLOT)
from PyQt4.QtGui import (QApplication, QCheckBox, QDialog, QFrame,
        QGridLayout, QHBoxLayout, QLabel, QLayout, QLineEdit,
        QPushButton, QVBoxLayout)

MAC = True
try:
    from PyQt4.QtGui import qt_mac_set_native_menubar
except ImportError:
    MAC = False


class FindAndReplaceDlg(QDialog):

    def __init__(self, parent=None):
        super(FindAndReplaceDlg, self).__init__(parent)

        findLabel = QLabel("Find &what:")
        self.findLineEdit = QLineEdit()
        findLabel.setBuddy(self.findLineEdit)
        replaceLabel = QLabel("Replace w&ith:")
        self.replaceLineEdit = QLineEdit()
        replaceLabel.setBuddy(self.replaceLineEdit)
        self.caseCheckBox = QCheckBox("&Case sensitive")
        self.wholeCheckBox = QCheckBox("Wh&ole words")
        self.wholeCheckBox.setChecked(True)
        moreFrame = QFrame()
        moreFrame.setFrameStyle(QFrame.StyledPanel|QFrame.Sunken)
        self.backwardsCheckBox = QCheckBox("Search &Backwards")
        self.regexCheckBox = QCheckBox("Regular E&xpression")
        self.ignoreNotesCheckBox = QCheckBox("Ignore foot&notes "
                                                   "and endnotes")
        line = QFrame()
        line.setFrameStyle(QFrame.VLine|QFrame.Sunken)
        self.findButton = QPushButton("&Find")
        self.replaceButton = QPushButton("&Replace")
        closeButton = QPushButton("Close")
        moreButton = QPushButton("&More")
        moreButton.setCheckable(True)
        if not MAC:
            self.findButton.setFocusPolicy(Qt.NoFocus)
            self.replaceButton.setFocusPolicy(Qt.NoFocus)
            closeButton.setFocusPolicy(Qt.NoFocus)
            moreButton.setFocusPolicy(Qt.NoFocus)

        gridLayout = QGridLayout()
        gridLayout.addWidget(findLabel, 0, 0)
        gridLayout.addWidget(self.findLineEdit, 0, 1)
        gridLayout.addWidget(replaceLabel, 1, 0)
        gridLayout.addWidget(self.replaceLineEdit, 1, 1)
        frameLayout = QVBoxLayout()
        frameLayout.addWidget(self.backwardsCheckBox)
        frameLayout.addWidget(self.regexCheckBox)
        frameLayout.addWidget(self.ignoreNotesCheckBox)
        moreFrame.setLayout(frameLayout)
        leftLayout = QVBoxLayout()
        leftLayout.addLayout(gridLayout)
        leftLayout.addWidget(self.caseCheckBox)
        leftLayout.addWidget(self.wholeCheckBox)
        leftLayout.addWidget(moreFrame)
        buttonLayout = QVBoxLayout()
        buttonLayout.addWidget(self.findButton)
        buttonLayout.addWidget(self.replaceButton)
        buttonLayout.addWidget(closeButton)
        buttonLayout.addWidget(moreButton)
        buttonLayout.addStretch()
        mainLayout = QHBoxLayout()
        mainLayout.addLayout(leftLayout)
        mainLayout.addWidget(line)
        mainLayout.addLayout(buttonLayout)
        self.setLayout(mainLayout)

        moreFrame.hide()
        mainLayout.setSizeConstraint(QLayout.SetFixedSize)

        self.connect(moreButton, SIGNAL("toggled(bool)"),
                     moreFrame, SLOT("setVisible(bool)"))
        self.connect(self.findLineEdit,
                SIGNAL("textEdited(QString)"), self.updateUi)
        self.connect(self.findButton, SIGNAL("clicked()"),
                     self.findClicked)
        self.connect(self.replaceButton, SIGNAL("clicked()"),
                     self.replaceClicked)

        self.updateUi()
        self.setWindowTitle("Find and Replace")


    def findClicked(self):
        self.emit(SIGNAL("find"), self.findLineEdit.text(),
                self.caseCheckBox.isChecked(),
                self.wholeCheckBox.isChecked(),
                self.backwardsCheckBox.isChecked(),
                self.regexCheckBox.isChecked(),
                self.ignoreNotesCheckBox.isChecked())
        
        
    def replaceClicked(self):
        self.emit(SIGNAL("replace"), self.findLineEdit.text(),
                self.replaceLineEdit.text(),
                self.caseCheckBox.isChecked(),
                self.wholeCheckBox.isChecked(),
                self.backwardsCheckBox.isChecked(),
                self.regexCheckBox.isChecked(),
                self.ignoreNotesCheckBox.isChecked())
        

    def updateUi(self):
        enable = not self.findLineEdit.text().isEmpty()
        self.findButton.setEnabled(enable)
        self.replaceButton.setEnabled(enable)


if __name__ == "__main__":

    def find(what, *args):
        print("Find {0} {1}".format(what, [x for x in args]))

    def replace(old, new, *args):
        print("Replace {0} with {1} {2}".format(
              old, new, [x for x in args]))

    app = QApplication(sys.argv)
    form = FindAndReplaceDlg()
    form.connect(form, SIGNAL("find"), find)
    form.connect(form, SIGNAL("replace"), replace)
    form.show()
    app.exec_()

