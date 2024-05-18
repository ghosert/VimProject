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

import functools
import sys
from PyQt4.QtCore import (Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QDialog, QHBoxLayout, QLabel,
        QPushButton)


class Form(QDialog):

    def __init__(self, parent=None):
        super(Form, self).__init__(parent)

        button1 = QPushButton("One")
        button2 = QPushButton("Two")
        button3 = QPushButton("Three")
        button4 = QPushButton("Four")
        button5 = QPushButton("Five")
        self.label = QLabel("Click a button...")

        layout = QHBoxLayout()
        layout.addWidget(button1)
        layout.addWidget(button2)
        layout.addWidget(button3)
        layout.addWidget(button4)
        layout.addWidget(button5)
        layout.addStretch()
        layout.addWidget(self.label)
        self.setLayout(layout)

        self.connect(button1, SIGNAL("clicked()"), self.one)
        self.button2callback = functools.partial(self.anyButton, "Two")
        self.connect(button2, SIGNAL("clicked()"),
                     self.button2callback)
        self.button3callback = lambda who="Three": self.anyButton(who)
        self.connect(button3, SIGNAL("clicked()"),
                     self.button3callback)
        self.connect(button4, SIGNAL("clicked()"), self.clicked)
        self.connect(button5, SIGNAL("clicked()"), self.clicked)

        self.setWindowTitle("Connections")


    def one(self):
        self.label.setText("You clicked button 'One'")


    def anyButton(self, who):
        self.label.setText("You clicked button '{0}'".format(who))


    def clicked(self):
        button = self.sender()
        if button is None or not isinstance(button, QPushButton):
            return
        self.label.setText("You clicked button '{0}'".format(
                           button.text()))


app = QApplication(sys.argv)
form = Form()
form.show()
app.exec_()

