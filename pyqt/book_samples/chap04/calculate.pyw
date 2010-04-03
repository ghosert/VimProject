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
from math import *
from PyQt4.QtCore import (Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QDialog, QLineEdit, QTextBrowser,
        QVBoxLayout)


class Form(QDialog):

    def __init__(self, parent=None):
        super(Form, self).__init__(parent)
        # For the any subclass of QWidget(QObject), you have three options:
        # 1. No parent widget means the current widget is a top-level window. In this case, it is the 'Form' widget here.
        # 2. Give the widget a parent widget when you initialize it.
        # 3. Or you don't need pass-in a parent widget argument but you must add it to a layout manager, In this case, 'QTextBrowser' and 'QLineEdit'
        #    are added into QVBoxLayout which is set to be a layout for 'Form' widget.
        self.browser = QTextBrowser()
        self.lineedit = QLineEdit("Type an expression and press Enter")
        self.lineedit.selectAll()
        layout = QVBoxLayout()
        layout.addWidget(self.browser)
        layout.addWidget(self.lineedit)
        self.setLayout(layout)
        self.lineedit.setFocus()
        self.connect(self.lineedit, SIGNAL("returnPressed()"),
                     self.updateUi)
        self.setWindowTitle("Calculate")


    def updateUi(self):
        try:
            text = unicode(self.lineedit.text())
            self.browser.append("{0} = <b>{1}</b>".format(text,
                                eval(text)))
        except:
            self.browser.append("<font color=red>{0} is invalid!</font>"
                                .format(text))


app = QApplication(sys.argv)
form = Form()
form.show()
app.exec_()

