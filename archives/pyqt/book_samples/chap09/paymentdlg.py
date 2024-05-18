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
from PyQt4.QtCore import (QDate, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QDialog, QDialogButtonBox)
import ui_paymentdlg


class PaymentDlg(QDialog, ui_paymentdlg.Ui_PaymentDlg):

    def __init__(self, parent=None):
        super(PaymentDlg, self).__init__(parent)
        self.setupUi(self)
        for lineEdit in (self.forenameLineEdit, self.surnameLineEdit,
                self.checkNumLineEdit, self.accountNumLineEdit,
                self.bankLineEdit, self.sortCodeLineEdit,
                self.creditCardLineEdit):
            self.connect(lineEdit, SIGNAL("textEdited(QString)"),
                         self.updateUi)
        for dateEdit in (self.validFromDateEdit, self.expiryDateEdit):
            self.connect(dateEdit, SIGNAL("dateChanged(QDate)"),
                         self.updateUi)
        self.connect(self.paidCheckBox, SIGNAL("clicked()"),
                     self.updateUi)
        self.updateUi()
        self.forenameLineEdit.setFocus()


    def updateUi(self):
        today = QDate.currentDate()
        enable = (not (self.forenameLineEdit.text().isEmpty() or
                       self.surnameLineEdit.text().isEmpty()))
        if enable:
            enable = (self.paidCheckBox.isChecked() or
                  (not (self.checkNumLineEdit.text().isEmpty() or
                        self.accountNumLineEdit.text().isEmpty() or
                        self.bankLineEdit.text().isEmpty() or
                        self.sortCodeLineEdit.text().isEmpty())) or
                  (not self.creditCardLineEdit.text().isEmpty() and
                   self.validFromDateEdit.date() <= today and
                   self.expiryDateEdit.date() >= today))
        self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(enable)


if __name__ == "__main__":
    app = QApplication(sys.argv)
    form = PaymentDlg()
    form.show()
    app.exec_()

