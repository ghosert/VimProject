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
from PyQt4.QtCore import (Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QComboBox, QDialog,
        QDialogButtonBox, QFrame, QGridLayout, QHBoxLayout, QLabel,
        QSpinBox, QStackedWidget, QVBoxLayout, QWidget)


class VehicleRentalDlg(QDialog):

    def __init__(self, parent=None):
        super(VehicleRentalDlg, self).__init__(parent)

        vehicleLabel = QLabel("&Vehicle Type:")
        self.vehicleComboBox = QComboBox()
        vehicleLabel.setBuddy(self.vehicleComboBox)
        self.vehicleComboBox.addItems(["Car", "Van"])
        colorLabel = QLabel("&Color:")
        self.colorComboBox = QComboBox()
        colorLabel.setBuddy(self.colorComboBox)
        self.colorComboBox.addItems(["Black", "Blue", "Green", "Red",
                                     "Silver", "White", "Yellow"])
        seatsLabel = QLabel("&Seats:")
        self.seatsSpinBox = QSpinBox()
        seatsLabel.setBuddy(self.seatsSpinBox)
        self.seatsSpinBox.setRange(2, 12)
        self.seatsSpinBox.setValue(4)
        self.seatsSpinBox.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
        weightLabel = QLabel("&Weight:")
        self.weightSpinBox = QSpinBox()
        weightLabel.setBuddy(self.weightSpinBox)
        self.weightSpinBox.setRange(1, 8)
        self.weightSpinBox.setValue(1)
        self.weightSpinBox.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
        self.weightSpinBox.setSuffix(" tons")
        volumeLabel = QLabel("V&olume")
        self.volumeSpinBox = QSpinBox()
        volumeLabel.setBuddy(self.volumeSpinBox)
        self.volumeSpinBox.setRange(4, 22)
        self.volumeSpinBox.setValue(10)
        self.volumeSpinBox.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
        self.volumeSpinBox.setSuffix(" cu m")
        mileageLabel = QLabel("Max. Mileage")
        self.mileageLabel = QLabel("1000 miles")
        self.mileageLabel.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
        self.mileageLabel.setFrameStyle(QFrame.StyledPanel|QFrame.Sunken)
        self.buttonBox = QDialogButtonBox(QDialogButtonBox.Ok|
                                          QDialogButtonBox.Cancel)

        self.stackedWidget = QStackedWidget()
        carWidget = QWidget()
        carLayout = QGridLayout()
        carLayout.addWidget(colorLabel, 0, 0)
        carLayout.addWidget(self.colorComboBox, 0, 1)
        carLayout.addWidget(seatsLabel, 1, 0)
        carLayout.addWidget(self.seatsSpinBox, 1, 1)
        carWidget.setLayout(carLayout)
        self.stackedWidget.addWidget(carWidget)
        vanWidget = QWidget()
        vanLayout = QGridLayout()
        vanLayout.addWidget(weightLabel, 0, 0)
        vanLayout.addWidget(self.weightSpinBox, 0, 1)
        vanLayout.addWidget(volumeLabel, 1, 0)
        vanLayout.addWidget(self.volumeSpinBox, 1, 1)
        vanWidget.setLayout(vanLayout)
        self.stackedWidget.addWidget(vanWidget)

        topLayout = QHBoxLayout()
        topLayout.addWidget(vehicleLabel)
        topLayout.addWidget(self.vehicleComboBox)
        bottomLayout = QHBoxLayout()
        bottomLayout.addWidget(mileageLabel)
        bottomLayout.addWidget(self.mileageLabel)
        layout = QVBoxLayout()
        layout.addLayout(topLayout)
        layout.addWidget(self.stackedWidget)
        layout.addLayout(bottomLayout)
        layout.addWidget(self.buttonBox)
        self.setLayout(layout)

        self.connect(self.buttonBox, SIGNAL("accepted()"), self.accept)
        self.connect(self.buttonBox, SIGNAL("rejected()"), self.reject)
        self.connect(self.vehicleComboBox,
                     SIGNAL("currentIndexChanged(QString)"),
                     self.setWidgetStack)
        self.connect(self.weightSpinBox, SIGNAL("valueChanged(int)"),
                     self.weightChanged)

        self.setWindowTitle("Vehicle Rental")


    def setWidgetStack(self, text):
        if text == "Car":
            self.stackedWidget.setCurrentIndex(0)
            self.mileageLabel.setText("1000 miles")
        else:
            self.stackedWidget.setCurrentIndex(1)
            self.weightChanged(self.weightSpinBox.value())


    def weightChanged(self, amount):
        self.mileageLabel.setText("{0} miles".format(8000 / amount))


app = QApplication(sys.argv)
form = VehicleRentalDlg()
form.show()
app.exec_()

