# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'E:\VimProject\pyqt\samples\chap17\newimagedlg.ui'
#
# Created: Tue Jan 13 19:18:16 2009
#      by: PyQt4 UI code generator 4.4.4
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_NewImageDlg(object):
    def setupUi(self, NewImageDlg):
        NewImageDlg.setObjectName("NewImageDlg")
        NewImageDlg.resize(287, 214)
        self.gridlayout = QtGui.QGridLayout(NewImageDlg)
        self.gridlayout.setMargin(9)
        self.gridlayout.setSpacing(6)
        self.gridlayout.setObjectName("gridlayout")
        self.buttonBox = QtGui.QDialogButtonBox(NewImageDlg)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.NoButton|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.gridlayout.addWidget(self.buttonBox, 5, 1, 1, 2)
        spacerItem = QtGui.QSpacerItem(269, 16, QtGui.QSizePolicy.Minimum, QtGui.QSizePolicy.Expanding)
        self.gridlayout.addItem(spacerItem, 4, 0, 1, 3)
        self.colorLabel = QtGui.QLabel(NewImageDlg)
        self.colorLabel.setFrameShape(QtGui.QFrame.StyledPanel)
        self.colorLabel.setFrameShadow(QtGui.QFrame.Raised)
        self.colorLabel.setScaledContents(True)
        self.colorLabel.setObjectName("colorLabel")
        self.gridlayout.addWidget(self.colorLabel, 3, 1, 1, 1)
        self.label_3 = QtGui.QLabel(NewImageDlg)
        self.label_3.setObjectName("label_3")
        self.gridlayout.addWidget(self.label_3, 3, 0, 1, 1)
        self.colorButton = QtGui.QPushButton(NewImageDlg)
        self.colorButton.setObjectName("colorButton")
        self.gridlayout.addWidget(self.colorButton, 3, 2, 1, 1)
        self.brushComboBox = QtGui.QComboBox(NewImageDlg)
        self.brushComboBox.setObjectName("brushComboBox")
        self.gridlayout.addWidget(self.brushComboBox, 2, 1, 1, 2)
        self.label_4 = QtGui.QLabel(NewImageDlg)
        self.label_4.setObjectName("label_4")
        self.gridlayout.addWidget(self.label_4, 2, 0, 1, 1)
        self.label = QtGui.QLabel(NewImageDlg)
        self.label.setObjectName("label")
        self.gridlayout.addWidget(self.label, 0, 0, 1, 1)
        self.label_2 = QtGui.QLabel(NewImageDlg)
        self.label_2.setObjectName("label_2")
        self.gridlayout.addWidget(self.label_2, 1, 0, 1, 1)
        self.heightSpinBox = QtGui.QSpinBox(NewImageDlg)
        self.heightSpinBox.setAlignment(QtCore.Qt.AlignRight)
        self.heightSpinBox.setMaximum(512)
        self.heightSpinBox.setMinimum(8)
        self.heightSpinBox.setSingleStep(4)
        self.heightSpinBox.setProperty("value", QtCore.QVariant(64))
        self.heightSpinBox.setObjectName("heightSpinBox")
        self.gridlayout.addWidget(self.heightSpinBox, 1, 1, 1, 1)
        self.widthSpinBox = QtGui.QSpinBox(NewImageDlg)
        self.widthSpinBox.setAlignment(QtCore.Qt.AlignRight)
        self.widthSpinBox.setMaximum(512)
        self.widthSpinBox.setMinimum(8)
        self.widthSpinBox.setSingleStep(4)
        self.widthSpinBox.setProperty("value", QtCore.QVariant(64))
        self.widthSpinBox.setObjectName("widthSpinBox")
        self.gridlayout.addWidget(self.widthSpinBox, 0, 1, 1, 1)
        self.label_4.setBuddy(self.brushComboBox)
        self.label.setBuddy(self.widthSpinBox)
        self.label_2.setBuddy(self.heightSpinBox)

        self.retranslateUi(NewImageDlg)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("accepted()"), NewImageDlg.accept)
        QtCore.QObject.connect(self.buttonBox, QtCore.SIGNAL("rejected()"), NewImageDlg.reject)
        QtCore.QMetaObject.connectSlotsByName(NewImageDlg)
        NewImageDlg.setTabOrder(self.widthSpinBox, self.heightSpinBox)
        NewImageDlg.setTabOrder(self.heightSpinBox, self.brushComboBox)
        NewImageDlg.setTabOrder(self.brushComboBox, self.colorButton)
        NewImageDlg.setTabOrder(self.colorButton, self.buttonBox)

    def retranslateUi(self, NewImageDlg):
        NewImageDlg.setWindowTitle(QtGui.QApplication.translate("NewImageDlg", "Image Chooser - New Image", None, QtGui.QApplication.UnicodeUTF8))
        self.label_3.setText(QtGui.QApplication.translate("NewImageDlg", "Color", None, QtGui.QApplication.UnicodeUTF8))
        self.colorButton.setText(QtGui.QApplication.translate("NewImageDlg", "&Color...", None, QtGui.QApplication.UnicodeUTF8))
        self.label_4.setText(QtGui.QApplication.translate("NewImageDlg", "&Brush pattern:", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("NewImageDlg", "&Width:", None, QtGui.QApplication.UnicodeUTF8))
        self.label_2.setText(QtGui.QApplication.translate("NewImageDlg", "&Height:", None, QtGui.QApplication.UnicodeUTF8))
        self.heightSpinBox.setSuffix(QtGui.QApplication.translate("NewImageDlg", " px", None, QtGui.QApplication.UnicodeUTF8))
        self.widthSpinBox.setSuffix(QtGui.QApplication.translate("NewImageDlg", " px", None, QtGui.QApplication.UnicodeUTF8))

