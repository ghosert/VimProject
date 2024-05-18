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
from PyQt4.QtCore import (QByteArray, QDataStream, QDate, QIODevice,
        QRegExp, QString, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QDateEdit, QFrame, QGridLayout,
        QHBoxLayout, QLabel, QLineEdit, QPushButton, QRegExpValidator,
        QWidget)
from PyQt4.QtNetwork import (QTcpSocket,)

MAC = True
try:
    from PyQt4.QtGui import qt_mac_set_native_menubar
except ImportError:
    MAC = False

PORT = 9407
SIZEOF_UINT16 = 2


class BuildingServicesClient(QWidget):

    def __init__(self, parent=None):
        super(BuildingServicesClient, self).__init__(parent)

        self.socket = QTcpSocket()
        self.nextBlockSize = 0
        self.request = None

        roomLabel = QLabel("&Room")
        self.roomEdit = QLineEdit()
        roomLabel.setBuddy(self.roomEdit)
        regex = QRegExp(r"[0-9](?:0[1-9]|[12][0-9]|3[0-4])")
        self.roomEdit.setValidator(QRegExpValidator(regex, self))
        self.roomEdit.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
        dateLabel = QLabel("&Date")
        self.dateEdit = QDateEdit()
        dateLabel.setBuddy(self.dateEdit)
        self.dateEdit.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
        self.dateEdit.setDate(QDate.currentDate().addDays(1))
        self.dateEdit.setDisplayFormat("yyyy-MM-dd")
        responseLabel = QLabel("Response")
        self.responseLabel = QLabel()
        self.responseLabel.setFrameStyle(QFrame.StyledPanel|QFrame.Sunken)

        self.bookButton = QPushButton("&Book")
        self.bookButton.setEnabled(False)
        self.unBookButton = QPushButton("&Unbook")
        self.unBookButton.setEnabled(False)
        quitButton = QPushButton("&Quit")
        if not MAC:
            self.bookButton.setFocusPolicy(Qt.NoFocus)
            self.unBookButton.setFocusPolicy(Qt.NoFocus)

        buttonLayout = QHBoxLayout()
        buttonLayout.addWidget(self.bookButton)
        buttonLayout.addWidget(self.unBookButton)
        buttonLayout.addStretch()
        buttonLayout.addWidget(quitButton)
        layout = QGridLayout()
        layout.addWidget(roomLabel, 0, 0)
        layout.addWidget(self.roomEdit, 0, 1)
        layout.addWidget(dateLabel, 0, 2)
        layout.addWidget(self.dateEdit, 0, 3)
        layout.addWidget(responseLabel, 1, 0)
        layout.addWidget(self.responseLabel, 1, 1, 1, 3)
        layout.addLayout(buttonLayout, 2, 1, 1, 4)
        self.setLayout(layout)

        self.connect(self.socket, SIGNAL("connected()"), self.sendRequest)
        self.connect(self.socket, SIGNAL("readyRead()"), self.readResponse)
        self.connect(self.socket, SIGNAL("disconnected()"),
                     self.serverHasStopped)
        self.connect(self.socket,
                     SIGNAL("error(QAbstractSocket::SocketError)"),
                     self.serverHasError)
        self.connect(self.roomEdit, SIGNAL("textEdited(QString)"),
                     self.updateUi)
        self.connect(self.dateEdit, SIGNAL("dateChanged(QDate)"),
                     self.updateUi)
        self.connect(self.bookButton, SIGNAL("clicked()"), self.book)
        self.connect(self.unBookButton, SIGNAL("clicked()"), self.unBook)
        self.connect(quitButton, SIGNAL("clicked()"), self.close)

        self.setWindowTitle("Building Services")


    def updateUi(self):
        enabled = False
        if (not self.roomEdit.text().isEmpty() and
            self.dateEdit.date() > QDate.currentDate()):
            enabled = True
        if self.request is not None:
            enabled = False
        self.bookButton.setEnabled(enabled)
        self.unBookButton.setEnabled(enabled)


    def closeEvent(self, event):
        self.socket.close()
        event.accept()


    def book(self):
        self.issueRequest(QString("BOOK"), self.roomEdit.text(),
                          self.dateEdit.date())


    def unBook(self):
        self.issueRequest(QString("UNBOOK"), self.roomEdit.text(),
                          self.dateEdit.date())


    def issueRequest(self, action, room, date):
        self.request = QByteArray()
        stream = QDataStream(self.request, QIODevice.WriteOnly)
        stream.setVersion(QDataStream.Qt_4_2)
        stream.writeUInt16(0)
        stream << action << room << date
        stream.device().seek(0)
        stream.writeUInt16(self.request.size() - SIZEOF_UINT16)
        self.updateUi()
        if self.socket.isOpen():
            self.socket.close()
        self.responseLabel.setText("Connecting to server...")
        self.socket.connectToHost("localhost", PORT)


    def sendRequest(self):
        self.responseLabel.setText("Sending request...")
        self.nextBlockSize = 0
        self.socket.write(self.request)
        self.request = None
        

    def readResponse(self):
        stream = QDataStream(self.socket)
        stream.setVersion(QDataStream.Qt_4_2)

        while True:
            if self.nextBlockSize == 0:
                if self.socket.bytesAvailable() < SIZEOF_UINT16:
                    break
                self.nextBlockSize = stream.readUInt16()
            if self.socket.bytesAvailable() < self.nextBlockSize:
                break
            action = QString()
            room = QString()
            date = QDate()
            stream >> action >> room
            if action != "ERROR":
                stream >> date
            if action == "ERROR":
                msg = QString("Error: %1").arg(room)
            elif action == "BOOK":
                msg = (QString("Booked room %1 for %2").arg(room)
                        .arg(date.toString(Qt.ISODate)))
            elif action == "UNBOOK":
                msg = (QString("Unbooked room %1 for %2").arg(room)
                        .arg(date.toString(Qt.ISODate)))
            self.responseLabel.setText(msg)
            self.updateUi()
            self.nextBlockSize = 0


    def serverHasStopped(self):
        self.responseLabel.setText(
                "Error: Connection closed by server")
        self.socket.close()


    def serverHasError(self, error):
        self.responseLabel.setText(QString("Error: %1")
                .arg(self.socket.errorString()))
        self.socket.close()


app = QApplication(sys.argv)
form = BuildingServicesClient()
form.show()
app.exec_()

