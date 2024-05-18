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

import bisect
import collections
import sys
from PyQt4.QtCore import (QByteArray, QDataStream, QDate, QIODevice,
        QString, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QMessageBox, QPushButton)
from PyQt4.QtNetwork import (QHostAddress, QTcpServer, QTcpSocket)

PORT = 9407
SIZEOF_UINT16 = 2
MAX_BOOKINGS_PER_DAY = 5

# Key = date, value = list of room IDs
Bookings = collections.defaultdict(list)

def printBookings():
    for key in sorted(Bookings):
        print(key, Bookings[key])
    print()


class Socket(QTcpSocket):

    def __init__(self, parent=None):
        super(Socket, self).__init__(parent)
        self.connect(self, SIGNAL("readyRead()"), self.readRequest)
        self.connect(self, SIGNAL("disconnected()"), self.deleteLater)
        self.nextBlockSize = 0


    def readRequest(self):
        stream = QDataStream(self)
        stream.setVersion(QDataStream.Qt_4_2)

        if self.nextBlockSize == 0:
            if self.bytesAvailable() < SIZEOF_UINT16:
                return
            self.nextBlockSize = stream.readUInt16()
        if self.bytesAvailable() < self.nextBlockSize:
            return

        action = QString()
        room = QString()
        date = QDate()
        stream >> action
        if action in ("BOOK", "UNBOOK", "BOOKINGSONDATE",
                      "BOOKINGSFORROOM"):
            stream >> room >> date
            bookings = Bookings.get(date.toPyDate())
            uroom = unicode(room)
        if action == "BOOK":
            if bookings is None:
                bookings = Bookings[date.toPyDate()]
            if len(bookings) < MAX_BOOKINGS_PER_DAY:
                if uroom in bookings:
                    self.sendError("Cannot accept duplicate booking")
                else:
                    bisect.insort(bookings, uroom)
                    self.sendReply(action, room, date)
            else:
                self.sendError(QString("%1 is fully booked")
                               .arg(date.toString(Qt.ISODate)))
        elif action == "UNBOOK":
            if bookings is None or uroom not in bookings:
                self.sendError("Cannot unbook nonexistent booking")
            else:
                bookings.remove(uroom)
                self.sendReply(action, room, date)
        elif action == "BOOKINGSONDATE":
            bookings = Bookings.get(date.toPyDate())
            if bookings is not None:
                self.sendReply(action, QString(", ".join(bookings)), date)
            else:
                self.sendError(QString(
                        "there are no rooms booked on %1").arg(
                        date.toString(Qt.ISODate)))
        elif action == "BOOKINGSFORROOM":
            dates = []
            for date, bookings in Bookings.items():
                if room in bookings:
                    dates.append(date)
            if dates:
                dates.sort()
                reply = QByteArray()
                stream = QDataStream(reply, QIODevice.WriteOnly)
                stream.setVersion(QDataStream.Qt_4_2)
                stream.writeUInt16(0)
                stream << action << room
                stream.writeInt32(len(dates))
                for date in dates:
                    stream << QDate(date)
                stream.device().seek(0)
                stream.writeUInt16(reply.size() - SIZEOF_UINT16)
                self.write(reply)
            else:
                self.sendError(QString(
                        "room %1 is not booked").arg(room))
        else:
            self.sendError("Unrecognized request")
        printBookings()


    def sendError(self, msg):
        reply = QByteArray()
        stream = QDataStream(reply, QIODevice.WriteOnly)
        stream.setVersion(QDataStream.Qt_4_2)
        stream.writeUInt16(0)
        stream << QString("ERROR") << QString(msg)
        stream.device().seek(0)
        stream.writeUInt16(reply.size() - SIZEOF_UINT16)
        self.write(reply)


    def sendReply(self, action, room, date):
        reply = QByteArray()
        stream = QDataStream(reply, QIODevice.WriteOnly)
        stream.setVersion(QDataStream.Qt_4_2)
        stream.writeUInt16(0)
        stream << action << room << date
        stream.device().seek(0)
        stream.writeUInt16(reply.size() - SIZEOF_UINT16)
        self.write(reply)


class TcpServer(QTcpServer):

    def __init__(self, parent=None):
        super(TcpServer, self).__init__(parent)


    def incomingConnection(self, socketId):
        socket = Socket(self)
        socket.setSocketDescriptor(socketId)
        

class BuildingServicesDlg(QPushButton):

    def __init__(self, parent=None):
        super(BuildingServicesDlg, self).__init__(
                "&Close Server", parent)
        self.setWindowFlags(Qt.WindowStaysOnTopHint)

        self.loadBookings()
        self.tcpServer = TcpServer(self)
        if not self.tcpServer.listen(QHostAddress("0.0.0.0"), PORT):
            QMessageBox.critical(self, "Building Services Server",
                    QString("Failed to start server: %1")
                    .arg(self.tcpServer.errorString()))
            self.close()
            return

        self.connect(self, SIGNAL("clicked()"), self.close)
        font = self.font()
        font.setPointSize(24)
        self.setFont(font)
        self.setWindowTitle("Building Services Server")


    def loadBookings(self):
        # Generate fake data
        import random

        today = QDate.currentDate()
        for i in range(10):
            date = today.addDays(random.randint(7, 60))
            for j in range(random.randint(1, MAX_BOOKINGS_PER_DAY)):
                # Rooms are 001..534 excl. 100, 200, ..., 500
                floor = random.randint(0, 5)
                room = random.randint(1, 34)
                bookings = Bookings[date.toPyDate()]
                if len(bookings) >= MAX_BOOKINGS_PER_DAY:
                    continue
                bisect.insort(bookings, "{0:1d}{1:02d}".format(
                              floor, room))
        printBookings()


app = QApplication(sys.argv)
form = BuildingServicesDlg()
form.show()
form.move(0, 0)
app.exec_()

