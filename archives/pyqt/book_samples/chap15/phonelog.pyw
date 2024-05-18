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

import os
import sys
from PyQt4.QtCore import (QDate, QDateTime, QFile, QString, QVariant, Qt,
        SIGNAL)
from PyQt4.QtGui import (QApplication, QCursor, QDataWidgetMapper,
        QDateTimeEdit, QDialog, QGridLayout, QHBoxLayout, QIcon, QLabel,
        QLineEdit, QMessageBox, QPixmap, QPushButton, QVBoxLayout)
from PyQt4.QtSql import (QSqlDatabase, QSqlQuery, QSqlTableModel)
import qrc_resources

MAC = True
try:
    from PyQt4.QtGui import qt_mac_set_native_menubar
except ImportError:
    MAC = False

ID, CALLER, STARTTIME, ENDTIME, TOPIC = range(5)
DATETIME_FORMAT = "yyyy-MM-dd hh:mm"


def createFakeData():
    import random

    print("Dropping table...")
    query = QSqlQuery()
    query.exec_("DROP TABLE calls")
    QApplication.processEvents()

    print("Creating table...")
    query.exec_("""CREATE TABLE calls (
                id INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,
                caller VARCHAR(40) NOT NULL,
                starttime DATETIME NOT NULL,
                endtime DATETIME NOT NULL,
                topic VARCHAR(80) NOT NULL)""")
    topics = ("Complaint", "Information request", "Off topic",
              "Information supplied", "Complaint", "Complaint")
    now = QDateTime.currentDateTime()
    print("Populating table...")
    query.prepare("INSERT INTO calls (caller, starttime, endtime, "
                  "topic) VALUES (?, ?, ?, ?)")
    for name in ('Joshan Cockerall', 'Ammanie Ingham',
            'Diarmuid Bettington', 'Juliana Bannister',
            'Oakley-Jay Buxton', 'Reilley Collinge',
            'Ellis-James Mcgehee', 'Jazmin Lawton',
            'Lily-Grace Smythe', 'Coskun Lant', 'Lauran Lanham',
            'Millar Poindexter', 'Naqeeb Neild', 'Maxlee Stoddart',
            'Rebia Luscombe', 'Briana Christine', 'Charli Pease',
            'Deena Mais', 'Havia Huffman', 'Ethan Davie',
            'Thomas-Jack Silver', 'Harpret Bray', 'Leigh-Ann Goodliff',
            'Seoras Bayes', 'Jenna Underhill', 'Veena Helps',
            'Mahad Mcintosh', 'Allie Hazlehurst', 'Aoife Warrington',
            'Cameron Burton', 'Yildirim Ahlberg', 'Alissa Clayton',
            'Josephine Weber', 'Fiore Govan', 'Howard Ragsdale',
            'Tiernan Larkins', 'Seren Sweeny', 'Arisha Keys',
            'Kiki Wearing', 'Kyran Ponsonby', 'Diannon Pepper',
            'Mari Foston', 'Sunil Manson', 'Donald Wykes',
            'Rosie Higham', 'Karmin Raines', 'Tayyibah Leathem',
            'Kara-jay Knoll', 'Shail Dalgleish', 'Jaimie Sells'):
        start = now.addDays(-random.randint(1, 30))
        start = now.addSecs(-random.randint(60 * 5, 60 * 60 * 2))
        end = start.addSecs(random.randint(20, 60 * 13))
        topic = random.choice(topics)
        query.addBindValue(QVariant(QString(name)))
        query.addBindValue(QVariant(start))
        query.addBindValue(QVariant(end))
        query.addBindValue(QVariant(QString(topic)))
        query.exec_()
    QApplication.processEvents()

    print("Calls:")
    query.exec_("SELECT id, caller, starttime, endtime, topic FROM calls "
                "ORDER by starttime")
    while query.next():
        id = query.value(0).toInt()[0]
        caller = unicode(query.value(1).toString())
        starttime = unicode(query.value(2).toDateTime().toString(
                            DATETIME_FORMAT))
        endtime = unicode(query.value(3).toDateTime().toString(
                          DATETIME_FORMAT))
        topic = unicode(query.value(4).toString())
        print("{0:02d}: {1} {2} - {3} {4}".format(id, caller,
              starttime, endtime, topic))
    QApplication.processEvents()


class PhoneLogDlg(QDialog):

    FIRST, PREV, NEXT, LAST = range(4)

    def __init__(self, parent=None):
        super(PhoneLogDlg, self).__init__(parent)

        callerLabel = QLabel("&Caller:")
        self.callerEdit = QLineEdit()
        callerLabel.setBuddy(self.callerEdit)
        today = QDate.currentDate()
        startLabel = QLabel("&Start:")
        self.startDateTime = QDateTimeEdit()
        startLabel.setBuddy(self.startDateTime)
        self.startDateTime.setDateRange(today, today)
        self.startDateTime.setDisplayFormat(DATETIME_FORMAT)
        endLabel = QLabel("&End:")
        self.endDateTime = QDateTimeEdit()
        endLabel.setBuddy(self.endDateTime)
        self.endDateTime.setDateRange(today, today)
        self.endDateTime.setDisplayFormat(DATETIME_FORMAT)
        topicLabel = QLabel("&Topic:")
        topicEdit = QLineEdit()
        topicLabel.setBuddy(topicEdit)
        firstButton = QPushButton()
        firstButton.setIcon(QIcon(":/first.png"))
        prevButton = QPushButton()
        prevButton.setIcon(QIcon(":/prev.png"))
        nextButton = QPushButton()
        nextButton.setIcon(QIcon(":/next.png"))
        lastButton = QPushButton()
        lastButton.setIcon(QIcon(":/last.png"))
        addButton = QPushButton("&Add")
        addButton.setIcon(QIcon(":/add.png"))
        deleteButton = QPushButton("&Delete")
        deleteButton.setIcon(QIcon(":/delete.png"))
        quitButton = QPushButton("&Quit")
        quitButton.setIcon(QIcon(":/quit.png"))
        if not MAC:
            addButton.setFocusPolicy(Qt.NoFocus)
            deleteButton.setFocusPolicy(Qt.NoFocus)

        fieldLayout = QGridLayout()
        fieldLayout.addWidget(callerLabel, 0, 0)
        fieldLayout.addWidget(self.callerEdit, 0, 1, 1, 3)
        fieldLayout.addWidget(startLabel, 1, 0)
        fieldLayout.addWidget(self.startDateTime, 1, 1)
        fieldLayout.addWidget(endLabel, 1, 2)
        fieldLayout.addWidget(self.endDateTime, 1, 3)
        fieldLayout.addWidget(topicLabel, 2, 0)
        fieldLayout.addWidget(topicEdit, 2, 1, 1, 3)
        navigationLayout = QHBoxLayout()
        navigationLayout.addWidget(firstButton)
        navigationLayout.addWidget(prevButton)
        navigationLayout.addWidget(nextButton)
        navigationLayout.addWidget(lastButton)
        fieldLayout.addLayout(navigationLayout, 3, 0, 1, 2)
        buttonLayout = QVBoxLayout()
        buttonLayout.addWidget(addButton)
        buttonLayout.addWidget(deleteButton)
        buttonLayout.addStretch()
        buttonLayout.addWidget(quitButton)
        layout = QHBoxLayout()
        layout.addLayout(fieldLayout)
        layout.addLayout(buttonLayout)
        self.setLayout(layout)

        self.model = QSqlTableModel(self)
        self.model.setTable("calls")
        self.model.setSort(STARTTIME, Qt.AscendingOrder)
        self.model.select()

        self.mapper = QDataWidgetMapper(self)
        self.mapper.setSubmitPolicy(QDataWidgetMapper.ManualSubmit)
        self.mapper.setModel(self.model)
        self.mapper.addMapping(self.callerEdit, CALLER)
        self.mapper.addMapping(self.startDateTime, STARTTIME)
        self.mapper.addMapping(self.endDateTime, ENDTIME)
        self.mapper.addMapping(topicEdit, TOPIC)
        self.mapper.toFirst()

        self.connect(firstButton, SIGNAL("clicked()"),
                     lambda: self.saveRecord(PhoneLogDlg.FIRST))
        self.connect(prevButton, SIGNAL("clicked()"),
                     lambda: self.saveRecord(PhoneLogDlg.PREV))
        self.connect(nextButton, SIGNAL("clicked()"),
                     lambda: self.saveRecord(PhoneLogDlg.NEXT))
        self.connect(lastButton, SIGNAL("clicked()"),
                     lambda: self.saveRecord(PhoneLogDlg.LAST))
        self.connect(addButton, SIGNAL("clicked()"), self.addRecord)
        self.connect(deleteButton, SIGNAL("clicked()"),
                     self.deleteRecord)
        self.connect(quitButton, SIGNAL("clicked()"), self.accept)

        self.setWindowTitle("Phone Log")


    def reject(self):
        self.accept()


    def accept(self):
        self.mapper.submit()
        QDialog.accept(self)

        
    def addRecord(self):
        row = self.model.rowCount()
        self.mapper.submit()
        self.model.insertRow(row)
        self.mapper.setCurrentIndex(row)
        now = QDateTime.currentDateTime()
        self.startDateTime.setDateTime(now)
        self.endDateTime.setDateTime(now)
        self.callerEdit.setFocus()


    def deleteRecord(self):
        caller = self.callerEdit.text()
        starttime = self.startDateTime.dateTime().toString(
                                            DATETIME_FORMAT)
        if (QMessageBox.question(self,
                QString("Delete"),
                QString("Delete call made by<br>%1 on %2?")
                .arg(caller).arg(starttime),
                QMessageBox.Yes|QMessageBox.No) ==
                QMessageBox.No):
            return
        row = self.mapper.currentIndex()
        self.model.removeRow(row)
        self.model.submitAll()
        if row + 1 >= self.model.rowCount():
            row = self.model.rowCount() - 1
        self.mapper.setCurrentIndex(row)


    def saveRecord(self, where):
        row = self.mapper.currentIndex()
        self.mapper.submit()
        if where == PhoneLogDlg.FIRST:
            row = 0
        elif where == PhoneLogDlg.PREV:
            row = 0 if row <= 1 else row - 1
        elif where == PhoneLogDlg.NEXT:
            row += 1
            if row >= self.model.rowCount():
                row = self.model.rowCount() - 1
        elif where == PhoneLogDlg.LAST:
            row = self.model.rowCount() - 1
        self.mapper.setCurrentIndex(row)


def main():
    app = QApplication(sys.argv)

    filename = os.path.join(os.path.dirname(__file__), "phonelog.db")
    create = not QFile.exists(filename)

    db = QSqlDatabase.addDatabase("QSQLITE")
    db.setDatabaseName(filename)
    if not db.open():
        QMessageBox.warning(None, "Phone Log",
            QString("Database Error: %1").arg(db.lastError().text()))
        sys.exit(1)

    splash = None
    if create:
        app.setOverrideCursor(QCursor(Qt.WaitCursor))
        splash = QLabel()
        pixmap = QPixmap(":/phonelogsplash.png")
        splash.setPixmap(pixmap)
        splash.setMask(pixmap.createHeuristicMask())
        splash.setWindowFlags(Qt.SplashScreen)
        rect = app.desktop().availableGeometry()
        splash.move((rect.width() - pixmap.width()) / 2,
                    (rect.height() - pixmap.height()) / 2)
        splash.show()
        app.processEvents()
        createFakeData()

    form = PhoneLogDlg()
    form.show()
    if create:
        splash.close()
        app.processEvents()
        app.restoreOverrideCursor()
    sys.exit(app.exec_())


main()

