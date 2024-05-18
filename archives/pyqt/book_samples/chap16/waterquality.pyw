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

import gzip
import os
import platform
import sys
from PyQt4.QtCore import (QAbstractTableModel, QDateTime, QModelIndex,
        QSize, QString, QTimer, QVariant, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QColor, QCursor, QDialog, QFont,
        QFontDatabase, QFontMetrics, QHBoxLayout, QLabel, QMessageBox,
        QPainter, QPalette, QPixmap, QScrollArea, QSplitter, QTableView,
        QWidget)


(TIMESTAMP, TEMPERATURE, INLETFLOW, TURBIDITY, CONDUCTIVITY,
 COAGULATION, RAWPH, FLOCCULATEDPH) = range(8)

TIMESTAMPFORMAT = "yyyy-MM-dd hh:mm"


class WaterQualityModel(QAbstractTableModel):

    def __init__(self, filename):
        super(WaterQualityModel, self).__init__()
        self.filename = filename
        self.results = []


    def load(self):
        exception = None
        fh = None
        try:
            if not self.filename:
                raise IOError, "no filename specified for loading"
            self.results = []
            for line in gzip.open(self.filename):
                parts = line.rstrip().split(",")
                date = QDateTime.fromString(parts[0] + ":00",
                                            Qt.ISODate)
                result = [date]
                for part in parts[1:]:
                    result.append(float(part))
                self.results.append(result)
        except (IOError, ValueError), e:
            exception = e
        finally:
            if fh is not None:
                fh.close()
            self.reset()
            if exception is not None:
                raise exception


    def data(self, index, role=Qt.DisplayRole):
        if (not index.isValid() or
            not (0 <= index.row() < len(self.results))):
            return QVariant()
        column = index.column()
        result = self.results[index.row()]
        if role == Qt.DisplayRole:
            item = result[column]
            if column == TIMESTAMP:
                item = item.toString(TIMESTAMPFORMAT)
            else:
                item = QString("%1").arg(item, 0, "f", 2)
            return QVariant(item)
        elif role == Qt.TextAlignmentRole:
            if column != TIMESTAMP:
                return QVariant(int(Qt.AlignRight|Qt.AlignVCenter))
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        elif role == Qt.TextColorRole and column == INLETFLOW:
            if result[column] < 0:
                return QVariant(QColor(Qt.red))
        elif (role == Qt.TextColorRole and
              column in (RAWPH, FLOCCULATEDPH)):
            ph = result[column]
            if ph < 7:
                return QVariant(QColor(Qt.red))
            elif ph >= 8:
                return QVariant(QColor(Qt.blue))
            else:
                return QVariant(QColor(Qt.darkGreen))
        return QVariant()


    def headerData(self, section, orientation, role=Qt.DisplayRole):
        if role == Qt.TextAlignmentRole:
            if orientation == Qt.Horizontal:
                return QVariant(int(Qt.AlignCenter))
            return QVariant(int(Qt.AlignRight|Qt.AlignVCenter))
        if role != Qt.DisplayRole:
            return QVariant()
        if orientation == Qt.Horizontal:
            if section == TIMESTAMP:
                return QVariant("Timestamp")
            elif section == TEMPERATURE:
                return QVariant("\u00B0" +"C")
            elif section == INLETFLOW:
                return QVariant("Inflow")
            elif section == TURBIDITY:
                return QVariant("NTU")
            elif section == CONDUCTIVITY:
                return QVariant("\u03BCS/cm")
            elif section == COAGULATION:
                return QVariant("mg/L")
            elif section == RAWPH:
                return QVariant("Raw Ph")
            elif section == FLOCCULATEDPH:
                return QVariant("Floc Ph")
        return QVariant(int(section + 1))


    def rowCount(self, index=QModelIndex()):
        return len(self.results)


    def columnCount(self, index=QModelIndex()):
        return 8


class WaterQualityView(QWidget):

    FLOWCHARS = (unichr(0x21DC), unichr(0x21DD), unichr(0x21C9))

    def __init__(self, parent=None):
        super(WaterQualityView, self).__init__(parent)
        self.scrollarea = None
        self.model = None
        self.setFocusPolicy(Qt.StrongFocus)
        self.selectedRow = -1
        self.flowfont = self.font()
        size = self.font().pointSize()
        if platform.system() == "Windows":
            fontDb = QFontDatabase()
            for face in [face.toLower() for face in fontDb.families()]:
                if face.contains("unicode"):
                    self.flowfont = QFont(face, size)
                    break
            else:
                self.flowfont = QFont("symbol", size)
                WaterQualityView.FLOWCHARS = (chr(0xAC), chr(0xAE),
                                              chr(0xDE))


    def setModel(self, model):
        self.model = model
        self.connect(self.model,
                SIGNAL("dataChanged(QModelIndex,QModelIndex)"),
                self.setNewSize)
        self.connect(self.model, SIGNAL("modelReset()"), self.setNewSize)
        self.setNewSize()


    def setNewSize(self):
        self.resize(self.sizeHint())
        self.update()
        self.updateGeometry()


    def minimumSizeHint(self):
        size = self.sizeHint()
        fm = QFontMetrics(self.font())
        size.setHeight(fm.height() * 3)
        return size


    def sizeHint(self):
        fm = QFontMetrics(self.font())
        size = fm.height()
        return QSize(fm.width("9999-99-99 99:99 ") + (size * 4),
                     (size / 4) + (size * self.model.rowCount()))


    def paintEvent(self, event):
        if self.model is None:
            return
        fm = QFontMetrics(self.font())
        timestampWidth = fm.width("9999-99-99 99:99 ")
        size = fm.height()
        indicatorSize = int(size * 0.8)
        offset = int(1.5 * (size - indicatorSize))
        minY = event.rect().y()
        maxY = minY + event.rect().height() + size
        minY -= size
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)
        painter.setRenderHint(QPainter.TextAntialiasing)
        y = 0
        for row in range(self.model.rowCount()):
            x = 0
            if minY <= y <= maxY:
                painter.save()
                painter.setPen(self.palette().color(QPalette.Text))
                if row == self.selectedRow:
                    painter.fillRect(x, y + (offset * 0.8),
                            self.width(), size, self.palette().highlight())
                    painter.setPen(self.palette().color(
                            QPalette.HighlightedText))
                timestamp = self.model.data(
                        self.model.index(row, TIMESTAMP)).toDateTime()
                painter.drawText(x, y + size,
                        timestamp.toString(TIMESTAMPFORMAT))
                x += timestampWidth
                temperature = self.model.data(
                        self.model.index(row, TEMPERATURE))
                temperature = temperature.toDouble()[0]
                if temperature < 20:
                    color = QColor(0, 0,
                            int(255 * (20 - temperature) / 20))
                elif temperature > 25:
                    color = QColor(int(255 * temperature / 100), 0, 0)
                else:
                    color = QColor(0, int(255 * temperature / 100), 0)
                painter.setPen(Qt.NoPen)
                painter.setBrush(color)
                painter.drawEllipse(x, y + offset, indicatorSize,
                                    indicatorSize)
                x += size
                rawPh = self.model.data(self.model.index(row, RAWPH))
                rawPh = rawPh.toDouble()[0]
                if rawPh < 7:
                    color = QColor(int(255 * rawPh / 10), 0, 0)
                elif rawPh >= 8:
                    color = QColor(0, 0, int(255 * rawPh / 10))
                else:
                    color = QColor(0, int(255 * rawPh / 10), 0)
                painter.setBrush(color)
                painter.drawEllipse(x, y + offset, indicatorSize,
                                    indicatorSize)
                x += size
                flocPh = self.model.data(
                        self.model.index(row, FLOCCULATEDPH))
                flocPh = flocPh.toDouble()[0]
                if flocPh < 7:
                    color = QColor(int(255 * flocPh / 10), 0, 0)
                elif flocPh >= 8:
                    color = QColor(0, 0, int(255 * flocPh / 10))
                else:
                    color = QColor(0, int(255 * flocPh / 10), 0)
                painter.setBrush(color)
                painter.drawEllipse(x, y + offset, indicatorSize,
                                    indicatorSize)
                painter.restore()
                painter.save()
                x += size
                flow = self.model.data(
                        self.model.index(row, INLETFLOW))
                flow = flow.toDouble()[0]
                char = None
                if flow <= 0:
                    char = WaterQualityView.FLOWCHARS[0]
                elif flow < 3:
                    char = WaterQualityView.FLOWCHARS[1]
                elif flow > 5.5:
                    char = WaterQualityView.FLOWCHARS[2]
                if char is not None:
                    painter.setFont(self.flowfont)
                    painter.drawText(x, y + size, char)
                painter.restore()
            y += size
            if y > maxY:
                break


    def mousePressEvent(self, event):
        fm = QFontMetrics(self.font())
        self.selectedRow = event.y() // fm.height()
        self.update()
        self.emit(SIGNAL("clicked(QModelIndex)"),
                  self.model.index(self.selectedRow, 0))


    def keyPressEvent(self, event):
        if self.model is None:
            return
        row = -1
        if event.key() == Qt.Key_Up:
            row = max(0, self.selectedRow - 1)
        elif event.key() == Qt.Key_Down:
            row = min(self.selectedRow + 1, self.model.rowCount() - 1)
        if row != -1 and row != self.selectedRow:
            self.selectedRow = row
            if self.scrollarea is not None:
                fm = QFontMetrics(self.font())
                y = fm.height() * self.selectedRow
                self.scrollarea.ensureVisible(0, y)
            self.update()
            self.emit(SIGNAL("clicked(QModelIndex)"),
                      self.model.index(self.selectedRow, 0))
        else:
            QWidget.keyPressEvent(self, event)


class MainForm(QDialog):

    def __init__(self, parent=None):
        super(MainForm, self).__init__(parent)

        self.model = WaterQualityModel(os.path.join(
                os.path.dirname(__file__), "waterdata.csv.gz"))
        self.tableView = QTableView()
        self.tableView.setAlternatingRowColors(True)
        self.tableView.setModel(self.model)
        self.waterView = WaterQualityView()
        self.waterView.setModel(self.model)
        scrollArea = QScrollArea()
        scrollArea.setBackgroundRole(QPalette.Light)
        scrollArea.setWidget(self.waterView)
        self.waterView.scrollarea = scrollArea

        splitter = QSplitter(Qt.Horizontal)
        splitter.addWidget(self.tableView)
        splitter.addWidget(scrollArea)
        splitter.setSizes([600, 250])
        layout = QHBoxLayout()
        layout.addWidget(splitter)
        self.setLayout(layout)

        self.setWindowTitle("Water Quality Data")
        QTimer.singleShot(0, self.initialLoad)


    def initialLoad(self):
        QApplication.setOverrideCursor(QCursor(Qt.WaitCursor))
        splash = QLabel(self)
        pixmap = QPixmap(os.path.join(os.path.dirname(__file__),
                "iss013-e-14802.jpg"))
        splash.setPixmap(pixmap)
        splash.setWindowFlags(Qt.SplashScreen)
        splash.move(self.x() + ((self.width() - pixmap.width()) / 2),
                    self.y() + ((self.height() - pixmap.height()) / 2))
        splash.show()
        QApplication.processEvents()
        try:
            self.model.load()
        except IOError, e:
            QMessageBox.warning(self, "Water Quality - Error", e)
        else:
            self.tableView.resizeColumnsToContents()
        splash.close()
        QApplication.processEvents()
        QApplication.restoreOverrideCursor()


app = QApplication(sys.argv)
form = MainForm()
form.resize(850, 620)
form.show()
app.exec_()

