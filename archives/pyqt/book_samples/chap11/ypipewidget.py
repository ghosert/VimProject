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

from PyQt4.QtCore import (QPointF, QSize, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QBrush, QColor, QFontMetricsF,
        QFrame, QLabel, QLinearGradient, QPainter, QPolygon,
        QSizePolicy, QSpinBox, QWidget)


class YPipeWidget(QWidget):

    def __init__(self, leftFlow=0, rightFlow=0, maxFlow=100,
                 parent=None):
        super(YPipeWidget, self).__init__(parent)

        self.leftSpinBox = QSpinBox(self)
        self.leftSpinBox.setRange(0, maxFlow)
        self.leftSpinBox.setValue(leftFlow)
        self.leftSpinBox.setSuffix(" l/s")
        self.leftSpinBox.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
        self.connect(self.leftSpinBox, SIGNAL("valueChanged(int)"),
                     self.valueChanged)

        self.rightSpinBox = QSpinBox(self)
        self.rightSpinBox.setRange(0, maxFlow)
        self.rightSpinBox.setValue(rightFlow)
        self.rightSpinBox.setSuffix(" l/s")
        self.rightSpinBox.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
        self.connect(self.rightSpinBox, SIGNAL("valueChanged(int)"),
                     self.valueChanged)

        self.label = QLabel(self)
        self.label.setFrameStyle(QFrame.StyledPanel|QFrame.Sunken)
        self.label.setAlignment(Qt.AlignCenter)
        fm = QFontMetricsF(self.font())
        self.label.setMinimumWidth(fm.width(" 999 l/s "))

        self.setSizePolicy(QSizePolicy(QSizePolicy.Expanding,
                                       QSizePolicy.Expanding))
        self.setMinimumSize(self.minimumSizeHint())
        self.valueChanged()


    def valueChanged(self):
        a = self.leftSpinBox.value()
        b = self.rightSpinBox.value()
        self.label.setText("{0} l/s".format(a + b))
        self.emit(SIGNAL("valueChanged"), a, b)
        self.update()


    def values(self):
        return self.leftSpinBox.value(), self.rightSpinBox.value()


    def minimumSizeHint(self):
        return QSize(self.leftSpinBox.width() * 3,
                     self.leftSpinBox.height() * 5)


    def resizeEvent(self, event=None):
        fm = QFontMetricsF(self.font())
        x = (self.width() - self.label.width()) / 2
        y = self.height() - (fm.height() * 1.5)
        self.label.move(x, y)
        y = self.height() / 60.0
        x = (self.width() / 4.0) - self.leftSpinBox.width()
        self.leftSpinBox.move(x, y)
        x = self.width() - (self.width() / 4.0)
        self.rightSpinBox.move(x, y)


    def paintEvent(self, event=None):
        LogicalSize = 100.0

        def logicalFromPhysical(length, side):
            return (length / side) * LogicalSize
        
        fm = QFontMetricsF(self.font())
        ymargin = ((LogicalSize / 30.0) +
                   logicalFromPhysical(self.leftSpinBox.height(),
                                       self.height()))
        ymax = (LogicalSize -
                logicalFromPhysical(fm.height() * 2, self.height()))
        width = LogicalSize / 4.0
        cx, cy = LogicalSize / 2.0, LogicalSize / 3.0
        ax, ay = cx - (2 * width), ymargin
        bx, by = cx - width, ay
        dx, dy = cx + width, ay
        ex, ey = cx + (2 * width), ymargin
        fx, fy = cx + (width / 2), cx + (LogicalSize / 24.0)
        gx, gy = fx, ymax
        hx, hy = cx - (width / 2), ymax
        ix, iy = hx, fy

        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)
        side = min(self.width(), self.height())
        painter.setViewport((self.width() - side) / 2,
                            (self.height() - side) / 2, side, side)
        painter.setWindow(0, 0, LogicalSize, LogicalSize)

        painter.setPen(Qt.NoPen)

        gradient = QLinearGradient(QPointF(0, 0),
                                         QPointF(0, 100))
        gradient.setColorAt(0, Qt.white)
        a = self.leftSpinBox.value()
        gradient.setColorAt(1, (Qt.red if a != 0 else Qt.white))
        painter.setBrush(QBrush(gradient))
        painter.drawPolygon(QPolygon([ax, ay, bx, by, cx, cy, ix, iy]))

        gradient = QLinearGradient(QPointF(0, 0), QPointF(0, 100))
        gradient.setColorAt(0, Qt.white)
        b = self.rightSpinBox.value()
        gradient.setColorAt(1, (Qt.blue if b != 0
                                else Qt.white))
        painter.setBrush(QBrush(gradient))
        painter.drawPolygon(QPolygon([cx, cy, dx, dy, ex, ey, fx, fy]))

        if (a + b) == 0:
            color = QColor(Qt.white)
        else:
            ashare = (a / (a + b)) * 255.0
            bshare = 255.0 - ashare
            color = QColor(ashare, 0, bshare)
        gradient = QLinearGradient(QPointF(0, 0), QPointF(0, 100))
        gradient.setColorAt(0, Qt.white)
        gradient.setColorAt(1, color)
        painter.setBrush(QBrush(gradient))
        painter.drawPolygon(QPolygon(
                [cx, cy, fx, fy, gx, gy, hx, hy, ix, iy]))

        painter.setPen(Qt.black)
        painter.drawPolyline(QPolygon([ax, ay, ix, iy, hx, hy]))
        painter.drawPolyline(QPolygon([gx, gy, fx, fy, ex, ey]))
        painter.drawPolyline(QPolygon([bx, by, cx, cy, dx, dy]))



if __name__ == "__main__":
    import sys

    def valueChanged(a, b):
        print(a, b)

    app = QApplication(sys.argv)
    form = YPipeWidget()
    form.connect(form, SIGNAL("valueChanged"), valueChanged)
    form.setWindowTitle("YPipe")
    form.move(0, 0)
    form.show()
    form.resize(400, 400)
    app.exec_()

