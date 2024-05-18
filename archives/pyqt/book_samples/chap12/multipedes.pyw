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

import math
import random
import sys
from PyQt4.QtCore import (QObject, QPointF, QRectF, QTimer, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QBrush, QColor, QDialog,
        QGraphicsItem, QGraphicsScene, QGraphicsView, QHBoxLayout,
        QPainter, QPainterPath, QPolygonF, QPushButton, QSlider,
        QVBoxLayout)


SCENESIZE = 500
INTERVAL = 200

Running = False


class Head(QGraphicsItem):

    Rect = QRectF(-30, -20, 60, 40)

    def __init__(self, color, angle, position):
        super(Head, self).__init__()
        self.color = color
        self.angle = angle
        self.setPos(position)
        self.timer = QTimer()
        QObject.connect(self.timer, SIGNAL("timeout()"), self.timeout)
        self.timer.start(INTERVAL)


    def boundingRect(self):
        return Head.Rect


    def shape(self):
        path = QPainterPath()
        path.addEllipse(Head.Rect)
        return path


    def paint(self, painter, option, widget=None):
        painter.setPen(Qt.NoPen)
        painter.setBrush(QBrush(self.color))
        painter.drawEllipse(Head.Rect)
        if option.levelOfDetail > 0.5: # Outer eyes
            painter.setBrush(QBrush(Qt.yellow))
            painter.drawEllipse(-12, -19, 8, 8)
            painter.drawEllipse(-12, 11, 8, 8)
            if option.levelOfDetail > 0.9: # Inner eyes
                painter.setBrush(QBrush(Qt.darkBlue))
                painter.drawEllipse(-12, -19, 4, 4)
                painter.drawEllipse(-12, 11, 4, 4)
                if option.levelOfDetail > 1.3: # Nostrils
                    painter.setBrush(QBrush(Qt.white))
                    painter.drawEllipse(-27, -5, 2, 2)
                    painter.drawEllipse(-27, 3, 2, 2)


    def timeout(self):
        if not Running:
            return
        angle = self.angle
        while True:
            angle += random.randint(-9, 9)
            offset = random.randint(3, 15)
            x = self.x() + (offset * math.sin(math.radians(angle)))
            y = self.y() + (offset * math.cos(math.radians(angle)))
            if 0 <= x <= SCENESIZE and 0 <= y <= SCENESIZE:
                break
        self.angle = angle
        self.rotate(random.randint(-5, 5))
        self.setPos(QPointF(x, y))
        for item in self.scene().collidingItems(self):
            if isinstance(item, Head):
                self.color.setRed(min(255, self.color.red() + 1))
            else:
                item.color.setBlue(min(255, item.color.blue() + 1))



class Segment(QGraphicsItem):

    def __init__(self, color, offset, parent):
        super(Segment, self).__init__(parent)
        self.color = color
        self.rect = QRectF(offset, -20, 30, 40)
        self.path = QPainterPath()
        self.path.addEllipse(self.rect)
        x = offset + 15
        y = -20
        self.path.addPolygon(QPolygonF([QPointF(x, y),
                QPointF(x - 5, y - 12), QPointF(x - 5, y)]))
        self.path.closeSubpath()
        y = 20
        self.path.addPolygon(QPolygonF([QPointF(x, y),
                QPointF(x - 5, y + 12), QPointF(x - 5, y)]))
        self.path.closeSubpath()
        self.change = 1
        self.angle = 0
        self.timer = QTimer()
        QObject.connect(self.timer, SIGNAL("timeout()"), self.timeout)
        self.timer.start(INTERVAL)


    def boundingRect(self):
        return self.path.boundingRect()


    def shape(self):
        return self.path


    def paint(self, painter, option, widget=None):
        painter.setPen(Qt.NoPen)
        painter.setBrush(QBrush(self.color))
        if option.levelOfDetail < 0.9:
            painter.drawEllipse(self.rect)
        else:
            painter.drawPath(self.path)


    def timeout(self):
        if not Running:
            return
        matrix = self.matrix()
        matrix.reset()
        self.setMatrix(matrix)
        self.angle += self.change
        if self.angle > 5:
            self.change = -1
            self.angle -= 1
        elif self.angle < -5:
            self.change = 1
            self.angle += 1
        self.rotate(self.angle)


class MainForm(QDialog):

    def __init__(self, parent=None):
        super(MainForm, self).__init__(parent)

        self.scene = QGraphicsScene(self)
        self.scene.setSceneRect(0, 0, SCENESIZE, SCENESIZE)
        self.scene.setItemIndexMethod(QGraphicsScene.NoIndex)
        self.view = QGraphicsView()
        self.view.setRenderHint(QPainter.Antialiasing)
        self.view.setScene(self.scene)
        self.view.setFocusPolicy(Qt.NoFocus)
        zoomSlider = QSlider(Qt.Horizontal)
        zoomSlider.setRange(5, 200)
        zoomSlider.setValue(100)
        self.pauseButton = QPushButton("Pa&use")
        quitButton = QPushButton("&Quit")
        quitButton.setFocusPolicy(Qt.NoFocus)

        layout = QVBoxLayout()
        layout.addWidget(self.view)
        bottomLayout = QHBoxLayout()
        bottomLayout.addWidget(self.pauseButton)
        bottomLayout.addWidget(zoomSlider)
        bottomLayout.addWidget(quitButton)
        layout.addLayout(bottomLayout)
        self.setLayout(layout)

        self.connect(zoomSlider, SIGNAL("valueChanged(int)"), self.zoom)
        self.connect(self.pauseButton, SIGNAL("clicked()"),
                     self.pauseOrResume)
        self.connect(quitButton, SIGNAL("clicked()"), self.accept)

        self.populate()
        self.startTimer(INTERVAL)
        self.setWindowTitle("Multipedes")


    def zoom(self, value):
        factor = value / 100.0
        matrix = self.view.matrix()
        matrix.reset()
        matrix.scale(factor, factor)
        self.view.setMatrix(matrix)


    def pauseOrResume(self):
        global Running
        Running = not Running
        self.pauseButton.setText("Pa&use" if Running else "Res&ume")


    def populate(self):
        red, green, blue = 0, 150, 0
        for i in range(random.randint(6, 10)):
            angle = random.randint(0, 360)
            offset = random.randint(0, SCENESIZE // 2)
            half = SCENESIZE / 2
            x = half + (offset * math.sin(math.radians(angle)))
            y = half + (offset * math.cos(math.radians(angle)))
            color = QColor(red, green, blue)
            head = Head(color, angle, QPointF(x, y))
            color = QColor(red, green + random.randint(10, 60), blue)
            offset = 25
            segment = Segment(color, offset, head)
            for j in range(random.randint(3, 7)):
                offset += 25
                segment = Segment(color, offset, segment)
            head.rotate(random.randint(0, 360))
            self.scene.addItem(head)
        global Running
        Running = True


    def timerEvent(self, event):
        if not Running:
            return
        dead = set()
        items = self.scene.items()
        if len(items) == 0:
            self.populate()
            return
        heads = set()
        for item in items:
            if isinstance(item, Head):
                heads.add(item)
                if item.color.red() == 255:
                    dead.add(item)
        if len(heads) == 1:
            dead = heads
        del heads
        while dead:
            item = dead.pop()
            self.scene.removeItem(item)
            del item


app = QApplication(sys.argv)
form = MainForm()
rect = QApplication.desktop().availableGeometry()
form.resize(int(rect.width() * 0.75), int(rect.height() * 0.9))
form.show()
app.exec_()

