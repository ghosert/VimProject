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
import time
from PyQt4.QtCore import (QTime, QTimer, Qt, SIGNAL)
from PyQt4.QtGui import (QApplication, QFont, QFontMetrics, QLabel,
        QPainter, QPixmap, QTextDocument)


app = QApplication(sys.argv)

try:
    due = QTime.currentTime()
    message = "Alert!"
    if len(sys.argv) < 2:
        raise ValueError
    hours, mins = sys.argv[1].split(":")
    due = QTime(int(hours), int(mins))
    if not due.isValid():
        raise ValueError
    if len(sys.argv) > 2:
        message = " ".join(sys.argv[2:])
except ValueError:
    message = "Usage: alert.pyw HH:MM [optional message]" # 24hr clock

while QTime.currentTime() < due:
    time.sleep(20) # 20 seconds

font = QFont("Helvetica", 36, QFont.Bold)
fm = QFontMetrics(font)
pixmap = QPixmap(fm.width(message) + 5, fm.height() + 5)
pixmap.fill(Qt.white)
painter = QPainter(pixmap)
document = QTextDocument()
document.setDefaultFont(font)
document.setHtml("<font color=red>{0}</font>".format(message))
document.drawContents(painter)
painter.end()
label = QLabel()
label.setPixmap(pixmap)
label.setMask(pixmap.createMaskFromColor(Qt.white))
label.setWindowFlags(Qt.SplashScreen|Qt.FramelessWindowHint)
label.show()
QTimer.singleShot(60000, app.quit) # 1 minute
app.exec_()

