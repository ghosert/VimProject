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

from PyQt4.QtCore import (QDate, QString, Qt, SIGNAL, pyqtSignature)
from PyQt4.QtGui import (QApplication, QDialog, QDialogButtonBox)
import moviedata
import ui_addeditmoviedlg


class AddEditMovieDlg(QDialog,
        ui_addeditmoviedlg.Ui_AddEditMovieDlg):

    def __init__(self, movies, movie=None, parent=None):
        super(AddEditMovieDlg, self).__init__(parent)
        self.setupUi(self)

        self.movies = movies
        self.movie = movie
        self.acquiredDateEdit.setDisplayFormat(moviedata.DATEFORMAT)
        if movie is not None:
            self.titleLineEdit.setText(movie.title)
            self.yearSpinBox.setValue(movie.year)
            self.minutesSpinBox.setValue(movie.minutes)
            self.acquiredDateEdit.setDate(movie.acquired)
            self.acquiredDateEdit.setEnabled(False)
            self.notesTextEdit.setPlainText(movie.notes)
            self.notesTextEdit.setFocus()
            self.buttonBox.button(QDialogButtonBox.Ok).setText(
                                  "&Accept")
            self.setWindowTitle("My Movies - Edit Movie")
        else:
            today = QDate.currentDate()
            self.acquiredDateEdit.setDateRange(today.addDays(-5),
                                               today)
            self.acquiredDateEdit.setDate(today)
            self.titleLineEdit.setFocus()
        self.on_titleLineEdit_textEdited(QString())


    @pyqtSignature("QString")
    def on_titleLineEdit_textEdited(self, text):
        self.buttonBox.button(QDialogButtonBox.Ok).setEnabled(
                not self.titleLineEdit.text().isEmpty())


    def accept(self):
        title = self.titleLineEdit.text()
        year = self.yearSpinBox.value()
        minutes = self.minutesSpinBox.value()
        notes = self.notesTextEdit.toPlainText()
        if self.movie is None:
            acquired = self.acquiredDateEdit.date()
            self.movie = moviedata.Movie(title, year, minutes,
                                         acquired, notes)
            self.movies.add(self.movie)
        else:
            self.movies.updateMovie(self.movie, title, year,
                                    minutes, notes)
        QDialog.accept(self)


if __name__ == "__main__":
    import sys

    app = QApplication(sys.argv)
    form = AddEditMovieDlg(0)
    form.show()
    app.exec_()

