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

import platform
from PyQt4.QtCore import (QAbstractTableModel, QDataStream, QFile,
        QIODevice, QModelIndex, QRegExp, QSize, QString, QVariant, Qt,
        SIGNAL)
from PyQt4.QtGui import (QApplication, QColor, QComboBox, QLineEdit,
        QSpinBox, QStyle, QStyledItemDelegate, QTextDocument, QTextEdit)
import richtextlineedit


NAME, OWNER, COUNTRY, DESCRIPTION, TEU = range(5)

MAGIC_NUMBER = 0x570C4
FILE_VERSION = 1


class Ship(object):

    def __init__(self, name, owner, country, teu=0, description=""):
        self.name = QString(name)
        self.owner = QString(owner)
        self.country = QString(country)
        self.teu = teu
        self.description = QString(description)


    def __hash__(self):
        return super(Ship, self).__hash__()


    def __lt__(self, other):
        r = QString.localeAwareCompare(self.name.toLower(),
                                       other.name.toLower())
        return True if r < 0 else False


    def __eq__(self, other):
        return 0 == QString.localeAwareCompare(self.name.toLower(),
                                               other.name.toLower())


class ShipContainer(object):

    def __init__(self, filename=QString()):
        self.filename = QString(filename)
        self.dirty = False
        self.ships = {}
        self.owners = set()
        self.countries = set()


    def ship(self, identity):
        return self.ships.get(identity)

        
    def addShip(self, ship):
        self.ships[id(ship)] = ship
        self.owners.add(unicode(ship.owner))
        self.countries.add(unicode(ship.country))
        self.dirty = True


    def removeShip(self, ship):
        del self.ships[id(ship)]
        del ship
        self.dirty = True


    def __len__(self):
        return len(self.ships)


    def __iter__(self):
        for ship in self.ships.values():
            yield ship


    def inOrder(self):
        return sorted(self.ships.values())


    def inCountryOwnerOrder(self):
        return sorted(self.ships.values(),
                      key=lambda x: (x.country, x.owner, x.name))


    def load(self):
        exception = None
        fh = None
        try:
            if self.filename.isEmpty():
                raise IOError, "no filename specified for loading"
            fh = QFile(self.filename)
            if not fh.open(QIODevice.ReadOnly):
                raise IOError, unicode(fh.errorString())
            stream = QDataStream(fh)
            magic = stream.readInt32()
            if magic != MAGIC_NUMBER:
                raise IOError, "unrecognized file type"
            fileVersion = stream.readInt16()
            if fileVersion != FILE_VERSION:
                raise IOError, "unrecognized file type version"
            self.ships = {}
            while not stream.atEnd():
                name = QString()
                owner = QString()
                country = QString()
                description = QString()
                stream >> name >> owner >> country >> description
                teu = stream.readInt32()
                ship = Ship(name, owner, country, teu, description)
                self.ships[id(ship)] = ship
                self.owners.add(unicode(owner))
                self.countries.add(unicode(country))
            self.dirty = False
        except IOError, e:
            exception = e
        finally:
            if fh is not None:
                fh.close()
            if exception is not None:
                raise exception


    def save(self):
        exception = None
        fh = None
        try:
            if self.filename.isEmpty():
                raise IOError, "no filename specified for saving"
            fh = QFile(self.filename)
            if not fh.open(QIODevice.WriteOnly):
                raise IOError, unicode(fh.errorString())
            stream = QDataStream(fh)
            stream.writeInt32(MAGIC_NUMBER)
            stream.writeInt16(FILE_VERSION)
            stream.setVersion(QDataStream.Qt_4_1)
            for ship in self.ships.values():
                stream << ship.name << ship.owner << ship.country \
                       << ship.description
                stream.writeInt32(ship.teu)
            self.dirty = False
        except IOError, e:
            exception = e
        finally:
            if fh is not None:
                fh.close()
            if exception is not None:
                raise exception


class ShipTableModel(QAbstractTableModel):

    def __init__(self, filename=QString()):
        super(ShipTableModel, self).__init__()
        self.filename = filename
        self.dirty = False
        self.ships = []
        self.owners = set()
        self.countries = set()


    def sortByName(self):
        self.ships = sorted(self.ships)
        self.reset()


    def sortByCountryOwner(self):
        self.ships = sorted(self.ships,
                            key=lambda x: (x.country, x.owner, x.name))
        self.reset()


    def flags(self, index):
        if not index.isValid():
            return Qt.ItemIsEnabled
        return Qt.ItemFlags(
                QAbstractTableModel.flags(self, index)|
                Qt.ItemIsEditable)


    def data(self, index, role=Qt.DisplayRole):
        if (not index.isValid() or
            not (0 <= index.row() < len(self.ships))):
            return QVariant()
        ship = self.ships[index.row()]
        column = index.column()
        if role == Qt.DisplayRole:
            if column == NAME:
                return QVariant(ship.name)
            elif column == OWNER:
                return QVariant(ship.owner)
            elif column == COUNTRY:
                return QVariant(ship.country)
            elif column == DESCRIPTION:
                return QVariant(ship.description)
            elif column == TEU:
                return QVariant(QString("%L1").arg(ship.teu))
        elif role == Qt.TextAlignmentRole:
            if column == TEU:
                return QVariant(int(Qt.AlignRight|Qt.AlignVCenter))
            return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
        elif role == Qt.TextColorRole and column == TEU:
            if ship.teu < 80000:
                return QVariant(QColor(Qt.black))
            elif ship.teu < 100000:
                return QVariant(QColor(Qt.darkBlue))
            elif ship.teu < 120000:
                return QVariant(QColor(Qt.blue))
            else:
                return QVariant(QColor(Qt.red))
        elif role == Qt.BackgroundColorRole:
            if ship.country in ("Bahamas", "Cyprus", "Denmark",
                    "France", "Germany", "Greece"):
                return QVariant(QColor(250, 230, 250))
            elif ship.country in ("Hong Kong", "Japan", "Taiwan"):
                return QVariant(QColor(250, 250, 230))
            elif ship.country in ("Marshall Islands",):
                return QVariant(QColor(230, 250, 250))
            else:
                return QVariant(QColor(210, 230, 230))
        return QVariant()


    def headerData(self, section, orientation, role=Qt.DisplayRole):
        if role == Qt.TextAlignmentRole:
            if orientation == Qt.Horizontal:
                return QVariant(int(Qt.AlignLeft|Qt.AlignVCenter))
            return QVariant(int(Qt.AlignRight|Qt.AlignVCenter))
        if role != Qt.DisplayRole:
            return QVariant()
        if orientation == Qt.Horizontal:
            if section == NAME:
                return QVariant("Name")
            elif section == OWNER:
                return QVariant("Owner")
            elif section == COUNTRY:
                return QVariant("Country")
            elif section == DESCRIPTION:
                return QVariant("Description")
            elif section == TEU:
                return QVariant("TEU")
        return QVariant(int(section + 1))


    def rowCount(self, index=QModelIndex()):
        return len(self.ships)


    def columnCount(self, index=QModelIndex()):
        return 5


    def setData(self, index, value, role=Qt.EditRole):
        """
        This method will be called when editing the data so that model can be updated and view will be reflected to update as well.
        For the default delegate, this call is automatically while for the customize delegate, this call should be made in the method:
        delegate.setModelData(self, editor, model, index), see more details below.
        """
        if index.isValid() and 0 <= index.row() < len(self.ships):
            ship = self.ships[index.row()]
            column = index.column()
            if column == NAME:
                ship.name = value.toString()
            elif column == OWNER:
                ship.owner = value.toString()
            elif column == COUNTRY:
                ship.country = value.toString()
            elif column == DESCRIPTION:
                ship.description = value.toString()
            elif column == TEU:
                value, ok = value.toInt()
                if ok:
                    ship.teu = value
            self.dirty = True
            self.emit(SIGNAL("dataChanged(QModelIndex,QModelIndex)"),
                      index, index)
            return True
        return False


    def insertRows(self, position, rows=1, index=QModelIndex()):
        self.beginInsertRows(QModelIndex(), position, position + rows - 1)
        for row in range(rows):
            self.ships.insert(position + row,
                              Ship(" Unknown", " Unknown", " Unknown"))
        self.endInsertRows()
        self.dirty = True
        return True


    def removeRows(self, position, rows=1, index=QModelIndex()):
        self.beginRemoveRows(QModelIndex(), position, position + rows - 1)
        self.ships = (self.ships[:position] +
                      self.ships[position + rows:])
        self.endRemoveRows()
        self.dirty = True
        return True


    def load(self):
        exception = None
        fh = None
        try:
            if self.filename.isEmpty():
                raise IOError, "no filename specified for loading"
            fh = QFile(self.filename)
            if not fh.open(QIODevice.ReadOnly):
                raise IOError, unicode(fh.errorString())
            stream = QDataStream(fh)
            magic = stream.readInt32()
            if magic != MAGIC_NUMBER:
                raise IOError, "unrecognized file type"
            fileVersion = stream.readInt16()
            if fileVersion != FILE_VERSION:
                raise IOError, "unrecognized file type version"
            self.ships = []
            while not stream.atEnd():
                name = QString()
                owner = QString()
                country = QString()
                description = QString()
                stream >> name >> owner >> country >> description
                teu = stream.readInt32()
                self.ships.append(Ship(name, owner, country, teu,
                                       description))
                self.owners.add(unicode(owner))
                self.countries.add(unicode(country))
            self.dirty = False
        except IOError, e:
            exception = e
        finally:
            if fh is not None:
                fh.close()
            if exception is not None:
                raise exception


    def save(self):
        exception = None
        fh = None
        try:
            if self.filename.isEmpty():
                raise IOError, "no filename specified for saving"
            fh = QFile(self.filename)
            if not fh.open(QIODevice.WriteOnly):
                raise IOError, unicode(fh.errorString())
            stream = QDataStream(fh)
            stream.writeInt32(MAGIC_NUMBER)
            stream.writeInt16(FILE_VERSION)
            stream.setVersion(QDataStream.Qt_4_1)
            for ship in self.ships:
                stream << ship.name << ship.owner << ship.country \
                       << ship.description
                stream.writeInt32(ship.teu)
            self.dirty = False
        except IOError, e:
            exception = e
        finally:
            if fh is not None:
                fh.close()
            if exception is not None:
                raise exception


class ShipDelegate(QStyledItemDelegate):

    def __init__(self, parent=None):
        super(ShipDelegate, self).__init__(parent)


    def paint(self, painter, option, index):
        if index.column() == DESCRIPTION:
            text = index.model().data(index).toString()
            palette = QApplication.palette()
            document = QTextDocument()
            document.setDefaultFont(option.font)
            if option.state & QStyle.State_Selected:
                document.setHtml(QString("<font color=%1>%2</font>")
                        .arg(palette.highlightedText().color().name())
                        .arg(text))
            else:
                document.setHtml(text)
            color = (palette.highlight().color()
                     if option.state & QStyle.State_Selected
                     else QColor(index.model().data(index,
                                 Qt.BackgroundColorRole)))
            painter.save()
            painter.fillRect(option.rect, color)
            painter.translate(option.rect.x(), option.rect.y())
            document.drawContents(painter)
            painter.restore()
        else:
            QStyledItemDelegate.paint(self, painter, option, index)


    def sizeHint(self, option, index):
        """
        This method is re-implemented because Description column is using HTML, it must return the exactly number of characters for
        presentation purpose rather than the number of HTML raw characters.
        """
        fm = option.fontMetrics
        if index.column() == TEU:
            return QSize(fm.width("9,999,999"), fm.height())
        if index.column() == DESCRIPTION:
            text = index.model().data(index).toString()
            document = QTextDocument()
            document.setDefaultFont(option.font)
            document.setHtml(text)
            return QSize(document.idealWidth() + 5, fm.height())
        return QStyledItemDelegate.sizeHint(self, option, index)


    def createEditor(self, parent, option, index):
        if index.column() == TEU:
            spinbox = QSpinBox(parent)
            spinbox.setRange(0, 200000)
            spinbox.setSingleStep(1000)
            spinbox.setAlignment(Qt.AlignRight|Qt.AlignVCenter)
            return spinbox
        elif index.column() == OWNER:
            combobox = QComboBox(parent)
            combobox.addItems(sorted(index.model().owners))
            combobox.setEditable(True)
            return combobox
        elif index.column() == COUNTRY:
            combobox = QComboBox(parent)
            combobox.addItems(sorted(index.model().countries))
            combobox.setEditable(True)
            return combobox
        elif index.column() == NAME:
            editor = QLineEdit(parent)
            self.connect(editor, SIGNAL("returnPressed()"),
                         self.commitAndCloseEditor)
            return editor
        elif index.column() == DESCRIPTION:
            editor = richtextlineedit.RichTextLineEdit(parent)
            self.connect(editor, SIGNAL("returnPressed()"),
                         self.commitAndCloseEditor)
            return editor
        else:
            return QStyledItemDelegate.createEditor(self, parent, option,
                                                    index)


    def commitAndCloseEditor(self):
        editor = self.sender()
        if isinstance(editor, (QTextEdit, QLineEdit)):
            self.emit(SIGNAL("commitData(QWidget*)"), editor)
            self.emit(SIGNAL("closeEditor(QWidget*)"), editor)


    def setEditorData(self, editor, index):
        text = index.model().data(index, Qt.DisplayRole).toString()
        if index.column() == TEU:
            value = text.replace(QRegExp("[., ]"), "").toInt()[0]
            editor.setValue(value)
        elif index.column() in (OWNER, COUNTRY):
            i = editor.findText(text)
            if i == -1:
                i = 0
            editor.setCurrentIndex(i)
        elif index.column() == NAME:
            editor.setText(text)
        elif index.column() == DESCRIPTION:
            editor.setHtml(text)
        else:
            QStyledItemDelegate.setEditorData(self, editor, index)


    def setModelData(self, editor, model, index):
        if index.column() == TEU:
            model.setData(index, QVariant(editor.value()))
        elif index.column() in (OWNER, COUNTRY):
            model.setData(index, QVariant(editor.currentText()))
        elif index.column() == NAME:
            model.setData(index, QVariant(editor.text()))
        elif index.column() == DESCRIPTION:
            model.setData(index, QVariant(editor.toSimpleHtml()))
        else:
            QStyledItemDelegate.setModelData(self, editor, model, index)


def generateFakeShips():
    for name, owner, country, teu, description in (
("Emma M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 151687,
 "<b>W\u00E4rtsil\u00E4-Sulzer RTA96-C</b> main engine,"
 "<font color=green>109,000 hp</font>"),
("MSC Pamela", "MSC", "Liberia", 90449,
 "Draft <font color=green>15m</font>"),
("Colombo Express", "Hapag-Lloyd", "Germany", 93750,
 "Main engine, <font color=green>93,500 hp</font>"),
("Houston Express", "Norddeutsche Reederei", "Germany", 95000,
 "Features a <u>twisted leading edge full spade rudder</u>. "
 "Sister of <i>Savannah Express</i>"),
("Savannah Express", "Norddeutsche Reederei", "Germany", 95000,
 "Sister of <i>Houston Express</i>"),
("MSC Susanna", "MSC", "Liberia", 90449, ""),
("Eleonora M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 151687,
 "Captain <i>Hallam</i>"),
("Estelle M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 151687,
 "Captain <i>Wells</i>"),
("Evelyn M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 151687,
  "Captain <i>Byrne</i>"),
("Georg M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 97933, ""),
("Gerd M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 97933, ""),
("Gjertrud M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 97933, ""),
("Grete M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 97933, ""),
("Gudrun M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 97933, ""),
("Gunvor M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 97933, ""),
("CSCL Le Havre", "Danaos Shipping", "Cyprus", 107200, ""),
("CSCL Pusan", "Danaos Shipping", "Cyprus", 107200,
 "Captain <i>Watts</i>"),
("Xin Los Angeles", "China Shipping Container Lines (CSCL)",
 "Hong Kong", 107200, ""),
("Xin Shanghai", "China Shipping Container Lines (CSCL)", "Hong Kong",
 107200, ""),
("Cosco Beijing", "Costamare Shipping", "Greece", 99833, ""),
("Cosco Hellas", "Costamare Shipping", "Greece", 99833, ""),
("Cosco Guangzho", "Costamare Shipping", "Greece", 99833, ""),
("Cosco Ningbo", "Costamare Shipping", "Greece", 99833, ""),
("Cosco Yantian", "Costamare Shipping", "Greece", 99833, ""),
("CMA CGM Fidelio", "CMA CGM", "France", 99500, ""),
("CMA CGM Medea", "CMA CGM", "France", 95000, ""),
("CMA CGM Norma", "CMA CGM", "Bahamas", 95000, ""),
("CMA CGM Rigoletto", "CMA CGM", "France", 99500, ""),
("Arnold M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 93496,
 "Captain <i>Morrell</i>"),
("Anna M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 93496,
 "Captain <i>Lockhart</i>"),
("Albert M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 93496,
 "Captain <i>Tallow</i>"),
("Adrian M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 93496,
 "Captain <i>G. E. Ericson</i>"),
("Arthur M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 93496, ""),
("Axel M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 93496, ""),
("NYK Vega", "Nippon Yusen Kaisha", "Panama", 97825, ""),
("MSC Esthi", "MSC", "Liberia", 99500, ""),
("MSC Chicago", "Offen Claus-Peter", "Liberia", 90449, ""),
("MSC Bruxelles", "Offen Claus-Peter", "Liberia", 90449, ""),
("MSC Roma", "Offen Claus-Peter", "Liberia", 99500, ""),
("MSC Madeleine", "MSC", "Liberia", 107551, ""),
("MSC Ines", "MSC", "Liberia", 107551, ""),
("Hannover Bridge", "Kawasaki Kisen Kaisha", "Japan", 99500, ""),
("Charlotte M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Clementine M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Columbine M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Cornelia M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Chicago Express", "Hapag-Lloyd", "Germany", 93750, ""),
("Kyoto Express", "Hapag-Lloyd", "Germany", 93750, ""),
("Clifford M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Sally M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Sine M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Skagen M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Sofie M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Sor\u00F8 M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Sovereing M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Susan M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Svend M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Svendborg M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("A.P. M\u00F8ller", "M\u00E6rsk Line", "Denmark", 91690,
 "Captain <i>Ferraby</i>"),
("Caroline M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Carsten M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Chastine M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("Cornelius M\u00E6rsk", "M\u00E6rsk Line", "Denmark", 91690, ""),
("CMA CGM Otello", "CMA CGM", "France", 91400, ""),
("CMA CGM Tosca", "CMA CGM", "France", 91400, ""),
("CMA CGM Nabucco", "CMA CGM", "France", 91400, ""),
("CMA CGM La Traviata", "CMA CGM", "France", 91400, ""),
("CSCL Europe", "Danaos Shipping", "Cyprus", 90645, ""),
("CSCL Africa", "Seaspan Container Line", "Cyprus", 90645, ""),
("CSCL America", "Danaos Shipping ", "Cyprus", 90645, ""),
("CSCL Asia", "Seaspan Container Line", "Hong Kong", 90645, ""),
("CSCL Oceania", "Seaspan Container Line", "Hong Kong", 90645,
 "Captain <i>Baker</i>"),
("M\u00E6rsk Seville", "Blue Star GmbH", "Liberia", 94724, ""),
("M\u00E6rsk Santana", "Blue Star GmbH", "Liberia", 94724, ""),
("M\u00E6rsk Sheerness", "Blue Star GmbH", "Liberia", 94724, ""),
("M\u00E6rsk Sarnia", "Blue Star GmbH", "Liberia", 94724, ""),
("M\u00E6rsk Sydney", "Blue Star GmbH", "Liberia", 94724, ""),
("MSC Heidi", "MSC", "Panama", 95000, ""),
("MSC Rania", "MSC", "Panama", 95000, ""),
("MSC Silvana", "MSC", "Panama", 95000, ""),
("M\u00E6rsk Stralsund", "Blue Star GmbH", "Liberia", 95000, ""),
("M\u00E6rsk Saigon", "Blue Star GmbH", "Liberia", 95000, ""),
("M\u00E6rsk Seoul", "Blue Star Ship Managment GmbH", "Germany",
 95000, ""),
("M\u00E6rsk Surabaya", "Offen Claus-Peter", "Germany", 98400, ""),
("CMA CGM Hugo", "NSB Niederelbe", "Germany", 90745, ""),
("CMA CGM Vivaldi", "CMA CGM", "Bahamas", 90745, ""),
("MSC Rachele", "NSB Niederelbe", "Germany", 90745, ""),
("Pacific Link", "NSB Niederelbe", "Germany", 90745, ""),
("CMA CGM Carmen", "E R Schiffahrt", "Liberia", 89800, ""),
("CMA CGM Don Carlos", "E R Schiffahrt", "Liberia", 89800, ""),
("CMA CGM Don Giovanni", "E R Schiffahrt", "Liberia", 89800, ""),
("CMA CGM Parsifal", "E R Schiffahrt", "Liberia", 89800, ""),
("Cosco China", "E R Schiffahrt", "Liberia", 91649, ""),
("Cosco Germany", "E R Schiffahrt", "Liberia", 89800, ""),
("Cosco Napoli", "E R Schiffahrt", "Liberia", 89800, ""),
("YM Unison", "Yang Ming Line", "Taiwan", 88600, ""),
("YM Utmost", "Yang Ming Line", "Taiwan", 88600, ""),
("MSC Lucy", "MSC", "Panama", 89954, ""),
("MSC Maeva", "MSC", "Panama", 89954, ""),
("MSC Rita", "MSC", "Panama", 89954, ""),
("MSC Busan", "Offen Claus-Peter", "Panama", 89954, ""),
("MSC Beijing", "Offen Claus-Peter", "Panama", 89954, ""),
("MSC Toronto", "Offen Claus-Peter", "Panama", 89954, ""),
("MSC Charleston", "Offen Claus-Peter", "Panama", 89954, ""),
("MSC Vittoria", "MSC", "Panama", 89954, ""),
("Ever Champion", "NSB Niederelbe", "Marshall Islands", 90449,
 "Captain <i>Phillips</i>"),
("Ever Charming", "NSB Niederelbe", "Marshall Islands", 90449,
 "Captain <i>Tonbridge</i>"),
("Ever Chivalry", "NSB Niederelbe", "Marshall Islands", 90449, ""),
("Ever Conquest", "NSB Niederelbe", "Marshall Islands", 90449, ""),
("Ital Contessa", "NSB Niederelbe", "Marshall Islands", 90449, ""),
("Lt Cortesia", "NSB Niederelbe", "Marshall Islands", 90449, ""),
("OOCL Asia", "OOCL", "Hong Kong", 89097, ""),
("OOCL Atlanta", "OOCL", "Hong Kong", 89000, ""),
("OOCL Europe", "OOCL", "Hong Kong", 89097, ""),
("OOCL Hamburg", "OOCL", "Marshall Islands", 89097, ""),
("OOCL Long Beach", "OOCL", "Marshall Islands", 89097, ""),
("OOCL Ningbo", "OOCL", "Marshall Islands", 89097, ""),
("OOCL Shenzhen", "OOCL", "Hong Kong", 89097, ""),
("OOCL Tianjin", "OOCL", "Marshall Islands", 89097, ""),
("OOCL Tokyo", "OOCL", "Hong Kong", 89097, "")):
        yield Ship(name, owner, country, teu, description)

