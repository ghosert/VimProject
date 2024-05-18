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

"""Provides the Item example classes.
"""

class Item(object):

    def __init__(self, artist, title, year=None):
        self.__artist = artist
        self.__title = title
        self.__year = year


    def artist(self):
        return self.__artist


    def setArtist(self, artist):
        self.__artist = artist


    def title(self):
        return self.__title


    def setTitle(self, title):
        self.__title = title


    def year(self):
        return self.__year


    def setYear(self, year):
        self.__year = year


    def __str__(self):
        year = ""
        if self.__year is not None:
            year = " in {0}".format(self.__year)
        return "{0} by {1}{2}".format(self.__title, self.__artist, year)


class Painting(Item):

    def __init__(self, artist, title, year=None):
        super(Painting, self).__init__(artist, title, year)


class Sculpture(Item):

    def __init__(self, artist, title, year=None, material=None):
        super(Sculpture, self).__init__(artist, title, year)
        self.__material = material


    def material(self):
        return self.__material


    def setMaterial(self, material):
        self.__material = material


    def __str__(self):
        materialString = ""
        if self.__material is not None:
            materialString = " ({0})".format(self.__material)
        return "{0}{1}".format(super(Sculpture, self).__str__(),
                               materialString)


class Dimension(object):

    def __init__(self, width, height, depth=None):
        self.__width = width
        self.__height = height
        self.__depth = depth


    def width(self):
        return self.__width


    def setWidth(self, width):
        self.__width = width


    def height(self):
        return self.__height


    def setHeight(self, height):
        self.__height = height


    def depth(self):
        return self.__depth


    def setDepth(self, depth):
        self.__depth = depth


    def area(self):
        raise NotImplemented


    def volume(self):
        raise NotImplemented



if __name__ == "__main__":
    items = []
    items.append(Painting("Cecil Collins", "The Poet", 1941))
    items.append(Painting("Cecil Collins", "The Sleeping Fool", 1943))
    items.append(Painting("Edvard Munch", "The Scream", 1893))
    items.append(Painting("Edvard Munch", "The Sick Child", 1896))
    items.append(Painting("Edvard Munch", "The Dance of Life", 1900))
    items.append(Sculpture("Auguste Rodin", "Eternal Springtime", 1917,
                           "plaster"))
    items.append(Sculpture("Auguste Rodin", "Naked Balzac", 1917,
                           "plaster"))
    items.append(Sculpture("Auguste Rodin", "The Secret", 1925,
                           "bronze"))
    uniquematerials = set()
    for item in items:
        print(item)
        if hasattr(item, "material"):
            uniquematerials.add(item.material())
    print("Sculptures use {0} unique materials".format(
          len(uniquematerials)))

