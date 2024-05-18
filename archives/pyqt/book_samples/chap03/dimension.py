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

"""Provides the Dimension example classes.
"""

class Dimension(object):

    def area(self):
        raise NotImplementedError, "Dimension.area()"


    def volume(self):
        raise NotImplementedError, "Dimension.volume()"



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


class Painting(Item, Dimension):

    def __init__(self, artist, title, year=None, width=None,
                 height=None):
        super(Painting, self).__init__(artist, title, year)
        self.__width = width
        self.__height = height


    def area(self):
        if self.__width is None or self.__height is None:
            return None
        return self.__width * self.__height


    def volume(self):
        return None


class Sculpture(Item, Dimension):

    def __init__(self, artist, title, year=None, material=None):
        super(Sculpture, self).__init__(artist, title, year)
        self.__material = material


    def material(self):
        return self.__material


    def setMaterial(self, material):
        self.__material = material


    def __str__(self):
        material = ""
        if self.__material is not None:
            material = " ({0})".format(self.__material)
        return "{1}{2}".format(super(Sculpture, self).__str__(), material)


if __name__ == "__main__":
    painting = Painting("Cecil Collins", "The Poet", 1941, 298, 400)
    print(painting.area(), painting.volume())
    sculpture = Sculpture("Auguste Rodin", "The Secret", 1925, "bronze")
    try:
        print(sculpture.area(), sculpture.volume())
    except NotImplementedError:
        print("Ooops, we forgot to implement area() and volume() for" + 
              " Sculptures")

