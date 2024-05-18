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

"""Provides the Rectangle example classes.
"""

class Rectangle(object):

    def __init__(self, width, height):
        self.width = width
        self.height = height

    def getWidth(self):
        return self.width

    def setWidth(self, width):
        self.width = width

    def getHeight(self):
        return self.height

    def setHeight(self, height):
        self.height = height

    def area(self):
        return self.getWidth() * self.getHeight()

    def __hash__(self):
        return super(Rectangle, self).__hash__()

    def __eq__(self, other):
        return self.area() == other.area()

    def __lt__(self, other):
        return self.area() < other.area()

    def __nonzero__(self):
        return self.width or self.height

    def __repr__(self):
        return "Rectangle({0}, {1})".format(self.width, self.height)


class Rectangle(object):

    def __init__(self, width, height):
        self.width = width
        self.height = height

    def _area(self):
        return self.width * self.height
    area = property(fget=_area)

    def __hash__(self):
        return super(Rectangle, self).__hash__()

    def __eq__(self, other):
        return self.area == other.area

    def __lt__(self, other):
        return self.area < other.area

    def __nonzero__(self):
        return self.width or self.height

    def __repr__(self):
        return "Rectangle({0}, {1})".format(self.width, self.height)


class Rectangle(object):

    def __init__(self, width, height):
        self.__width = width
        self.__height = height

    def _area(self):
        return self.__width * self.__height
    area = property(fget=_area)

    def _height(self):
        return self.__height

    def _setHeight(self, height):
        # Perform computation
        self.__height = height

    height = property(fget=_height, fset=_setHeight)

    def _width(self):
        return self.__width

    def _setWidth(self, width):
        # Perform computation
        self.__width = width

    width = property(fget=_width, fset=_setWidth)

    def __hash__(self):
        return super(Rectangle, self).__hash__()

    def __eq__(self, other):
        return self.area == other.area

    def __lt__(self, other):
        return self.area < other.area

    def __nonzero__(self):
        return self.__width or self.__height

    def __repr__(self):
        return "Rectangle({0}, {1})".format(self.__width, self.__height)

