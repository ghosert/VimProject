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


class Tribool(object):

    def __init__(self, value=None):
        """Creates a Tribool object whose value is True, False, or None

        None is used to signify "Unknown" or "Null".
        We use non-propagating logic, i.e., even if an operand is None,
        if we can determine the logical outcome based on the other
        operand we do so, rather than simply returning None.

        >>> a = Tribool()
        >>> print(a)
        None
        >>> b = Tribool(True)
        >>> print(b)
        True
        >>> c = Tribool(False)
        >>> print(c)
        False
        """
        self.__value = None
        if value is not None:
            self.__value = bool(value)


    def __str__(self):
        return str(self.__value)


    def __repr__(self):
        return "Tribool({0})".format(self.__value)


    def __hash__(self):
        return super(Tribool, self).__hash__()


    def __eq__(self, other):
        """
        >>> a = Tribool()
        >>> b = Tribool(True)
        >>> c = Tribool(False)
        >>> a < b
        True
        >>> a < c
        True
        >>> a < c < b
        True
        >>> b == Tribool(True)
        True
        """
        return self.__value == other.__value


    def __lt__(self, other):
        """
        >>> a = Tribool()
        >>> b = Tribool(True)
        >>> c = Tribool(False)
        >>> a < b
        True
        >>> a < c
        True
        >>> a < c < b
        True
        >>> b == Tribool(True)
        True
        """
        selfValue = self.__value
        otherValue = other.__value
        if selfValue is None:
            selfValue = -1
        if otherValue is None:
            otherValue = -1
        return selfValue < otherValue


    def __nonzero__(self):
        """
        >>> a = Tribool()
        >>> bool(a)
        False
        >>> b = Tribool(True)
        >>> bool(b)
        True
        >>> c = Tribool(False)
        >>> bool(c)
        False
        """
        return self.__value == True


    def __invert__(self):
        """
        >>> a = Tribool()
        >>> print(~a)
        None
        >>> b = Tribool(True)
        >>> print(~b)
        False
        >>> c = Tribool(False)
        >>> print(~c)
        True
        """
        if self.__value is not None:
            return not self.__value
        return None


    def __and__(self, other):
        """
        >>> a = Tribool()
        >>> b = Tribool(True)
        >>> c = Tribool(False)
        >>> a & b
        Tribool(None)
        >>> a & c
        Tribool(False)
        >>> b & c
        Tribool(False)
        >>> b & Tribool(True)
        Tribool(True)
        >>> print(a & a)
        None
        >>> print(b & b)
        True
        >>> print(c & c)
        False
        """
        if self.__value == True and other.__value == True:
            return Tribool(True)
        if self.__value == False or other.__value == False:
            return Tribool(False)
        return Tribool()


    def __or__(self, other):
        """
        >>> a = Tribool()
        >>> b = Tribool(True)
        >>> c = Tribool(False)
        >>> a | b
        Tribool(True)
        >>> a | c
        Tribool(None)
        >>> b | c
        Tribool(True)
        >>> a | Tribool()
        Tribool(None)
        >>> print(a | a)
        None
        >>> print(b | b)
        True
        >>> print(c | c)
        False
        """
        if self.__value == True or other.__value == True:
            return Tribool(True)
        if self.__value == False and other.__value == False:
            return Tribool(False)
        return Tribool()


if __name__ == "__main__":
    import doctest
    doctest.testmod()
