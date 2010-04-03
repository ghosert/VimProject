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

"""Provides the Length example class.
"""

class Length(object):
    """This class holds lengths.

    A length is an amount and a unit. Internally all lengths are held as
    meters, but lengths can be created using any common standard unit,
    and can be retrieved converted to any of the supported units.
    """

    convert = dict(mi=621.371e-6, miles=621.371e-6, mile=621.371e-6,
                   yd=1.094, yards=1.094, yard=1.094,
                   ft=3.281, feet=3.281, foot=3.281,
                   inches=39.37, inch=39.37,
                   mm=1000, millimeter=1000, millimeters=1000,
                   millimetre=1000, millimetres=1000,
                   cm=100, centimeter=100, centimeters=100,
                   centimetre=100, centimetres=100,
                   m=1.0, meter=1.0, meters=1.0, metre=1.0, metres=1.0,
                   km=0.001, kilometer=0.001, kilometers=0.001,
                   kilometre=0.001, kilometres=0.001)
    convert["in"] = 39.37
    numbers = frozenset("0123456789.eE")


    def __init__(self, length=None):
        """Initializes a Length with the given length

        The length is specified as an amount followed by a unit;
        intervening whitespace is allowed. The units that are understood
        are: millimeters, meters, kilometers, inches, feet, yards, and
        miles. They can be specified with their full names or with the
        standard abbreviations.

        Will raise ValueError if the amount cannot be interpreted as a
        floating point number or if the unit is missing or unrecognized.
        Will raise KeyError if the unit isn't valid.

        >>> x = Length("2 mi")
        >>> str(x)
        '3218.689m'
        >>> x
        Length('3218.688996m')
        >>> Length("3.5yd")
        Length('3.199269m')
        >>> Length("2 m")
        Length('2.000000m')
        >>> Length()
        Length('0.000000m')
        >>> Length(3) #doctest: +IGNORE_EXCEPTION_DETAIL
        Traceback (most recent call last):
        ...
        TypeError: iteration over non-sequence
        >>> Length("3")
        Traceback (most recent call last):
        ...
        ValueError: need an amount and a unit
        """
        if length is None:
            self.__amount = 0.0
        else:
            digits = ""
            for i, char in enumerate(length):
                if char in Length.numbers:
                    digits += char
                else:
                    self.__amount = float(digits)
                    unit = length[i:].strip().lower()
                    break
            else:
                raise ValueError, "need an amount and a unit"
            self.__amount /= Length.convert[unit]


    def set(self, length):
        """Sets the length to the new given length

        >>> x = Length("3m")
        >>> round(x.to("m"))
        3.0
        >>> x.set("39 in")
        >>> round(x.to("m"))
        1.0
        """
        self.__init__(length)


    def to(self, unit):
        """Returns the length as a float in the given unit.

        >>> x = Length("10mi")
        >>> round(x.to("km"), 3)
        16.093
        >>> x = Length("6 inches")
        >>> str(x)
        '0.152m'
        >>> round(x.to("mm"))
        152.0
        >>> round(x.to("ft"), 3)
        0.5
        >>> round(Length("1m"))
        1.0
        """
        return self.__amount * Length.convert[unit]


    def copy(self):
        """Returns a unique copy of the length

        >>> x = Length("2m")
        >>> y = x
        >>> x, y
        (Length('2.000000m'), Length('2.000000m'))
        >>> x.set("1m")
        >>> x, y
        (Length('1.000000m'), Length('1.000000m'))
        >>> y = x.copy()
        >>> x, y
        (Length('1.000000m'), Length('1.000000m'))
        >>> y.set("0.5m")
        >>> x, y
        (Length('1.000000m'), Length('0.500000m'))
        """
        other = Length()
        other.__amount = self.__amount
        return other


    @staticmethod
    def units():
        return Length.convert.keys()


    def __hash__(self):
        return super(Length, self).__hash__()


    def __eq__(self, other):
        """
        >>> x = Length("2m")
        >>> y = Length("3m")
        >>> x == y
        False
        >>> x < y
        True
        >>> x > y
        False
        >>> x = y
        >>> x == y
        True
        """
        return self.__amount == other.__amount


    def __lt__(self, other):
        """
        >>> x = Length("2m")
        >>> y = Length("3m")
        >>> x == y
        False
        >>> x < y
        True
        >>> x > y
        False
        >>> x = y
        >>> x == y
        True
        """
        return self.__amount < other.__amount


    def __repr__(self):
        """
        >>> repr(Length("2.5km"))
        "Length('2500.000000m')"
        """
        return "Length('{0:.6f}m')".format(self.__amount)


    def __str__(self):
        """
        >>> str(Length("200 mm"))
        '0.200m'
        """
        return "{0:.3f}m".format(self.__amount)


    def __add__(self, other):
        """
        >>> x = Length("10mi")
        >>> y = Length("10KM")
        >>> x = x + y
        >>> x, str(x), round(float(x), 3)
        (Length('26093.444979m'), '26093.445m', 26093.445)
        >>> x + 5
        Traceback (most recent call last):
        ...
        AttributeError: 'int' object has no attribute '_Length__amount'
        """
        return Length("{0:f}m".format((self.__amount + other.__amount)))


    def __iadd__(self, other):
        """
        >>> x = Length("3.5mi")
        >>> y = Length("200m")
        >>> x += y
        >>> x
        Length('5832.705743m')
        >>> x += 5
        Traceback (most recent call last):
        ...
        AttributeError: 'int' object has no attribute '_Length__amount'
        """
        self.__amount += other.__amount
        return self


    def __sub__(self, other):
        """
        >>> Length("1km") - Length("100m")
        Length('900.000000m')
        >>> Length("1m") + 5
        Traceback (most recent call last):
        ...
        AttributeError: 'int' object has no attribute '_Length__amount'
        """
        return Length("{0:f}m".format(self.__amount - other.__amount))


    def __isub__(self, other):
        """
        >>> x = Length("1km")
        >>> x -= Length("150m")
        >>> x
        Length('850.000000m')
        >>> x -= 78
        Traceback (most recent call last):
        ...
        AttributeError: 'int' object has no attribute '_Length__amount'
        """
        self.__amount -= other.__amount
        return self


    def __mul__(self, other):
        """
        >>> x = Length("350m")
        >>> x * 3
        Length('1050.000000m')
        >>> y = Length("40ft")
        >>> x * y
        Traceback (most recent call last):
        ...
        ValueError: Length * Length produces an area not a Length
        """
        if isinstance(other, Length):
            raise ValueError, \
                   "Length * Length produces an area not a Length"
        return Length("{0:f}m".format(self.__amount * other))


    def __rmul__(self, other):
        """
        >>> x = Length("350m")
        >>> 3 * x
        Length('1050.000000m')
        """
        return Length("{0:f}m".format(other * self.__amount))


    def __imul__(self, other):
        """
        >>> x = Length("350m")
        >>> x *= 3
        >>> x
        Length('1050.000000m')
        """
        self.__amount *= other
        return self


    def __truediv__(self, other):
        """
        >>> x = Length("360m")
        >>> x / 3
        Length('120.000000m')
        >>> y = Length("1m")
        >>> x / y
        Traceback (most recent call last):
        ...
        TypeError: unsupported operand type(s) for /: 'float' and 'Length'
        """
        return Length("{0:f}m".format(self.__amount / other))


    def __itruediv__(self, other):
        """
        >>> x = Length("360m")
        >>> x /= 3
        >>> x
        Length('120.000000m')
        >>> y = Length("1m")
        >>> x /= y
        Traceback (most recent call last):
        ...
        TypeError: unsupported operand type(s) for /=: 'float' and 'Length'
        """
        self.__amount /= other
        return self


    def __float__(self):
        """
        >>> float(Length("10m"))
        10.0
        """
        return self.__amount


    def __int__(self):
        """
        >>> int(Length("10.499m"))
        10
        """
        return int(round(self.__amount))


if __name__ == "__main__":
    import doctest
    doctest.testmod()
