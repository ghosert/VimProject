#!/usr/bin/env python
# Copyright (c) 2007-8 Qtrac Ltd. All rights reserved.
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
#from __future__ import unicode_literals # confuses doctest
from future_builtins import *

"""Provides two simple simplify functions for strings.
"""


def simplify(text, space=" \t\r\n\f", delete=""):
    """Returns the text with multiple spaces reduced to single spaces

    The space parameter is a string of characters each of which is
    considered to be a space.
    Any characters in delete are excluded from the resultant string.

    >>> simplify(" this    and\\n that\\t too")
    'this and that too'
    >>> simplify("  Washington   D.C.\\n")
    'Washington D.C.'
    >>> simplify("  Washington   D.C.\\n", delete=",;:.")
    'Washington DC'
    >>> simplify(" disemvoweled ", delete="aeiou")
    'dsmvwld'
    """
    result = []
    word = ""
    for char in text:
        if char in delete:
            continue
        elif char in space:
            if word:
                result.append(word)
                word = ""
        else:
            word += char
    if word:
        result.append(word)
    return " ".join(result)


def simplified(text, delete=""):
    """Returns text with multiple whitespace reduced to single spaces

    Any characters in delete are excluded from the resultant string.

    >>> simplified(" this    and\\n that\\t too")
    'this and that too'
    >>> simplified("  Washington   D.C.\\n")
    'Washington D.C.'
    >>> simplified("  Washington   D.C.\\n", delete=",;:.")
    'Washington DC'
    >>> simplified(" disemvoweled ", delete="aeiou")
    'dsmvwld'
    """
    result = []
    word = []
    for char in text:
        if char in delete:
            continue
        elif char.isspace():
            if word:
                result.append("".join(word))
                word = []
        else:
            word.append(char)
    if word:
        result.append("".join(word))
    return " ".join(result)


if __name__ == "__main__":
    import doctest
    doctest.testmod()

