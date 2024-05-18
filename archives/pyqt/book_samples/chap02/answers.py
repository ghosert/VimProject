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

"""Answers to the exercises in chapter 2.
"""

def valid(text, chars="ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"):
    """Returns a copy of text than only contains characters in chars

    This function is case-sensitive.
    
    >>> valid("Barking")
    'B'
    >>> valid("KL754", "0123456789")
    '754'
    >>> valid("BEAN", "abcdefghijklmnopqrstuvwxyz")
    ''
    """
    result = []
    for char in text:
        if char in chars:
            result.append(char)
    return "".join(result)


def charcount(text):
    """Returns a dictionary with character counts for each letter in text

    The text is lowercased. Any whitespace character's count is given
    the "whitespace" key, any non-whitespace, non-A-Z character's is
    given the "others" key; the letter keys are lowercase, i.e., "a",
    "b", ..., "z".

    >>> stats = {}
    >>> for char in "abcdefghijklmnopqrstuvwxyz": stats[char] = 0
    >>> stats["whitespace"] = 0
    >>> stats["others"] = 0
    >>> charcount("") == stats
    True
    >>> stats["a"] = 1
    >>> stats["b"] = 2
    >>> stats["l"] = 1
    >>> stats["e"] = 2
    >>> stats["whitespace"] = 1
    >>> stats["o"] = 1
    >>> stats["d"] = 2
    >>> stats["i"] = 1
    >>> charcount("Able bodied") == stats
    True
    >>> for char in "abcdefghijklmnopqrstuvwxyz": stats[char] = 0
    >>> stats["e"] = 5
    >>> stats["x"] = 1
    >>> stats["c"] = 1
    >>> stats["d"] = 2
    >>> stats["i"] = 2
    >>> stats["n"] = 1
    >>> stats["g"] = 1
    >>> stats["l"] = 2
    >>> stats["y"] = 1
    >>> stats["whitespace"] = 1
    >>> stats["b"] = 1
    >>> charcount("Exceedingly Edible") == stats
    True
    """
    stats = {} # Same as: stats = dict()
    for char in "abcdefghijklmnopqrstuvwxyz":
        stats[char] = 0
    stats["whitespace"] = 0
    stats["others"] = 0
    for char in text.lower():
        if char in stats:
            stats[char] += 1
        elif char.isspace():
            stats["whitespace"] += 1
        else:
            stats["others"] += 1
    return stats


def integer(number):
    """Returns an integer whatever the input value
    
    If the input is invalid returns 0

    >>> integer("tonsils")
    0
    >>> integer(4.5)
    5
    >>> integer(32)
    32
    >>> integer("-15.1")
    -15
    >>> integer(22.499999999)
    22
    >>> integer("22.5")
    23
    >>> integer("-red")
    0
    """
    try:
        x = int(round(float(number)))
    except ValueError:
        x = 0
    return x


def incrementString(text="AAAA"):
    """Returns the text incremented by one letter

    The text must be alphabetic or a ValueError is raised.
    
    >>> incrementString("A")
    'B'
    >>> incrementString("Z")
    'AA'
    >>> incrementString("AM")
    'AN'
    >>> incrementString("AZ")
    'BA'
    >>> incrementString("BA")
    'BB'
    >>> incrementString("BZ")
    'CA'
    >>> incrementString("ZZA")
    'ZZB'
    >>> incrementString("ZZZ")
    'AAAA'
    >>> incrementString("AAAA")
    'AAAB'
    >>> incrementString("AAAZ")
    'AABA'
    >>> incrementString("ABC2")
    Traceback (most recent call last):
    ValueError: text must be purely alphabetic
    """
    if not text.isalpha():
        raise ValueError, "text must be purely alphabetic"
    OrdA = ord("A")
    OrdZ = ord("Z")
    changed = False
    values = [ord(c) for c in reversed(text.upper())]
    for i in range(len(values)):
        if values[i] < OrdZ:
            values[i] += 1
            changed = True
            break
        elif values[i] == OrdZ:
            values[i] = OrdA
    if not changed:
        values = [OrdA] + values
    return "".join([chr(v) for v in reversed(values)])


def leapyears(yearlist):
    """Returns a generator that returns the leap years in yearlist

    >>> years = [1600, 1604, 1700, 1704, 1800, 1900, 1996, 2000, 2004]
    >>> list(leapyears(years))
    [1600, 1604, 1704, 1996, 2000, 2004]
    """
    for year in yearlist:
        if year % 4 == 0:
            if year % 100 == 0 and not year % 400 == 0:
                continue
            yield year


if __name__ == "__main__":
    import doctest
    doctest.testmod()

