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
from __future__ import unicode_literals
from future_builtins import *

"""Provides two simple range functions for floating point numbers.
"""


def frange(arg0, arg1=None, arg2=None):
    """Returns a list of floats using range-like syntax

    frange(start, stop, inc)   # start = arg0  stop = arg1  inc = arg2
    frange(start, stop)        # start = arg0  stop = arg1  inc = 1.0
    frange(stop)               # start = 0.0   stop = arg0  inc = 1.0
    
    >>> frange(5)
    [0.0, 1.0, 2.0, 3.0, 4.0]
    >>> frange(5, 10)
    [5, 6.0, 7.0, 8.0, 9.0]
    >>> frange(2, 5, 0.5)
    [2, 2.5, 3.0, 3.5, 4.0, 4.5]
    """
    start = 0.0
    inc = 1.0
    if arg2 is not None:    # 3 arguments given
        start = arg0
        stop = arg1
        inc = arg2
    elif arg1 is not None:  # 2 arguments given
        start = arg0
        stop = arg1
    else:                   # 1 argument given
        stop = arg0
    # Build and return a list
    result = []
    while start < (stop - (inc / 2.0)):
        result.append(start)
        start += inc
    return result


def gfrange(arg0, arg1=None, arg2=None):
    """Returns a generator of floats using range-like syntax

    gfrange(start, stop, inc)
    gfrange(start, stop) # inc == 1.0
    gfrange(stop) # start = 0.0, inc == 1.0

    >>> list(gfrange(5))
    [0.0, 1.0, 2.0, 3.0, 4.0]
    >>> list(gfrange(5, 10))
    [5, 6.0, 7.0, 8.0, 9.0]
    >>> list(gfrange(2, 5, 0.5))
    [2, 2.5, 3.0, 3.5, 4.0, 4.5]
    """
    start = 0.0
    inc = 1.0
    if arg2 is not None:    # 3 arguments given
        start = arg0
        stop = arg1
        inc = arg2
    elif arg1 is not None:  # 2 arguments given
        start = arg0
        stop = arg1
    else:                   # 1 argument given
        stop = arg0
    # Return each value on demand
    while start < (stop - (inc / 2.0)):
        yield start
        start += inc


if __name__ == "__main__":
    import doctest
    doctest.testmod()

