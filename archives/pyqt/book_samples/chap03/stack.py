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
#from __future__ import unicode_literals # confuses doctest
from future_builtins import *


class EmptyStackError(Exception): pass


class Stack(object):

    def __init__(self):
        """A stack class that supports pop(), top(), and push(), and
        special methods to support str() and len()
        
        >>> s = Stack()
        >>> print(s)
        []
        """
        self.__items = []


    def pop(self):
        """Returns and removes the stack's most recent (right-most) item

        >>> s = Stack()
        >>> s.push(1)
        >>> s.push(2)
        >>> print(s)
        [1, 2]
        >>> s.pop()
        2
        >>> s.pop()
        1
        >>> s.pop()
        Traceback (most recent call last):
        ...
        EmptyStackError
        """
        if not self.__items:
            raise EmptyStackError
        return self.__items.pop()


    def top(self):
        """Returns the stack's most recent (right-most) item

        >>> s = Stack()
        >>> s.push(1)
        >>> s.push(2)
        >>> s.top()
        2
        >>> s.pop()
        2
        >>> s.top()
        1
        """
        if not self.__items:
            raise EmptyStackError
        return self.__items[-1]


    def push(self, item):
        """Pushes the given item onto (right-end of) the stack

        >>> s = Stack()
        >>> s.push(1)
        >>> s.push(2)
        >>> print(s)
        [1, 2]
        """
        self.__items.append(item)


    def __len__(self):
        """Returns the number of items in the stack

        >>> s = Stack()
        >>> len(s)
        0
        >>> s.push(1)
        >>> s.push(2)
        >>> s.push("X")
        >>> len(s)
        3
        """
        return len(self.__items)


    def __str__(self):
        """Returns a string representation of the stack's contents from
        bottom to top

        >>> s = Stack()
        >>> s.push(1)
        >>> s.push(2)
        >>> s.push("X")
        >>> print(s)
        [1, 2, 'X']
        """
        return "[{0}]".format(", ".join(["{0!r}".format(x)
                                         for x in self.__items]))
        # Alternatively:
        # pieces = []
        # for x in self.__items:
        #     pieces.append("{0!r}".format(x))
        # return "[{0}]".format(", ".join(pieces))



if __name__ == "__main__":
    import doctest
    doctest.testmod()

