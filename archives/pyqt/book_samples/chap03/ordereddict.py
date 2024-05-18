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

import bisect

"""Provides the OrderedDict example class.

WARNING: This class is wrongly named, it should be called SortedDict.
A correctly named version is included in this directory, and a more
sophisticated version is available from
http://pypi.python.org/pypi/sorteddict
"""



class OrderedDict(object):
    """A dictionary that is ordered by key
    
    Initializing with a dictionary is expensive because all the
    dictionary's keys must be sorted. This is also true of the update()
    method.
    """

    def __init__(self, dictionary=None):
        """Initializes with a shallow copy of the given dictionary

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.items()
        [('a', 2), ('i', 4), ('n', 3), ('s', 1), ('t', 5), ('y', 6)]
        >>> OrderedDict()
        OrderedDict({})
        >>> e = OrderedDict(d)
        >>> e.items()
        [('a', 2), ('i', 4), ('n', 3), ('s', 1), ('t', 5), ('y', 6)]
        """
        self.__keys = []
        self.__dict = {}
        if dictionary is not None:
            if isinstance(dictionary, OrderedDict):
                self.__dict = dictionary.__dict.copy()
                self.__keys = dictionary.__keys[:]
            else:
                self.__dict = dict(dictionary).copy()
                self.__keys = sorted(self.__dict.keys())


    def update(self, dictionary=None, **kwargs):
        """Updates this dictionary with another dictionary and/or with
        keyword key=value pairs


        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5))
        >>> d.update(dict(a=4, z=-4))
        >>> d.items()
        [('a', 4), ('i', 4), ('n', 3), ('s', 1), ('t', 5), ('z', -4)]
        >>> del d["a"]
        >>> del d["i"]
        >>> d.update({'g': 9}, a=1, z=3)
        >>> d.items()
        [('a', 1), ('g', 9), ('n', 3), ('s', 1), ('t', 5), ('z', 3)]
        >>> e = OrderedDict(dict(p=4, q=5))
        >>> del d["a"]
        >>> del d["n"]
        >>> e.update(d)
        >>> e.items()
        [('g', 9), ('p', 4), ('q', 5), ('s', 1), ('t', 5), ('z', 3)]
        """
        if dictionary is None:
            pass
        elif isinstance(dictionary, OrderedDict):
            self.__dict.update(dictionary.__dict)
        elif (isinstance(dictionary, dict) or 
              not hasattr(dictionary, "items")):
            self.__dict.update(dictionary)
        else:
            for key, value in dictionary.items():
                self.__dict[key] = value
        if kwargs:
            self.__dict.update(kwargs)
        self.__keys = sorted(self.__dict.keys())


    @classmethod
    def fromkeys(cls, iterable, value=None):
        """A class method that returns an OrderedDict whose keys are
        from the iterable and each of whose values is value

        >>> d = OrderedDict()
        >>> e = d.fromkeys("KYLIE", 21)
        >>> e.items()
        [('E', 21), ('I', 21), ('K', 21), ('L', 21), ('Y', 21)]
        >>> e = OrderedDict.fromkeys("KYLIE", 21)
        >>> e.items()
        [('E', 21), ('I', 21), ('K', 21), ('L', 21), ('Y', 21)]
        """
        dictionary = cls()
        for key in iterable:
            dictionary[key] = value
        return dictionary


    def getAt(self, index):
        """Returns the index-th item's value

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.getAt(0)
        2
        >>> d.getAt(5)
        6
        >>> d.getAt(2)
        3
        >>> d.getAt(19)
        Traceback (most recent call last):
        ...
        IndexError: list index out of range
        """
        return self.__dict[self.__keys[index]]


    def setAt(self, index, value):
        """Sets the index-th item's value to the given value

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.getAt(5)
        6
        >>> d.setAt(5, 99)
        >>> d.getAt(5)
        99
        >>> d.setAt(19, 42)
        Traceback (most recent call last):
        ...
        IndexError: list index out of range
        """
        self.__dict[self.__keys[index]] = value


    def copy(self):
        """Returns a shallow copy of this OrderedDict

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> e = d.copy()
        >>> e.items()
        [('a', 2), ('i', 4), ('n', 3), ('s', 1), ('t', 5), ('y', 6)]
        """
        dictionary = OrderedDict()
        dictionary.__keys = self.__keys[:]
        dictionary.__dict = self.__dict.copy()
        return dictionary


    def clear(self):
        """Removes every item from this OrderedDict
        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> len(d)
        6
        >>> d.clear()
        >>> len(d)
        0
        >>> d["m"] = 3
        >>> d["a"] = 5
        >>> d["z"] = 7
        >>> d["e"] = 9
        >>> d.keys()
        ['a', 'e', 'm', 'z']
        """
        self.__keys = []
        self.__dict = {}


    def get(self, key, value=None):
        """Returns the value associated with key or value if key isn't
        in the dictionary

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.get("X", 21)
        21
        >>> d.get("i")
        4
        """
        return self.__dict.get(key, value)


    def setdefault(self, key, value):
        """If key is in the dictionary, returns its value;
        otherwise adds the key with the given value which is also
        returned

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.setdefault("n", 99)
        3
        >>> d.values()
        [2, 4, 3, 1, 5, 6]
        >>> d.setdefault("r", -20)
        -20
        >>> d.items()[2:]
        [('n', 3), ('r', -20), ('s', 1), ('t', 5), ('y', 6)]
        >>> d.setdefault("@", -11)
        -11
        >>> d.setdefault("z", 99)
        99
        >>> d.setdefault("m", 50)
        50
        >>> d.keys()
        ['@', 'a', 'i', 'm', 'n', 'r', 's', 't', 'y', 'z']
        """
        if key not in self.__dict:
            bisect.insort_left(self.__keys, key)
        return self.__dict.setdefault(key, value)


    def pop(self, key, value=None):
        """If key is in the dictionary, returns its value and removes it
        from the dictionary; otherwise returns the given value

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.pop("n")
        3
        >>> "n" in d
        False
        >>> d.pop("q", 41)
        41
        >>> d.keys()
        ['a', 'i', 's', 't', 'y']
        >>> d.pop("a")
        2
        >>> d.pop("t")
        5
        >>> d.keys()
        ['i', 's', 'y']
        """
        if key not in self.__dict:
            return value
        i = bisect.bisect_left(self.__keys, key)
        del self.__keys[i]
        return self.__dict.pop(key, value)


    def popitem(self):
        """Returns and removes an arbitrary item from the dictionary

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> len(d)
        6
        >>> item = d.popitem()
        >>> item = d.popitem()
        >>> item = d.popitem()
        >>> len(d)
        3
        """
        item = self.__dict.popitem()
        i = bisect.bisect_left(self.__keys, item[0])
        del self.__keys[i]
        return item


    def keys(self):
        """Returns the dictionary's keys in key order

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.keys()
        ['a', 'i', 'n', 's', 't', 'y']
        """
        return self.__keys[:]


    def values(self):
        """Returns the dictionary's values in key order

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.values()
        [2, 4, 3, 1, 5, 6]
        """
        return [self.__dict[key] for key in self.__keys]


    def items(self):
        """Returns the dictionary's items in key order

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.items()
        [('a', 2), ('i', 4), ('n', 3), ('s', 1), ('t', 5), ('y', 6)]
        """
        return [(key, self.__dict[key]) for key in self.__keys]


    def __iter__(self):
        """Returns an iterator over the dictionary's keys

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> list(d)
        ['a', 'i', 'n', 's', 't', 'y']
        """
        return iter(self.__keys)


    def iterkeys(self):
        """Returns an iterator over the dictionary's keys

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> list(d)
        ['a', 'i', 'n', 's', 't', 'y']
        """
        return iter(self.__keys)


    def itervalues(self):
        """Returns an iterator over the dictionary's values in key order

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> list(d.itervalues())
        [2, 4, 3, 1, 5, 6]
        """
        for key in self.__keys:
            yield self.__dict[key]


    def iteritems(self):
        """Returns an iterator over the dictionary's values in key order

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> list(d.iteritems())
        [('a', 2), ('i', 4), ('n', 3), ('s', 1), ('t', 5), ('y', 6)]
        """
        for key in self.__keys:
            yield key, self.__dict[key]


    def has_key(self, key):
        """Returns True if key is in the dictionary; otherwise returns
        False. Use in instead.

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d.has_key("a")
        True
        >>> d.has_key("x")
        False
        """
        return key in self.__dict


    def __contains__(self, key):
        """Returns True if key is in the dictionary; otherwise returns
        False

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> "a" in d
        True
        >>> "x" in d
        False
        """
        return key in self.__dict


    def __len__(self):
        """Returns the number of items in the dictionary

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> len(d)
        6
        >>> del d["n"]
        >>> del d["y"]
        >>> len(d)
        4
        >>> d.clear()
        >>> len(d)
        0
        """
        return len(self.__dict)


    def __delitem__(self, key):
        """Deletes the item with the given key from the dictionary

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> del d["s"]
        >>> del d["y"]
        >>> del d["a"]
        >>> d.keys()
        ['i', 'n', 't']
        """
        i = bisect.bisect_left(self.__keys, key)
        del self.__keys[i]
        del self.__dict[key]


    def __getitem__(self, key):
        """Returns the value of the item with the given key

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d["i"]
        4
        >>> d["y"]
        6
        >>> d["z"]
        Traceback (most recent call last):
        ...
        KeyError: 'z'
        """
        return self.__dict[key]


    def __setitem__(self, key, value):
        """If key is in the dictionary, sets its value to value;
        otherwise adds the key to the dictionary with the given value

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5, y=6))
        >>> d["t"] = -17
        >>> d["z"] = 43
        >>> d["@"] = -11
        >>> d["m"] = 22
        >>> d["r"] = 5
        >>> d.keys()
        ['@', 'a', 'i', 'm', 'n', 'r', 's', 't', 'y', 'z']
        """
        if key not in self.__dict:
            bisect.insort_left(self.__keys, key)
        self.__dict[key] = value


    def __repr__(self):
        """Returns an eval()-able string representation of the
        dictionary

        >>> d = OrderedDict(dict(s=1, a=2, n=3, i=4, t=5))
        >>> repr(d)
        "OrderedDict({'a': 2, 'i': 4, 'n': 3, 's': 1, 't': 5})"
        >>> d = OrderedDict({2: 'a', 3: 'm', 1: 'x'})
        >>> repr(d)
        "OrderedDict({1: 'x', 2: 'a', 3: 'm'})"

        Alternative implementation using a list comprehension:

        return "OrderedDict({{{0}}})".format(", ".join(
               ["{0!r}: {1!r}".format((key, self.__dict[key])) \
                for key in self.__keys]))
        """
        pieces = []
        for key in self.__keys:
            pieces.append("{0!r}: {1!r}".format(key, self.__dict[key]))
        return "OrderedDict({{{0}}})".format(", ".join(pieces))



if __name__ == "__main__":
    import doctest
    doctest.testmod()

