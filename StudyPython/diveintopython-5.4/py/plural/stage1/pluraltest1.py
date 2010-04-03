"""Unit test for plural1.py

This program is part of "Dive Into Python", a free Python book for
experienced programmers.  Visit http://diveintopython.org/ for the
latest version.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.2 $"
__date__ = "$Date: 2004/03/17 14:34:40 $"
__copyright__ = "Copyright (c) 2004 Mark Pilgrim"
__license__ = "Python"

from plural1 import plural
import unittest, new

class KnownValues(unittest.TestCase):
    nouns = {'bass': 'basses',
             'bus': 'buses',
             'walrus': 'walruses',
             'box': 'boxes',
             'fax': 'faxes',
             'suffix': 'suffixes',
             'mailbox': 'mailboxes',
             'buzz': 'buzzes',
             'waltz': 'waltzes',
             'coach': 'coaches',
             'glitch': 'glitches',
             'rash': 'rashes',
             'watch': 'watches',
             'cheetah': 'cheetahs',
             'cough': 'coughs',
             'utility': 'utilities',
             'vacancy': 'vacancies',
             'boy': 'boys',
             'day': 'days',
             'computer': 'computers',
             'rock': 'rocks',
             'paper': 'papers',
             }

for noun, pluralnoun in KnownValues.nouns.items():
    func = lambda self, noun=noun, pluralnoun=pluralnoun: \
           KnownValues.failUnlessEqual(self, plural(noun), pluralnoun)
    func.__doc__ = "%s --> %s" % (noun, pluralnoun)
    instanceMethod = new.instancemethod(func, None, KnownValues)
    setattr(KnownValues, "test_%s" % noun, instanceMethod)

if __name__ == "__main__":
    unittest.main()
