"""Unit test for plural6.py

This program is part of "Dive Into Python", a free Python book for
experienced programmers.  Visit http://diveintopython.org/ for the
latest version.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2004/03/17 14:34:40 $"
__copyright__ = "Copyright (c) 2004 Mark Pilgrim"
__license__ = "Python"

from plural6 import plural
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
             'soliloquy': 'soliloquies',
             'boy': 'boys',
             'day': 'days',
             'computer': 'computers',
             'rock': 'rocks',
             'paper': 'papers',
             
             'mouse': 'mice',
             'louse': 'lice',
             'child': 'children',
             'foot': 'feet',
             'booth': 'booths',
             'tooth': 'teeth',
             'leaf': 'leaves',
             'loaf': 'loaves',
             'thesis': 'theses',
             'man': 'men',
             'mailman': 'mailmen',
             'knife': 'knives',
             'wife': 'wives',
             'tableau': 'tableaux',
             'elf': 'elves',
             'shelf': 'shelves',
             }

for noun, correctPlural in KnownValues.nouns.items():
    testMethod = lambda self: KnownValues.failUnlessEqual(self, plural(noun), correctPlural)
    testMethod.__doc__ = "%s --> %s" % (noun, correctPlural)
    instanceMethod = new.instancemethod(testMethod, None, KnownValues)
    setattr(KnownValues, "test_%s" % noun, instanceMethod)

if __name__ == "__main__":
    unittest.main()
