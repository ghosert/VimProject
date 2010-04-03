"""Unit test for soundex.py

This program is part of "Dive Into Python", a free Python book for
experienced programmers.  Visit http://diveintopython.org/ for the
latest version.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.1 $"
__date__ = "$Date: 2004/05/06 17:18:17 $"
__copyright__ = "Copyright (c) 2004 Mark Pilgrim"
__license__ = "Python"

import soundex
import unittest

class KnownValues(unittest.TestCase):
    knownValues = (('', '0000'),
		   ('Woo', 'W000'),
		   ('Pilgrim', 'P426'),
		   ('Radiohead', 'R330'),
		   ('Flingjingwaller', 'F452'),
		   ('Euler', 'E460'),
		   ('Ellery', 'E460'),
		   ('Gauss', 'G200'),
		   ('Ghosh', 'G200'),
		   ('Hilbert', 'H416'),
		   ('Heilbronn', 'H416'),
		   ('Knuth', 'K530'),
		   ('Kant', 'K530'),
		   ('Lukasiewicz', 'L222'),
		   ('Lissajous', 'L222')
                  )
    
    def testKnownValues(self):
        """soundex should give known result with known input"""
        for name, result in self.knownValues:
	    self.assertEqual(soundex.soundex(name), result)

if __name__ == "__main__":
    unittest.main()
