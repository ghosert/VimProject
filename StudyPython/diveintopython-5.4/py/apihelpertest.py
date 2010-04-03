"""Unit test for apihelper.py

This program is part of "Dive Into Python", a free Python book for
experienced programmers.  Visit http://diveintopython.org/ for the
latest version.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.4 $"
__date__ = "$Date: 2004/05/05 21:57:19 $"
__copyright__ = "Copyright (c) 2001 Mark Pilgrim"
__license__ = "Python"

import unittest
import apihelper
import sys
from StringIO import StringIO

class Redirector(unittest.TestCase):
    def setUp(self):
        self.savestdout = sys.stdout
        self.redirect = StringIO()
        sys.stdout = self.redirect

    def tearDown(self):
        sys.stdout = self.savestdout

class KnownValues(Redirector):
    def testApiHelper(self):
        """info should return known result for apihelper"""
        apihelper.info(apihelper)
        self.redirect.seek(0)
        self.assertEqual(self.redirect.read(),
"""info       Print methods and doc strings. Takes module, class, list, dictionary, or string.
""")

class ParamChecks(Redirector):
    def testSpacing(self):
        """info should honor spacing argument"""
        apihelper.info(apihelper, spacing=20)
        self.redirect.seek(0)
        self.assertEqual(self.redirect.read(),
"""info                 Print methods and doc strings. Takes module, class, list, dictionary, or string.
""")

    def testCollapse(self):
        """info should honor collapse argument"""
        apihelper.info(apihelper, collapse=0)
        self.redirect.seek(0)
        self.assertEqual(self.redirect.read(),
"""info       Print methods and doc strings.

	Takes module, class, list, dictionary, or string.
""")

class BadInput(unittest.TestCase):
    def testNoObject(self):
        """info should fail with no object"""
        self.assertRaises(TypeError, apihelper.info, spacing=20)

if __name__ == "__main__":
    unittest.main()
