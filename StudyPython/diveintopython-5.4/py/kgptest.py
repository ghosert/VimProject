"""Unit test for kgp.py

This program is part of "Dive Into Python", a free Python book for
experienced programmers.  Visit http://diveintopython.org/ for the
latest version.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.2 $"
__date__ = "$Date: 2004/05/05 21:57:19 $"
__copyright__ = "Copyright (c) 2001 Mark Pilgrim"
__license__ = "Python"

import unittest
import sys
if 'kgp' not in sys.path:
	sys.path.append('kgp')
import kgp

class KGPTest(unittest.TestCase):
	resultsMap = {"a":"0",
				  "b":"1",
				  "c":"2",
				  "d":"",
				  "e":"0",
				  "f":"10",
				  "g":"1"}
	
	def setUp(self):
		self.parser = kgp.KantGenerator('kgp/test.xml')

	def doTest(self, key):
		self.parser.loadSource('<xref id="%s"/>' % key)
		self.assertEqual(self.resultsMap[key], self.parser.refresh())
		
	def testA(self):
		"""kgp a ref test"""
		self.doTest("a")

	def testB(self):
		"""kgp b ref test"""
		self.doTest("b")

	def testC(self):
		"""kgp c ref test"""
		self.doTest("c")

	def testD(self):
		"""kgp d ref test"""
		self.doTest("d")

	def testE(self):
		"""kgp e ref test"""
		self.doTest("e")

	def testF(self):
		"""kgp f ref test"""
		self.doTest("f")

	def testG(self):
		"""kgp g ref test"""
		self.doTest("g")

if __name__ == "__main__":
	unittest.main()
