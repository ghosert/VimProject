"""Build examples of output of dialect module

This script is used during the build process of "Dive Into Python"
(http://diveintopython.org/) to create examples of the output of the
code in chapter 4 (dialect.py and BaseHTMLProcessor.py).

It takes one argument, the source HTML file to translate.  It outputs
chef.html, fudd.html, and olde.html in the same directory as the source.

Safe to run more than once.  Output files are silently overridden if
they already exist.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.2 $"
__date__ = "$Date: 2004/05/05 21:57:19 $"
__copyright__ = "Copyright (c) 2001 Mark Pilgrim"
__license__ = "Python"

import dialect
import sys, os

def translateAndWrite(filename, dialectname):
	targetfilename = os.path.join(os.path.split(filename)[0], "%s.html" % dialectname)
	fsock = open(targetfilename, "wb")
	fsock.write(dialect.translate(filename, dialectname))
	fsock.close()

if __name__ == "__main__":
	filename = sys.argv[1]
	for dialectname in ("chef", "fudd", "olde"):
		translateAndWrite(filename, dialectname)
