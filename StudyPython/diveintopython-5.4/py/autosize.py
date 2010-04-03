"""
Add size of all downloadable archives as title attribute of download link

This script is used during the build process of "Dive Into Python"
(http://diveintopython.org/) to add size descriptions to all download links
on the home page.  Requires that the links be relative paths from the current
working directory.  Also requires that the download links already have some
title already; download size is appended to the existing title.

Looks for two arguments on the command line.  First: if a file, the file is
processed.  If a directory, all .html files in the directory are processed.
Second: directory of files to use for sizing information.
If no arguments given, a test suite is performed on a hard-coded test file
which saves the output to a temporary file and opens it in a web browser locally.

Not safe to run on the same file(s) more than once, since the size information
is simply concatenated to the existing title.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.2 $"
__date__ = "$Date: 2004/05/05 21:57:19 $"
__copyright__ = "Copyright (c) 2001 Mark Pilgrim"
__license__ = "Python"

import sys
import os
import stat
import urlparse
from BaseHTMLProcessor import BaseHTMLProcessor

class AutosizeParser(BaseHTMLProcessor):
	def __init__(self, basedir):
		BaseHTMLProcessor.__init__(self)
		self.basedir = basedir
		
	def start_a(self, attrs):
		try:
			href = [e[1] for e in attrs if e[0]=='href'][0]
			title = [e[1] for e in attrs if e[0]=='title'][0]
		except IndexError:
			pass
		else:
			filename = os.path.split(urlparse.urlparse(href)[2])[1]
			if os.path.splitext(filename)[1] in ('.zip', '.tgz', '.hqx'):
				zipfile = os.path.join(self.basedir, filename)
				title += " (%d KB)" % ((os.stat(zipfile)[stat.ST_SIZE] + 1023)/1024,)
				attrs = tuple([e for e in attrs if e[0]<>'title'] + [('title', title)])
		self.unknown_starttag("a", attrs)
		
def process(filename, basedir, outfile=None):
	if not outfile:
		outfile = filename
	sock = open(filename, "r")
	parser = AutosizeParser(basedir)
	parser.feed(sock.read())
	output = parser.output()
	sock.close()
	sock = open(outfile, "w")
	sock.write(output)
	sock.close()
	return output

def test(filename, basedir, outfile="c:\\out.html"):
	output = process(filename, basedir, outfile)
	print output
	import webbrowser
	webbrowser.open(outfile)

if __name__ == "__main__":
	if sys.argv[1:]:
		filedir = sys.argv[1]
		basedir = sys.argv[2]
		if os.path.isdir(filedir):
			for f in [os.path.join(filedir, s) for s in os.listdir(filedir) if os.path.splitext(s)[1].lower() == '.html']:
##				print "Autosizing %s" % os.path.basename(f)
				process(f, basedir)
		else:
##			print "Autosizing %s" % os.path.basename(filedir)
			process(filedir, basedir)
	else:
		test("c:\\docbook\\dip\\dist\\html\\index.html", "c:\\docbook\\dip\\dist\\download\\")
	
