"""Convert HTML page to Word 97 document

This script is used during the build process of "Dive Into Python"
(http://diveintopython.org/) to create the downloadable Word 97 version
of the book (http://diveintopython.org/diveintopython.doc)

Looks for 2 arguments on the command line.  The first argument is the input (HTML)
file; the second argument is the output (.doc) file.

Only runs on Windows.  Requires Microsoft Word 2000.

Safe to run on the same file(s) more than once.  The output file will be
silently overwritten if it already exists.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.2 $"
__date__ = "$Date: 2004/05/05 21:57:19 $"
__copyright__ = "Copyright (c) 2001 Mark Pilgrim"
__license__ = "Python"

import sys, os
from win32com.client import gencache, constants

def makeRealWordDoc(infile, outfile):
	word = gencache.EnsureDispatch("Word.Application")
	try:
		worddoc = word.Documents.Open(FileName=infile)
		try:
			worddoc.TablesOfContents.Add(Range=word.ActiveWindow.Selection.Range, \
										 RightAlignPageNumbers=1, \
										 UseHeadingStyles=1, \
										 UpperHeadingLevel=1, \
										 LowerHeadingLevel=2, \
										 IncludePageNumbers=1, \
										 AddedStyles='', \
										 UseHyperlinks=1, \
										 HidePageNumbersInWeb=1)
			worddoc.TablesOfContents(1).TabLeader = constants.wdTabLeaderDots
			worddoc.TablesOfContents.Format = constants.wdIndexIndent

			word.ActiveWindow.ActivePane.View.SeekView = constants.wdSeekCurrentPageHeader
			word.Selection.TypeText(Text="Dive Into Python\t\thttp://diveintopython.org/")
			word.ActiveWindow.ActivePane.View.SeekView = constants.wdSeekCurrentPageFooter
			word.NormalTemplate.AutoTextEntries("- PAGE -").Insert(Where=word.ActiveWindow.Selection.Range)
			word.ActiveWindow.View.Type = constants.wdPrintView

			worddoc.TablesOfContents(1).Update()
			
			worddoc.SaveAs(FileName=outfile, \
				FileFormat=constants.wdFormatDocument)
		finally:
			worddoc.Close(0)
			del worddoc
	finally:
		word.Quit()
		del word

if __name__ == "__main__":
	infile = os.path.normpath(os.path.join(os.getcwd(), sys.argv[1]))
	outfile = os.path.normpath(os.path.join(os.getcwd(), sys.argv[2]))
	makeRealWordDoc(infile, outfile)
