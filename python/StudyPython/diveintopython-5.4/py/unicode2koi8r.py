"""Convert Cyrillic from iso-8859-1 Unicode-encoded to KOI8-R-encoded

This script is used during the build process of the Russian translation
of "Dive Into Python" (http://diveintopython.org/).

It takes one argument, which can be either an HTML file or a directory.
If a file, it converts the file in place; if a directory, it converts
every HTML file in the immediate directory (but not recursively).

Safe but pointless to run more than once on the same file or directory.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.2 $"
__date__ = "$Date: 2004/05/05 21:57:19 $"
__copyright__ = "Copyright (c) 2001 Mark Pilgrim"
__license__ = "Python"

import os
import sys
import re

unicodeToKOI8R = { \
	'&#1025;': '\xb3',
	'&#1040;': '\xe1',
	'&#1041;': '\xe2',
	'&#1042;': '\xf7',
	'&#1043;': '\xe7',
	'&#1044;': '\xe4',
	'&#1045;': '\xe5',
	'&#1046;': '\xf6',
	'&#1047;': '\xfa',
	'&#1048;': '\xe9',
	'&#1049;': '\xea',
	'&#1050;': '\xeb',
	'&#1051;': '\xec',
	'&#1052;': '\xed',
	'&#1053;': '\xee',
	'&#1054;': '\xef',
	'&#1055;': '\xf0',
	'&#1056;': '\xf2',
	'&#1057;': '\xf3',
	'&#1058;': '\xf4',
	'&#1059;': '\xf5',
	'&#1060;': '\xe6',
	'&#1061;': '\xe8',
	'&#1062;': '\xe3',
	'&#1063;': '\xfe',
	'&#1064;': '\xfb',
	'&#1065;': '\xfd',
	'&#1066;': '\xff',
	'&#1067;': '\xf9',
	'&#1068;': '\xf8',
	'&#1069;': '\xfc',
	'&#1070;': '\xe0',
	'&#1071;': '\xf1',
	'&#1072;': '\xc1',
	'&#1073;': '\xc2',
	'&#1074;': '\xd7',
	'&#1075;': '\xc7',
	'&#1076;': '\xc4',
	'&#1077;': '\xc5',
	'&#1078;': '\xd6',
	'&#1079;': '\xda',
	'&#1080;': '\xc9',
	'&#1081;': '\xca',
	'&#1082;': '\xcb',
	'&#1083;': '\xcc',
	'&#1084;': '\xcd',
	'&#1085;': '\xce',
	'&#1086;': '\xcf',
	'&#1087;': '\xd0',
	'&#1088;': '\xd2',
	'&#1089;': '\xd3',
	'&#1090;': '\xd4',
	'&#1091;': '\xd5',
	'&#1092;': '\xc6',
	'&#1093;': '\xc8',
	'&#1094;': '\xc3',
	'&#1095;': '\xde',
	'&#1096;': '\xdb',
	'&#1097;': '\xdd',
	'&#1098;': '\xdf',
	'&#1099;': '\xd9',
	'&#1100;': '\xd8',
	'&#1101;': '\xdc',
	'&#1102;': '\xc0',
	'&#1103;': '\xd1',
	'&#1105;': '\xa3' }

unicodePattern = re.compile(r'&#[0-9]{4,4};')
charsetPattern = re.compile(r'ISO-8859-1', re.IGNORECASE)

def translateMatch(match):
	unicode = match.group(0)
	if unicodeToKOI8R.has_key(unicode):
		return unicodeToKOI8R[unicode]
	else:
		return unicode

def translateBuffer(buffer):
	buffer = unicodePattern.sub(translateMatch, buffer)
	buffer = charsetPattern.sub('KOI8-R', buffer)
	return buffer

def translateFile(filename, outfilename=None):
	if not outfilename:
		outfilename = filename
	fsock = open(filename)
	buffer = fsock.read()
	fsock.close()
	buffer = translateBuffer(buffer)
	fsock = open(outfilename, 'wb')
	fsock.write(buffer)
	fsock.close()

def htmlFilter(filename):
	return os.path.splitext(filename)[1] == '.html'

def translateDirectory(directoryname, filterFunc=htmlFilter):
	fileList = [os.path.join(directoryname, f) for f in os.listdir(directoryname)]
	fileList = filter(filterFunc, fileList)
	map(translateFile, fileList)

if __name__ == "__main__":
	name = sys.argv[1]
	if os.path.isdir(name):
		translateDirectory(name)
	else:
		translateFile(name)
