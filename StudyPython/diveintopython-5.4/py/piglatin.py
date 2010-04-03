"""Convert text to Pig Latin

This program is part of "Dive Into Python", a free Python book for
experienced programmers.  Visit http://diveintopython.org/ for the
latest version.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.2 $"
__date__ = "$Date: 2004/05/05 21:57:19 $"
__copyright__ = "Copyright (c) 2002 Mark Pilgrim"
__license__ = "Python"

import re

def _wordToPigLatin(match):
    word = match.group()
    consonants = match.group(1)
    restOfWord = match.group(2)
    # put consonants after rest of word, and add "ay"
    result = "%s%say" % (restOfWord, consonants)
    # if word was all uppercase, make result uppercase
    if word == word.upper():
        result = result.upper()
    # if word was capitalized, make result capitalized
    elif word == word.capitalize():
        result = result.capitalize()
    return result

def pigLatin(source):
    pattern = re.compile(r'\b([bcdfghjklmnpqrstvwxyz]*)(\w+)\b', re.IGNORECASE)
    return pattern.sub(_wordToPigLatin, source)
    
