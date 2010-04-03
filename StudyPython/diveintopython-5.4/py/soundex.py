"""Soundex algorithm

This program is part of "Dive Into Python", a free Python book for
experienced programmers.  Visit http://diveintopython.org/ for the
latest version.
"""

__author__ = "Mark Pilgrim (mark@diveintopython.org)"
__version__ = "$Revision: 1.5 $"
__date__ = "$Date: 2004/05/11 19:11:21 $"
__copyright__ = "Copyright (c) 2004 Mark Pilgrim"
__license__ = "Python"

import string

allChar = string.uppercase + string.lowercase
charToSoundex = string.maketrans(allChar, "91239129922455912623919292" * 2)

def soundex(source):
    "convert string to Soundex equivalent"

    # Soundex requirements:
    # source string must be at least 1 character
    # and must consist entirely of letters
    if (not source) or (not source.isalpha()):
        return "0000"

    # Soundex algorithm:
    # 1. make first character uppercase
    # 2. translate all other characters to Soundex digits
    digits = source[0].upper() + source[1:].translate(charToSoundex)

    # 3. remove consecutive duplicates
    digits2 = digits[0]
    for d in digits[1:]:
        if digits2[-1] != d:
            digits2 += d
        
    # 4. remove all "9"s
    # 5. pad end with "0"s to 4 characters
    return (digits2.replace('9', '') + '000')[:4]

if __name__ == '__main__':
    import sys
    if sys.argv[1:]:
        print soundex(sys.argv[1])
    else:
        from timeit import Timer
        names = ('Woo', 'Pilgrim', 'Flingjingwaller')
        for name in names:
            statement = "soundex('%s')" % name
            t = Timer(statement, "from __main__ import soundex")
            print name.ljust(15), soundex(name), min(t.repeat())
