""" This is a brief standard library test case for python """

if __name__ == "__main__":

    # Operating System Interface
    import os
    # os.system('command') to execute the native command, return 0 means success while 1 mean failure.
    os.system('dir jiaweizhang111')
    print os.getcwd()
    os.chdir('.')
    print os.getcwd()

    # According to daily file and dictionary management task, the shutil a higher level interface that is eaiser to use.
    import shutil
    shutil.copyfile('data', 'data1')
    shutil.move('data1', 'data')

    # glob module for making lists from directory wildcard searches.
    import glob
    print glob.glob('*.py')

    # standard error, standard out, standard in and terminate program
    import sys
    sys.stderr.write('stderr')
    print
    sys.stdout.write('stdout')
    print
    # print sys.stdin.readline()   # input a line from keyboard.
    # print sys.stdin.readlines()  # input mutipule lines from keyboard, ctrl-z to break.
    # sys.exit(0) # exit program with successful status.

    # String pattern matching
    import re
    print re.findall('\\bf[a-z]*', 'which afoot foot or hand fell fastest')
    # equals to because of \b here, afoot will not be shown.
    print re.findall(r'\bf[a-z]*', 'which afoot foot or hand fell fastest')
    print re.sub(r'(\b[a-z]+) \1', r'\1', 'cat in the the hat')
    print 'one too three'.replace('too', 'two')

    # mathematics
    import math
    print math.sin(math.pi / 2)
    print math.log(1024, 2)
    import random
    print random.choice(['apple', 'pear', 'banana'])
    print random.sample(range(100), 10)
    print random.random()
    print random.randrange(6)

    # internet access
    import urllib2
    for line in urllib2.urlopen('http://www.baidu.com'):
        if 'style=height:60px' in line:
            print line

    # comment the codes below since it needs a mailserver running on localhost, 'sudo apt-get install sendmail' if you want to run a mail server locally.
    # import smtplib
    # server = smtplib.SMTP('localhost')
    # server.sendmail(...)
    # server.quit()

    # download file.
    # import urllib
    # urllib.urlretrieve('http://haoetv.com/haoting/wma08/318.wma', 'C:\\318.wma')

    # Date time
    from datetime import date
    now = date.today()
    print now.strftime("%m-%d-%y. %d %b %Y is a %A on the %d day of %B.") # same to: import time && print time.strftime('%d%b%y')
    birthday = date(1980, 11, 14)
    age = now - birthday
    print age.days

    # archiving and compression data, other module including zlib, gzip, bz2, zipfile, tarfile
    import zlib
    s = 'The datas will be archived and compressed.'
    print 'The length of data before compression: {0}'.format(len(s))
    t = zlib.compress(s)
    print 'The length of data after compression: {0}'.format(len(t))
    s = zlib.decompress(t)
    print 'The length of data after decompression: {0}'.format(len(s))

    # performance measurement, other module to test performace including profile and pstats
    import timeit
    print timeit.Timer('t = a; a = b; b = t', 'a = 1; b = 2').timeit()
    print timeit.Timer('a, b = b, a', 'a = 1; b = 2').timeit()

    # unit test, doctest and unittest module for unit test.
    def average(values):
        """Computing average value.

        >>> print average([1, 2, 3])
        2.0
        """
        return sum(values, 0.0) / len(values)

    import doctest
    print doctest.testmod()   # automatically validate the embedded tests, the case in doc string above can be cut and pasted from interpreter.

    import unittest
    class TestStatisticalFunctions(unittest.TestCase):
        def test_average(self):
            self.assertEqual(average([20, 30, 70]), 40.0)
            self.assertEqual(round(average([1, 5, 7]), 1), 4.3)
            self.assertRaises(ZeroDivisionError, average, [])
            self.assertRaises(TypeError, average, 20, 30, 70)
    # unittest.main() # Calling from the command line invokes all tests

    # output formatting
    print '\noutput formatting test'
    import repr
    print repr.repr(set('helloworldworldhelloyouareabigmanforallofus'))
    import pprint
    t = [[[['black', 'cyan'], 'white', ['green', 'red']], [['magenta', 'yellow'], 'blue']]]
    pprint.pprint(t, width=30)
    import textwrap
    doc = """The wrap() method is just like fill() except that it returns
    a list of strings instead of one big string with newlines to separate
    the wrapped lines."""
    print doc
    print textwrap.fill(doc, width=40)
    
    import locale
    locale.setlocale(locale.LC_ALL, 'English_United States.1252')
    conv = locale.localeconv() # get a mapping of conventions
    x = 1234567.8
    print locale.format("%d", x, grouping=True)
    print locale.format("%s%.*f", (conv['currency_symbol'], conv['frac_digits'], x), grouping=True)

    # templating
    from string import Template
    template = Template('${village}folk send $$10 to $cause.')
    print template.substitute(village='Nottingham', cause='the ditch fund')    # the parameter of template.substitue() support both "dict object" and the form of "(xxx = yyy, jjj = kkk)"

    t = Template('Return the $item to $owner.')
    d = dict(item='unladen swallow') # equals to d = {'item':'unladen swallow'}
    # print t.substitute(d)    # throw a exception saying: KeyError: 'owner'
    print t.safe_substitute(d) # because of the line above, so we use safe_substitute instead of substitue. This will remain $owner in the string instead of a KeyError exception.

    # use the new delimiter instead of $ above
    class NewDelimiter(Template):
        delimiter = '%'
    print NewDelimiter('%jiawei is a good man.').substitute(jiawei='JIAWEI')

    print

    # multi-thread
    import threading, zipfile
    class AsyncZip(threading.Thread):
        def __init__(self, infile, outfile):
            threading.Thread.__init__(self)
            self.infile = infile
            self.outfile = outfile
        def run(self):
            f = zipfile.ZipFile(self.outfile, 'w', zipfile.ZIP_DEFLATED)
            f.write(self.infile)
            f.close()
            print 'Finished background zip of: ', self.infile
    background = AsyncZip('data', 'myarchive.zip')
    background.start()
    print 'The main program continues to run in foreground.'
    background.join() # Wait for the background task to finish
    print 'Main program waited until background was done.'

    print

    # loggin system
    import logging
    logging.debug('Debugging information')
    logging.info('Informational message')
    logging.warning('Warning:config file %s not found', 'server.conf')
    logging.error('Error occurred')
    logging.critical('Critical error -- shutting down')
    # by default, debug and info above is supressed, and all the logging message output is sent to standard error.

    # add item to sorted list 
    import bisect
    li = [1, 2, 3, 4, 5, 6]
    bisect.insort(li, 0)
    print li
    
    print

    # heappq make sure the smallest value put into the zero value.
    from heapq import heapify, heappop, heappush
    data = [1, 3, 5, 7, 9, 2, 4, 6, 8, 0]
    print 'list before re-range: {0}'.format(data)
    heapify(data) # rearrange the list into heap order
    print 'list after re-range: {0}'.format(data)
    heappush(data, -5) # add a new entry
    print 'list after add new value -5: {0}'.format(data)
    print 'fetch the three smallest entries: {0}'.format([heappop(data) for i in range(3)])

    def fff():
        return 'i'

    print [fff() for i in range(3)]
    

    
    

    

