""" This is a python study module to practise the python."""

class Study:
    " This is a python study class to practise the python."
    def do(self):
        print "Hello Study().do()!"
    def __setitem__(self, key, item):
        print "I'm setting item."
    def __getitem__(self, key):
        print "I'm getting item."

def studyMethod():
    " This is a python study method to practise the python."
    print "Hello studyMethod()"

if __name__ == "__main__":
    # new a object and set it to a variable.
    study = Study() 
    # because of __setitem__ above, it will be triggled by this line.
    study["jiawei"] = "zhang" 
    # because of __getitem__ above, it will be triggled by this line.
    study["jiawei"] 
    print "Study.__module__"
    # see the module of the object, it should be __main__.
    print Study.__module__ 
    print
    # import new module
    import study 
    print "study.Study.__module__"
    # see the module of the object, it should be __study__ compared with __main__ above.
    print study.Study.__module__ 
    print
    print "study.__doc__"
    # see the python doc.
    print study.__doc__ 
    print
    print "dir(study)"
    # dir a object to see its attribute.
    print dir(study) 
    print
    import sys
    # print the list of the python sys path
    print sys.path 
    # import a self defined python file.
    import test 
    t = test.TestMe()
    t.doSomething()
    # see whether a filename is fold or file.
    import os
    print os.path.isdir("c:\\")
    # split a filename
    print os.path.splitext("jiawei.zip")

    # see what's happen in r"jiawei\n"
    print "jiawei\njiawei\jjjj \
                                   jiadfdssdf"
    print r"jiawei\njiawei\jjjj \
                                   jiadfdssdf"
    print "Test + word sequence"
    word = " jiawei "
    print word
    print word * 5
    print word[4] # should be "w" here
    print word[0:2] # should be " j" here
    print word[2:4] # should be "ia" here
    print word[:2] # should be " j" here
    print word[2:] # should be "iawei " here
    print word[1:100] # should be "jiawei " here
    print word[10:] # should be "" here
    print word[2:1] # should be "" here
    # print word[100] # out of range error

    print "Test - word sequence"
    print word[-1] # should be " " here
    print word[-2] # should be "i" here
    print word[-2:] # should be "i " here
    print word[:-2] # should be " jiawe" here
    print word[-100:] # should be " jiawei " here
    # print word[-100] # index out of range.

    print "len(word): " + str(len(word))

    # test unicode
    print u"Hello World"
    print u"Hello\u0020World"
    print u"Hello\\u0020World"

    # Testing below need convert this file to UTF-8 format, but ms console fail to change righ code page by using "chcp" dos command, try it later.
    # convert utf-8 to unicode, str.decode('codepage') equals to unicode(str, 'codepage')
    decoded_str1 = "中国".decode("utf-8")
    print decoded_str1
    decoded_str2 = unicode("中国", "utf-8")
    print decoded_str2
    decoded_str3 = u"中国"
    print decoded_str3
    # convert unicode to utf-8, str.encode('codepage')
    print '中国'
    print decoded_str1.encode('utf-8')
    print decoded_str2.encode('utf-8')
    print decoded_str3.encode('utf-8')

    # test list
    print "Test list, list is just a sequence, its behaviour"
    a = ['spam', 'eggs', 100, 1234]
    print a
    print a[0]
    print a[3]
    print a[-2]
    print a[1:-1]
    print a[:2] + ['bacon', 2*2]
    a[2] = a[2] + 23
    print a
    a[0:2] = [1, 12] # should be 1, 12, 123, 1234
    print a
    a[0:2] = [] # should be [123, 1234]
    print a
    a[1:1] = ['bletch', 'xyzzy'] # insert some items in the position 1, and only take 1 space. should be 123, 'bletch', 'xyzzy', 1234
    print a
    a[:0] = a
    print a
    print "len(a): " + str(len(a))
    a[:] = [] # clear all
    print a
    q = [2, 3]
    p = [1, q, 4]
    print p[1] # should be [2, 3]
    print p[1][0] # should be 2
    p[1].append('xtra') # should be [2, 3, 'xtra']
    print p
    print q

    # while and assignment
    a, b = 0, 1
    while b < 10:
        print b, # , after print means print the content in one line with space for splitting.
        a, b = b, a + b # assign two variables at the same time.

    # Input from console, I comment it for it break the test, you can uncomment it as you like.
    # i = int(raw_input("Please input an integer: "))
    # print "You have already inputted:", i

    # for and range()
    for i in range(10):
        print i,
    
    print

    # for and else
    for i in range(10):
        if i > 5:
            print "else of for clause will not be executed, because break clause has been executed."
            break
    else:
        print "else clause is being executed because no break clause has been executed."

    for i in range(10):
        if i > 9:
            print "else of for clause will not be executed, because break clause has been executed."
            break
    else:
        print "else clause is being executed because no break clause has been executed."

    def function(str):
        print str

    # test functions.
    function("This is a function.")

    f = function

    f("This is a assigned function")

    print f("") # every function has at least one returned value, if no explicit definition, it's None.

    # pass the tupe and dictionary as parameter list
    def function1(string, *tuple, **dictionary):
        print "string: " + string
        print "tuple: " + str(tuple)
        print "dictionary: " + str(dictionary)

    function1("i", 1, 2, 'list', age=30)

    # unpack parameter from list/tuple, dictionary.
    def function2(one, two, three):
        print one, two, three

    list = [1, 2, 3]
    function2(*list)
    dicts = {"two":2, "one":1, "three":3}
    function2(**dicts)

    # test both function1 & unpack parameter from list/tuple, dictionary.
    function1("i", *list, **dicts)

    # test lambda
    function = lambda x: x * 3 # lambda

    print function(1)

    # Use CamelCase for the class name and lower_case_with_underscores for functions and methods, use self as the first method argument

# Data structures

    # More on list
    a = [66.25, 333, 333, 1, 1234.5]
    print a.count(333), a.count(66.25), a.count('x')
    a.insert(2, -1)
    a.append(333)
    print a
    print "333 is located in the position: " + str(a.index(333))
    a.remove(333)
    print "The first 333 has been removed: \n %s" % a
    a.reverse()
    print "The a list has been reversed: \n %s" % a
    a.sort()
    print "The a list has been sorted: \n {0}".format(a)

    # Use List as stack
    print "Pop the last item like stack, filo: \n {0}\n {1}".format(a.pop(), a)

    # Use List as queue
    print "Pop the first item like queue, fifo: \n {0}\n {1}".format(a.pop(0), a)

    # List comprehensions
    freshfruit = [' banana', ' loganberry ', 'passion fruit ']
    print [weapon.strip() for weapon in freshfruit]
    vec = [2, 4, 6]
    print [3 * x for x in vec if x > 3]
    # print [x, x**2 for x in vec]  ## This clause will produce a exception. ##
    print [(x, x**2) for x in vec]  ## This clause is right without problem. ##
    vec1 = [1, 2, 3]
    print "vector 1: {0}".format(vec)
    print "vector 2: {0}".format(vec1)
    print "[x * y for x in vec for y in vec1]: {0}".format([x * y for x in vec for y in vec1])
    vec2 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    print "vector 3: {0}".format(vec2)
    print "zip(*vector3): {0}".format(zip(*vec2))
    # del statement
    print "ready to delete list: {0}".format(a)
    del a[0]
    print "del a[0]: {0}".format(a)
    del a[1:3]
    print "del a[1:3]: {0}".format(a)
    del a[:]
    print "del a[:]: {0}".format(a)
    del a
    print '"del a" means a will not point to any memory block'

    # define a tuple
    a = (1, 2, 3)
    print a

    # sequence packing
    a = 1, 2, 3
    print "sequence packing: {0}".format(a)
    # sequence unpacking
    i, j, k = a
    print "sequence unpacking i={0}, j={1}, k={2}".format(i, j, k)
    # sequence packing only create tuple while sequence unpacking works for any sequence.

    # set function to remove duplicate data from sequence.
    basket = ['apple', 'orange', 'apple', 'pear', 'orange', 'banana']
    print "Before invoking set function: {0}".format([item for item in basket])
    fruit = set(basket)
    print "After invoking set function: {0}".format([item for item in fruit])
    # String is also a sequence can be invoked by Set function.
    a = set("aaaaabbbbbccccccdddddd")
    print a

    # set function support the sign "- | & ^"
    b = set("aaaaabbbbbcccccc")
    print b
    print "a - b: {0}".format(a - b)

    # dictionary
    # dictionaries are indexed by keys, which can be any immutable type; strings and numbers can always be keys.
    # Tuples can be used as keys if they contain only strings, numbers, or tuples;
    # if a tuple contains any mutable object either directly or indirectly, it cannot be used as a key.
    tel = {'jack': 4098, 'sape': 4139}
    tel['guido'] = 4127
    print "dictionary: {0}".format(tel)
    print "jack in dictionary: {0}".format(tel['jack'])
    del tel['sape']
    tel['irv'] = 4127
    print "dictionary: {0}".format(tel)
    print "dictionary keys: {0}".format(tel.keys())
    print "'guido' in tel: {0}".format('guido' in tel)
    # Using dict() to build a dictionary. dict() equals to {}
    tel = dict([('sape', 4139), ('guido', 4127), ('jack', 4098)])
    print "dictionary built by dict(): {0}".format(tel)
    print "reversal dict() by using {{}}.items(): {0}".format(tel.items())
    # Looping data structure
    for k, v in tel.iteritems():
        print k, v
    # You can also using tel.items() above, but it seems encourage to use tel.iteritems()
    # Get index and item at the same time by using enumerate(list)
    for k, v in enumerate(['a', 'b', 'c', 'd']):
        print k, v
    # zip(list, list) in looping
    questions = ['name', 'quest', 'favorite color']
    answers = ['lancelot', 'the holy grail', 'blue']
    for q, a in zip(questions, answers):
        print 'What is your {0}? It is {1}.'.format(q, a)
    # loop a sequence in reverse
    print [i for i in reversed(range(1, 10, 2))]
    print [i for i in reversed("abcde")]
    # loop a sequence in with set and sorted
    # Notice: sorted(sequence) is different from sequence.sort(): sort sequence itself
    basket = ['apple', 'orange', 'apple', 'pear', 'orange', 'banana']
    for f in sorted(set(basket)):
        print f,
    print
    
    # "and" and "or", their arguments are evaluated from left to right
    # "not" has the highest priority "and" and "or" the lowest, so that A and not B or C is equivalent to (A and (not B)) or C.
    # no "&&" and "||" in python, but "and" and "or"
    # It is possible to assign the result of a comparison or other Boolean expression to a variable.
    string1, string2, string3 = '', 'Trondheim', 'Hammer Dance'
    non_null = string1 or string2 or string3
    print non_null
    # '', [], None, {}, (), 0 means False while any other value in python mean True
    # evaluation stops as soon as the outcome is determined, so the result below is 'Ture Value' while other expression is false.
    print '' or [] or None or {} or () or 0 or 'True Value'
    print "True Value" and "True Value"

    # In Python, there is no expression like: boolean ? valueForTrue : valueForFalse
    # But we have this now in python 2.6 later:
    print 'valueForTrue' if True else 'valueForFalse'

# module in python

    # invoke a function of module
    import test
    test.test_method()
    # use from "module" import "object"
    from test import test_method
    test_method()
    # Using "from module import object" is different from "import module", which means you can use "object" directly instead of "module.object"
    # Using "from module import *" to import all the objects, see below, TestMe class has been imported without "test." prefix need
    from test import *
    TestMe().doSomething()
    
    # When the study.py is executed by python.exe, all the objects in the module study.py has been imported, you can use these objects directly. e.g. below:
    print "I'm invoking studyMethod() in main directly without module name as prefix. "
    studyMethod()
    print '"python study.py" will make sys.argv equals to: {0}'.format(sys.argv)
    print sys.path
    # add new path to sys.path list for the module searching purpose.
    sys.path.append(".")
    print sys.path
    # set environment variable PYTHONPATH as the module search path.

    # The only thing that's faster about .pyc or .pyo files is the speed with which they are loaded.
    # It is possible to have a file called .pyc/.pyo without .py for the same module. This can be used for avoiding reverse engineer

    # dir(object), object can be any thing in python such as function, class, method, variable while dir() lists the names you have defined currently.
    print "dir(): {0}".format(dir())

    # Package is a folder which contains __init__.py(can be empty) and sub-modules, you should know about it.

    # sound/                   Top-level package
        # __init__.py          Initialize the sound package
        # formats/             Subpackage for file format conversions
        # __init__.py
        # wavread.py
        # wavwrite.py
        # aiffread.py
        # aiffwrite.py
        # auread.py
        # auwrite.py
        # ...
    # effects/                 Subpackage for sound effects
        # __init__.py
        # echo.py
        # surround.py
        # reverse.py
        # ...
    # filters/                 Subpackage for filters
        # __init__.py
        # equalizer.py
        # vocoder.py
        # karaoke.py
        # ...

    # The __init__.py files are required to make Python treat the directories as containing packages
    # In the simplest case, __init__.py can just be an empty file, but it can also execute initialization code for the package or set the __all__ variable, described later.
        # import sound.effects.echo
        # sound.effects.echo.echofilter(input, output, delay=0.7, atten=4)
        # from sound.effects import echo
        # echo.echofilter(input, output, delay=0.7, atten=4)
        # from sound.effects.echo import echofilter
        # echofilter(input, output, delay=0.7, atten=4)
    # from package/module import objects
    # import package.module/package.subpackage (can not import any other objects here except package and module)
    # exceptions when import package:
        # import sound.effects
        # sound.effects.echo (WRONG)
        # from sound import effects
        # effects.echo (WRONG)
        # (The four lines above are wrong because, import package can not make it recognize sub module, unless you import module directly like below:)
        # import sound.effects.echo
        # sound.effects.echo (RIGHT)
        # from sound.effects import echo
        # echo (RIGHT)
        # (total forms of import/from ... import ...)
        # import package (fun/class in package __init__.py can be accessable by package.fun/class, package.module can not be accessable)
        # import package.module
        # from package import subpackage (fun/class in subpackage __init__.py can be accessable by subpackage.fun/class, subpackage.module can not be accessable)
        # from package import module
        # from package.module import function/class
        # from package.module import *
        # from package import * (sub modules can be accessable if they are in the '__all__' list in __init__.py file, see more details below.)

    # when __all__ is defined in __init__.py like below:
    # __all__ = ["echo", "surround", "reverse"]
    # means "from sound.effects import *" will import the three submodule above.
    # if no __all__ is defined, "from sound.effects import *" means import package sound.effects only, no any module will be imported.
    # For simplify the case, just you don't have to define __all__ here, just using "from package import specific_submodule"

# file in python
    
    # str() returns a human readable result while repr() returns a interpreter readable result
    s = 'Hello world.'
    print str(s)
    print repr(s)

    # print '{0:2d} {1:3d} {2:4d}'.format(x, x*x, x*x*x)
    # equal to:
    # print "  %d   %d    %d" % (x, x*x, x*x*x) "{0:2d}" means 0 the first replaceholder, : is a sign to distinguish replaceholder and argument 2d.
    # 2 means two left padding before replaceholder, d means decimal
    
    for x in range(1, 11):
        print '{0:2d} {1:3d} {2:4d}'.format(x, x*x, x*x*x)

    print "12".zfill(5)              # 00012
    print "-3.14".zfill(7)           # -003.14
    print "3.14159265359".zfill(5)   # 3.14159265359

    import math
    print '{0}, {1:.3f}, {two}'.format(1, math.pi, two='2')
    li = [1, 2]
    print '{0}, {1}'.format(*li)
    table = {"jiawei":"zhang"}
    print 'jiawei {jiawei:s}'.format(**table)

    # About read and write the file with python.
    # r w a
    # r+ means reading and writing
    # r is omitted.
    # On Windows: b for binary mode
    # so rb wb r+b is possible.

    f = open('test.py', 'r')
    print f.tell()          # f.tell() return the position of the file, since no data has been read, it is 0 now.
    print f.readline()      # f.readline() return one line
    f.read()                # f.read() until EOF while f.read(n) return up to n bytes
    print f.readline()      # when EOF, it return ''
    # f.seek(offset, from_what)
    # offset: 0 means the first one.
    # from_what: 0, 1, 2
    # 0 means from start, default
    # 1 means from current
    # 2 means from end
    # f.seek(-3, 2) means 3rd byte before end, the last one is -1 just like sequence.
    f.seek(-3, 2)                #
    print "I'm printing the result of f.seek(-3,2) for test.py: {0}".format(f.read())
    f.close()

    # Another way better to read line from a file.
    f = open('test.py', 'r')
    for line in f:
        print line
        break
    f.close()

    # Use the sample below instead of f.close() and try-finally clause to make sure the file will be closed anyway.
    with open('test.py', 'r') as f:
        for line in f:
            # see whether 'test_method' in the line, if so, print it.
            if 'test_method' in line:
                print line
    print 'f.closed: {0}'.format(f.closed)

    print

    import pickle
    with open('data', 'w') as f:
        pickle.dump(TestMe(), f)    # pickling means serialization in java
    with open('data', 'r') as f:
        tm = pickle.load(f)         # unpickling means un-serialization in java
    print "I'm running an method of object which has been dumped and loaded with pickle above, it return: {0}".format(tm.doSomething())

# Exceptions in python

    try:
        raise Exception('spam', 'eggs')    # Using clause "raise Exception" here is right as well.
    # exception (ValueError, RuntimeError, TypeError, NameError):
    # inst below is just an instance object of Exception, means "inst = Exception('spam', 'eggs')"
    except Exception as inst:
        print type(inst)
        print inst       # inst can be print because the __str__(self) method of Exception has been rewritten
        x, y = inst      # __getitem__ allows args ('spam', 'eggs') to be unpacked directly.
   # except Exception as (x, y)
   # works as well. The same to "x, y = inst" which will also triggle the __getitem__ method
        print 'x =', x
        print 'y =', y
        # raise inst     can be re-raised here or can be just "pass" here


    # Implement it like below in your own object:
    class f:
        def __init__(self, *li):
            self.value = li
        def __getitem__(self, key):
            print "get item" + str(key)
            return self.value[key]
    # usage:
    x, y, z = f(1, 2, 3)
    # result: x = 1, y = 2, z =3
    print 'x = {0}, y = {1}, z = {2}'.format(x, y, z)
    

    try:
        f = open(sys.argv[0])
    except IOError:
        print 'can not open', sys.argv[0]
    # if try clause above is executed without any error, else clause below will be executed.
    else:
        print 'can be opened'
        f.close()

    # construct, re-raise exception and finally clause
    try:
        raise NameError, 'HiThere' # same to raise NameError('HiThere')
    except NameError:
        print 'An exception flew by!'
        # raise     simply to raise the current exception without instance object following
    finally:
        print 'I am finally.'

    # user defined error
    class MyError(Exception):
        def __init__(self, value):
            self.value = value
        def __str__(self):
            return repr(self.value)

    try:
        raise MyError(2*2)
    except MyError as e:
        print 'My exception occurred, value:', e.value

# Class in python

    class MyClass:
        """ A simple example class """
        iClassVariable = 12345              # Here is the only position to put class variable.
        def __init__(self):
            self.iInstanceVariable = 54321  # Here is the only position to put instance variable.
        def return_class_variable(self):
            return  self.__class__.iClassVariable
        def return_instance_variable(self):
            return  self.iInstanceVariable

    # class doc in python
    print 'The MyClass doc: {0}'.format(MyClass.__doc__)

    print

    print 'iClassVariable can be visited as a class variable by the ways below:'
    print 'MyClass.iClassVariable: {0}'.format(MyClass.iClassVariable)
    print 'Increase the value of class variable by 1. MyClass.iClassVariable += 1'
    MyClass.iClassVariable += 1
    print 'MyClass().__class__.iClassVariable: {0}'.format(MyClass().__class__.iClassVariable)
    print 'self.__class__.iClassVariable: {0}'.format(MyClass().return_class_variable())

    print

    print 'MyClass().iClassVariable has two behaviour:'
    print 'if there is no the same name called iClassVariable for instance variable, it return class variable.'
    print 'if there is the same name called iClassVariable for instance variable, it return instance variable.'
    print 'So, use object.__class__.iClassVariable to visit class variable always while use object.iInstanceVariable to visit instance variable always.'

    print

    print 'iInstanceVariable can be visited by the ways below:'
    print 'MyClass().iInstanceVariable: {0}'.format(MyClass().iInstanceVariable)
    print 'self.iInstanceVariable: {0}'.format(MyClass().return_instance_variable())
    print 'Invoking MyClass.iInstanceVariable will definitely raise a error saying: no attribute iInstanceVariable for MyClass'
    print 'Thus, MyClass can only invoke class variable, not instance variable, not any method.'

    print

    # inheritance in python
    # define a sub class of MyClass
    class SubMyClass(MyClass):
        # iClassVariable = 222222
        def __init__(self):
            self.iInstanceVariable = 111111 # redefine the same instance, class variable in the parent class will lead to over-write happen.
        def return_class_variable(self): # No override but overwrite in python, here is the overwrite
            print MyClass.return_class_variable(self)
            return 'I am sub class overwritten method.'
            # Conclution: In the overwritten method of sub class: You have to always use parent class MyClass as prefix to invoke any method of the parent class in sub class.
        def subclass_own_method(self):
            print 'invoking a parent class method which is not overwritten: '
            # The two ways below are same.
            print self.return_instance_variable()
            print MyClass.return_instance_variable(self)
            print
            print 'invoking a parent class method which is overwritten: '
            # The two ways below are not same, the first one means invoking the overwritten method while the second one means invoking the parent's method.
            print self.return_class_variable()
            print MyClass.return_class_variable(self)
            print
            print 'invoking a subclass its own method: '
            return self.subclass_invoked_method()  # In the same class: You have to always use "self" as prefix to invoke any method in the same class. e.g. "self.subclass_invoked_method()"
        def subclass_invoked_method(self):
            print self.iInstanceVariable          # iInstanceVariable is overwritten.
            print self.__class__.iClassVariable   # iClassVariable can be overwritten but isn't here since iClassVariable = 222222 has been overwritten there.
            return 'I am sub class own method'

    subclass = SubMyClass()
    print 'invoking a parent class method which is not overwritten: '
    print subclass.return_instance_variable()
    print
    print 'invoking a parent class method which is overwritten: '
    print subclass.return_class_variable()
    print
    print 'invoking a subclass its own method: '
    print subclass.subclass_own_method()
    print
    # Conclution: 1) invoking a sub class undefined method will invoke the parent class eventually.
    #             2) invoking a sub class overwritten method will invoke the sub class method eventually.
    #             3) invoking a sub class its own method will invoke the sub class method eventually.

    print '"object" subclass is instance of "class" SubMyClass: {0}'.format(isinstance(subclass, SubMyClass))
    print '"class" SubMyClass is sub class of "class" MyClass: {0}'.format(issubclass(SubMyClass, MyClass))

    # private object
    class PrivateSample:
        __private_class_variable = 1
        def __init__(self):
            self.__private_instance_variable = 1
        def __private_method(self):
            print 'I am private method in class PrivateSample'
        def public_method(self):
            print PrivateSample.__private_class_variable
            print self.__class__.__private_class_variable
            print self.__private_instance_variable
            self.__private_method()
            self.instance_variable = 0    # you can not try to define a instance variable here. error happen when using it like PrivateSample().instance_variable, define it in __init__

    # You can not invoke any one of the object below, since they are private class variable, private instance method and private instance variable
    # PrivateSample.__private_class_variable
    # PrivateSample().__class__.__private_instance_variable
    # PrivateSample().__private_instance_variable
    # PrivateSample().__private_method()
    PrivateSample().public_method()

    # Odds and Ends
    # C struct like definition by using class:
    class Employee:
        pass
    john = Employee()
    john.name = 'John'
    john.salary = 1000

    # Look class as exception
    # the following code will print B, C, D in that order:
    class B:
        pass
    class C(B):
        pass
    class D(C):
        pass
    for c in [B, C, D]:
        try:
            raise c()    # here, you are raising an instance of class just look it as an exception.
        except D:
            print "D"
        except C:
            print "C"
        except B:
            print "B"

    # Iterators
    for key in {"one":1, "two":2}:
        print key,
    print
    for key in "123":
        print key,
    print
    # While dictionary and string sequence including other sequence can be iterated by "for ... in ..." clause?
    # Because the dictionary/sequence object implement __iter__(self) method and next(self) method, and next(self) method raise "StopIteration" eventually to stop the for clause.
    # Another way is to use a Generator, which will be easier.
    print 'Testing generator for iterator instead of implementing __iter__(self) and next(self) method.(reversing "123")'
    class Reverse:
        "Iterator for looping over a sequence backwards"
        def __init__(self, data):
            self.data = data
            self.index = len(data)
        def reverse(self):
            for index in range(self.index-1, -1, -1):
                yield self.data[index]

    x = Reverse("123")
    for char in x.reverse():
        print char, 
    # results: 3 2 1
    print
    
    # Generator Expression
    print 'Testing generator expression'
    print sum(i*i for i in range(10)) # sum of squares
    xvec = [10, 20, 30]
    yvec = [7, 5, 3]
    print sum(x*y for x,y in zip(xvec, yvec)) # dot product

    from math import pi, sin
    sine_table = dict((x, sin(x*pi/180)) for x in range(0, 91))
    print sine_table
    # unique_words = set(word for line in page for word in line.split())
    # print unique_words
    # valedictorian = max((student.gpa, student.name) for student in graduates)
    # print valedictorian
    data = 'golf'
    li = [data[i] for i in range(len(data)-1,-1,-1)]
    print li
    print


""" Below is the line can be executed by pressing <F12> or <F5> directly """
""" Can be executed in visual mode by pressing <C-F5> as well """
""" THE CODE BELOW IS DANGEROUS IN SERIOUS PROJECT!!! """
""" Because the code below will be executed once the module test.py be imported by other module. """

#print "jiawei in test.py"
#import sys
#print sys.api_version
#print sys.platform

