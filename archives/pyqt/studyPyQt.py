from PyQt4.QtGui import *
from PyQt4.QtCore import *

# an example for non-short circuit python signal, which there is at lease one argument in signal, here is SIGNAL('numChanged(int, int)')
# for non-short circuit python signal and stardard QT signal:
# 1. they will convert python object from/to C++ data type.
# 2. the number of arguments of slots for them is <= the number of arguments of signals, here slot is "numberListener(num, num2)",
#    there are two arguments num, num2 which are same to signal has, the data types between signal and slot should the same correspondingly.
# 3. because of point 1 above, non-short circuit python signal and stardard QT signal support both the forms
#    's.connect(w, SIGNAL("XXX(...)"), instance, SLOT("ooo(...)"))' and 's.connect(w, SIGNAL("XXX(...)"), callableObject)'

class MyObject(QObject):
    def __init__(self):
        QObject.__init__(self)
        self.num = 1

    def setNumber(self, num):
        self.num = num
        QObject.emit(self, SIGNAL('numChanged(int, int)'), num, num + 1)

def numberListener(num, num2):
    print "number is changed to {0}, {1}".format(num, num2)

obj = MyObject()
obj.connect(obj, SIGNAL('numChanged(int, int)'), numberListener)
obj.setNumber(10)

# an example for short circuit python signal, which there is no argument in signal, here is SIGNAL('numChanged')
# for short circuit python signal:
# 1. it will never convert python object from/to C++ data type.
# 2. it can emit any number of arguments and passed python object directly, without C++ data type conversion.
# 3. because of point 1 above, short circuit python signal only supports the form
#    's.connect(w, SIGNAL("XXX"), callableObject)'

class MyObject2(QObject):
    def __init__(self):
        QObject.__init__(self)
        self.num = 1

    def setNumber(self, num):
        self.num = num
        QObject.emit(self, SIGNAL('numChanged'), num, num + 1)

def numberListener2(num, num2):
    print "number is changed to {0}, {1}".format(num, num2)

obj = MyObject2()
obj.connect(obj, SIGNAL('numChanged'), numberListener2)
obj.setNumber(10)


# THERE ARE ONLY THE FORMS BELOW TO USE SIGNAL AND SLOT:
# s.connect(w, SIGNAL('stardardSignal(int)'), instance, SLOT('stardardSlot(int)'))
# s.connect(w, SIGNAL('stardardSignal(int)'), instance.stardardSlot | instance.selfMethod) #instance.selfMethod has no form of 'instance, SLOT('selfMethod(int)')', only stardard qt slot has
# s.connect(w, SIGNAL('selfSignal'), instance.stardardSlot | instance.selfMethod)

# ONCE YOU WANT TO USE THE FORM WITH 'instance.stardardSlot | instance.selfMethod', YOU ARE SAFE TO USE BOTH SIGNAL("stardardSignal(int)") or SIGNAL("selfSignal")
# ONCE YOU WANT TO USE THE FORM WITH 'instance, SLOT("stardardSlot(int)")', YOU SHOULD ONLY USE SIGNAL("stardardSignal(int)")

#FINAL CONCULUSION, TO MAKE THE THINGS CLEAR, IN PYQT ONLY USE TWO FORMS BELOW:
# For self emitted signal:
# s.connect(w, SIGNAL('selfSignal'), instance.stardardSlot | instance.selfMethod)
# For stardard QT signal:
# s.connect(w, SIGNAL('stardardSignal(int)'), instance.stardardSlot | instance.selfMethod)

