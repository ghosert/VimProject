class TestInnerFunCallback():

    def libfun(self, callback):
        print 'start'
        firstname = 'jiawei'
        callback(firstname)
        print 'end'

    def test(self):

        lastname = 'zhang'

        def callback(firstname):
            print lastname, firstname

        self.libfun(callback)

print 'Testing inner function callback.'
TestInnerFunCallback().test()
print

class TestInnerClassCallback():

    def libclass(self, callback):
        print 'start'
        firstname = 'jiawei'
        callback.doit(firstname)
        print 'end'

    def test(self):

        lastname = 'zhang'

        class Callback:
            def doit(self, firstname):
                print lastname, firstname

        self.libclass(Callback())

print 'Testing inner class callback.'
TestInnerClassCallback().test()
print

# In this case UncoupledObjectA and UncoupledObjectB don't know each other, they are using channel to coummunicate.
def TestInnerClassUnCoupled():
    class Observer:
        def doit(self, param):
            pass

    class Channel:
        def addObserver(self, observer):
            self.observer = observer
        def notifyObserver(self, param):
            self.observer.doit(param)

    class UnCoupledObjectA:
        def __init__(self, channel):
            self.channel = channel

        def doSomething(self, lastname):

            class NameObserver(Observer):
                def doit(self, firstname):
                    print lastname, firstname

            self.channel.addObserver(NameObserver())

    class UnCoupledObjectB:
        def __init__(self, channel):
            self.channel = channel

        def doSomething(self, firstname):
            self.channel.notifyObserver(firstname)

    print 'test'
    channel = Channel()
    UnCoupledObjectA(channel).doSomething('zhang')
    UnCoupledObjectB(channel).doSomething('jiawei')

print 'Testing inner class uncoupled.'
TestInnerClassUnCoupled()
print

# In this case UncoupledObjectA and UncoupledObjectB don't know each other, they are using signal/slot to coummunicate.
from PyQt4.QtCore import *

class UnCoupledObjectA:
    def __init__(self, lastname):
        self.lastname = lastname

    def doSomething(self, firstname):
        print self.lastname, firstname

class UnCoupledObjectB(QObject):
    def __init__(self):
        QObject.__init__(self)
    def doSomething(self, firstname):
        QObject.emit(self, SIGNAL('param'), firstname)

class TestSignalSlotUncoupled(QObject):
    def __init__(self):
        QObject.__init__(self)

    def test(self):
        ucoa = UnCoupledObjectA('zhang')
        ucob = UnCoupledObjectB()
        self.connect(ucob, SIGNAL('param'), ucoa.doSomething)
        ucob.doSomething('jiawei')

print 'Testing signal slot uncoupled.'
TestSignalSlotUncoupled().test()
print


