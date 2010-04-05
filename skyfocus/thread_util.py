import sys
import threading
import traceback
from Queue import Queue
from Queue import Empty
from Queue import Full

class KThread(threading.Thread):
    """A subclass of threading.Thread, with a kill()
    method.
    
    Come from:
    Kill a thread in Python: 
    http://mail.python.org/pipermail/python-list/2004-May/260937.html
    """
    def __init__(self, *args, **kwargs):
        threading.Thread.__init__(self, *args, **kwargs)
        self.killed = False

    def start(self):
        """Start the thread."""
        self.__run_backup = self.run
        self.run = self.__run      # Force the Thread to install our trace.
        threading.Thread.start(self)

    def __run(self):
        """Hacked run function, which installs the
        trace."""
        sys.settrace(self.globaltrace)
        self.__run_backup()
        self.run = self.__run_backup

    def globaltrace(self, frame, why, arg):
        if why == 'call':
            return self.localtrace
        else:
            return None

    def localtrace(self, frame, why, arg):
        if self.killed:
            if why == 'line':
                raise SystemExit()
        return self.localtrace

    def kill(self):
        self.killed = True

class Producer:
    def __init__(self, timerSeconds = None):
        "Set timerSeconds not None means, the producer thread is a polling thread with interval of time."
        self.__timerSeconds = timerSeconds
    
    def getTimerSeconds(self):
        try:
            return self.__timerSeconds
        except AttributeError:
            print 'No timerSeconds was set, assign None instead.'
            return None
        
    def process(self, channel):
        """1. Same to Request.doAction below, be careful to add time.sleep method.
           2. If this method maybe throw exceptions, make sure you'v try-except it.
              If you invoked channel.putRequest(), it may raise FullQueue Exception(see below), re-raise it if you want to stop current producer thread.
              
			    try:
			        # (block = False) or (block = True and timeout is not None) may cause FullQueue Exception when the queue is full.
			        channel.putRequest(request, False) 
			    except FullQueue as full:
			        raise full
			    except Exception as e:
			        print "I'm handling."
        """
        pass

class ProducerThread(KThread):
    def __init__(self, queue, channel, producer):
        KThread.__init__(self)
        self.__queue = queue
        self.__channel = channel
        self.__producer = producer
        self.__timerSeconds = producer.getTimerSeconds()
        self.__finished = threading.Event() if producer.getTimerSeconds() else None
        
    def shutdown(self):
        self.__queue.not_full.acquire()
        try:
            self.__queue.not_full.notifyAll()
            if self.__finished:
                self.__finished.set()
            self.kill()
        finally:
            self.__queue.not_full.release()
            
    def run(self):
        try:
            while True:
                try:
                    self.__producer.process(self.__channel)
                except FullQueue as full:
                    raise full
                except Exception:
                    print 'unexpected exception happened. please try-except your code in Producer.process(channel) method.'
                    print traceback.format_exc()
                    
                if self.__finished:
                    self.__finished.wait(self.__timerSeconds)
                else:
                    break
        except FullQueue as full:
            print 'request queue is full, so stop producer thread.'
            print full

class ConsumerThread(KThread):
    def __init__(self, queue, block = True, timeout = None):
        KThread.__init__(self)
        self.__queue = queue
        self.__block = block
        self.__timeout = timeout
    
    def shutdown(self):
        self.__queue.not_empty.acquire()
        try:
            self.__queue.not_empty.notifyAll()
            self.kill()
        finally:
            self.__queue.not_empty.release()
        
    def run(self):
        try:
            while True:
                request = self.__queue.get(self.__block, self.__timeout)
                try:
                    request.doAction()
                except Exception:
                    print 'unexpected exception happened. please try-except your code in request.doAction()  method.'
                    print traceback.format_exc()
        except Empty as empty:
            print 'no more request in queue, so stop consumer thread.'
            print empty

class Request:
    def doAction(self):
        """ 1. Be care to use time.sleep method in this method, channel will fail to stop consumer threads in the sleep time.
            2. If this method maybe throw exceptions, make sure you'v try-except it.
        """
        pass
    
class FullQueue(Exception):
    pass

class Channel:
    def __init__(self, maxsize = 0):
        self.__queue = Queue(maxsize)
        self.__consumers = None
        self.__producers = None
    
    def putRequest(self, request, block = True, timeout = None):
        "Use this method to put request and if (block = True and timeout is not None) or (block = False), it throw FullQueue exception when the queue is full."
        try:
            self.__queue.put(request, block, timeout)
        except Full as full:
            raise FullQueue(full)
    
    def size(self):
        return self.__queue.qsize()
    
    def startConsumer(self, maxsize, block = True, timeout = None):
        "Set block = False, if you want to stop consumers once there is no more request. But invoke putRequest() above first if this is the case."
        self.__consumers = [ConsumerThread(self.__queue, block, timeout) for i in xrange(maxsize)]
        for consumerThread in self.__consumers:
            consumerThread.start()
            
    def stopConsumer(self):
        for consumerThread in self.__consumers:
            consumerThread.shutdown()
    
    def waitingForConsumerExist(self):
        for consumerThread in self.__consumers:
            consumerThread.join()
    
    def startProducer(self, producers):
        "producers: must be the list of thread_util.Producer's subclass. The size of producers will decide the number of producer threads."
        self.__producers = [ProducerThread(self.__queue, self, producer) for producer in producers]
        for producerThread in self.__producers:
            producerThread.start()
    
    def stopProducer(self):
        for producerThread in self.__producers:
            producerThread.shutdown()
        
    def waitingForProducerExist(self):
        for producerThread in self.__producers:
            producerThread.join()
            
if __name__ == '__main__':
    
    import time
    
    channel = Channel()
    
    print 'begin to test 1st part.'
    class MyRequest(Request):
        def doAction(self):
            print '1st test case.'
    channel.putRequest(MyRequest())
    channel.startConsumer(1)
    time.sleep(5)
    channel.stopConsumer()
    channel.waitingForConsumerExist()
    print 'finish to test 1st part.\n'
    
    print 'begin to test 2nd part.'
    class MyProducer(Producer):
        def process(self, channel):
            print '2nd test case.'
        
    channel.startProducer([MyProducer()])
    print 'finish to test 2nd part.\n'
    
    print 'begin to test 3rd part.'
    class MyProducer2(Producer):
        def process(self, channel):
            print '3rd test case.'
        
    channel.startProducer([MyProducer2(2)])
    time.sleep(5)
    channel.stopProducer()
    channel.waitingForProducerExist()
    print 'finish to test 3rd part.\n'
    
    print 'begin to test 4th part.'
    class MyRequest2(Request):
        def doAction(self):
            print threading.currentThread().getName() + ': 4th test case consumer doAction.'
            
    class MyProducer3(Producer):
        def process(self, channel):
            channel.putRequest(MyRequest2())
            print threading.currentThread().getName() + ': 4th test case producer process.'
        
    channel.startProducer([MyProducer3(1), MyProducer3(2)])
    channel.startConsumer(2)
    time.sleep(4)
    channel.stopConsumer()
    channel.waitingForConsumerExist()
    channel.stopProducer()
    channel.waitingForProducerExist()
    print 'finish to test 4th part.\n'
    
