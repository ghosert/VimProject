""" This is a test to study Python """

class TestMe:
    def doSomething(self):
        print "I'm doing something here in TestMe."

def test_method():
    print "I'm test_method()"

if __name__ == "__main__":
    testMe = TestMe()
    testMe.doSomething()
    test_method()

