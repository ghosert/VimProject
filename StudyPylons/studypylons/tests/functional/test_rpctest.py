from studypylons.tests import *

class TestRpctestController(TestController):

    def test_index(self):
        response = self.app.get(url(controller='rpctest', action='index'))
        # Test response...
