from studypylons.tests import *

class TestShowconfigController(TestController):

    def test_index(self):
        response = self.app.get(url(controller='showconfig', action='index'))
        # Test response...
