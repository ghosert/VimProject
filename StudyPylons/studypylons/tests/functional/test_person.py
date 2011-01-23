from studypylons.tests import *

class TestPersonController(TestController):

    def test_index(self):
        response = self.app.get(url(controller='person', action='index'))
        # Test response...
