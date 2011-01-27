from studypylons.tests import *
from studypylons.lib.base import Session
from studypylons.model import Person

class TestModels(TestController):

    def setUp(self):
        Session.remove()

    def test_index(self):
        # test your models
        persons = Session.query(Person).all()
        for person in persons:
            print person.name, person.email

