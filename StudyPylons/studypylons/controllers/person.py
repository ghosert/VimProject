import logging

from pylons import request, response, session, tmpl_context as c, url
from pylons.controllers.util import abort, redirect

from studypylons.lib.base import BaseController, render

log = logging.getLogger(__name__)

from studypylons.lib.base import Session
from studypylons.model import Person

class PersonController(BaseController):

    def __before__(self):
        self.person_q = Session.query(Person)

    def index(self):
        # c.names = [person.name for person in self.person_q.all()]
        c.persons = self.person_q.all()
        return render('person.mako')

    def create(self, name, email):
        person = Person(name = name, email = email)
        print name, email
        Session.add(person)
        Session.commit()
        redirect('/person/index')

    def save(self, id):
        person = self.person_q.filter_by(id = id).first()
        person.name = escape(request.POST.getone('name'))
        person.email = escape(request.POST.getone('email'))
        Session.commit()
        redirect('/person/index')

    def delete(self, id):
        person = self.person_q.filter_by(id = id).first()
        Session.delete(person)
        Session.commit()
        redirect('/person/index')
