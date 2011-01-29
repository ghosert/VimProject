import logging

from pylons import request, response, session, tmpl_context as c, url
from pylons.controllers.util import abort, redirect

from studypylons.lib.base import BaseController, render

from cgi import escape

log = logging.getLogger(__name__)

from studypylons.lib.base import Session
from studypylons.model import Person

import webhelpers.paginate

class PersonController(BaseController):

    def __before__(self):
        self.person_q = Session.query(Person) # no db performing so far.

    def index(self):
        # c.names = [person.name for person in self.person_q.all()] # self.person_1.all() method here will cause the db performing.

        # pagination is smart enough to get only 10 rows belong to the page 1 via LIMIT/OFFSET in the actual SQL query.
        iPage = 1
        try:
            iPage = int(request.params['page'])
        except:
            pass
        c.persons = webhelpers.paginate.Page(self.person_q, iPage, items_per_page = 10)

        # Paging over an SQLAlchemy select
        # selection = sqlalchemy.select([Employee.c.first_name])
        # page2 = webhelpers.paginate.Page(selection, page = 2, items_per_page = 10, sqlalchemy_session = Session)
        # for first_name in page2: print first_name

        c.page2 = webhelpers.paginate.Page(range(1, 201), page = 10, items_per_page = 10)
        return render('person.mako')

    def create(self, name, email):
        person = Person(name = name, email = email)
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
