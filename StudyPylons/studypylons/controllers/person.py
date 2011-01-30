import logging

from pylons import request, response, session, tmpl_context as c, url
from pylons.controllers.util import abort, redirect

from studypylons.lib.base import BaseController, render

from cgi import escape

log = logging.getLogger(__name__)

import os, shutil

from studypylons.lib.base import Session
from studypylons.model import Person

import webhelpers.paginate

permanent_store = '/home/jiawzhang/Templates/'

class PersonController(BaseController):

    def __before__(self):
        self.person_q = Session.query(Person) # no db performing so far.

    def index(self):
        # c.names = [person.name for person in self.person_q.all()] # self.person_1.all() method here will cause the db performing.

        # pagination is smart enough to get only 10 rows belong to the page 1 via LIMIT/OFFSET in the actual SQL query.
        iPage = request.params.get('page', 1) # If there is no 'page' key, 1 is the default value for the case. The key submitted without value will get empty string.
        # print request.params['page'] # If you make sure page passed in always, this is ok, otherwise, it raised a key not found exception, use the invoking above to handle this.
        # print request.params.get('page') # This invoking is equivalent to above.
        # print request.params.getall('page') # If you have multiple same key name, the invoking above will return the first value, to get all, using this, it returns a list contains all values.
        # print request.params.getone('page') # If you hope unique key name submitted only, otherwise raising error, use this.

        c.persons = webhelpers.paginate.Page(self.person_q, iPage, items_per_page = 10)
        # Put the line below to person.mako to show the paginations link, but it will show only when the items created are more than items_per_page=10
        # ${c.employees.pager('Page $page: $link_previous $link_next ~4~')}
        # The line below with an addtional "onclick" parameter is for partial updates with AJAX, see more details on pylons official documents.
        # ${c.employees.pager('Page $page: $link_previous $link_next ~4~', onclick="$('#my-page-area').load('%s'); return false;")}


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
        person.name = request.POST.getone('name')
        log.debug("reqeust.POST.getone('name') is same to request.params.getone('name'): %s", person.name)
        person.email = request.POST.getone('email')
        Session.commit()
        redirect('/person/index')

    def delete(self, id):
        person = self.person_q.filter_by(id = id).first()
        Session.delete(person)
        Session.commit()
        redirect('/person/index')

    def upload(self):
        myfile = request.POST['myfile']
        # Suppose
        # permanent_store = '/home/jiawzhang/Templates/'    myfile.filename = '/testfile.txt'
        # os.path.join(permanent, myfile.filename.lstrip(os.sep)) = '/home/jiawzhang/Templates/testfile.txt'
        # remember to lstrip the filename always otherwise you will get '/testfile.txt' in this case.
        myfile.file.seek(0, 2)
        filesize = myfile.file.tell()
        myfile.file.seek(0)
        if filesize > 1024 * 1024:
            return "The size of file is more than 1M."
        else:
            permanent_file = open(os.path.join(permanent_store, myfile.filename.lstrip(os.sep)), 'w')
            shutil.copyfileobj(myfile.file, permanent_file)
            myfile.file.close()
            permanent_file.close()
            return 'Successfully uploaded: %s, size: %.2fKB, description: %s' % (myfile.filename, filesize/1024.0, request.POST['description'])

