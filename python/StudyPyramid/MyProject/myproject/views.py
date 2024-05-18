import re
from docutils.core import publish_parts

from pyramid.httpexceptions import (
    HTTPFound,
    HTTPNotFound,
    )

from pyramid.view import (
    view_config,
    forbidden_view_config,
    )

from pyramid.security import (
    remember,
    forget,
    authenticated_userid,
    )

from .models import (
    DBSession,
    Page,
    )

from .security import USERS

# The log below will leverage the logging configurations in development.ini or production.ini files.
import logging
log = logging.getLogger(__name__)

# regular expression used to find WikiWords
wikiwords = re.compile(r"\b([A-Z]\w+[A-Z]+\w+)")

@view_config(route_name='view_wiki')
def view_wiki(request):
    log.info("I'm going to redirect to FrontPage page. Here is the %s for info.", "sample")
    log.debug("I'm going to redirect to FrontPage page. Here is the %s for debug.", "sample")
    return HTTPFound(location = request.route_url('view_page',
                                                  pagename='FrontPage'))

@view_config(route_name='view_page', renderer='templates/view.pt')
def view_page(request):
    pagename = request.matchdict['pagename']
    page = DBSession.query(Page).filter_by(name=pagename).first()
    if page is None:
        return HTTPNotFound('No such page')

    def check(match):
        # match.group() the entire matched string.
        # match.group(0) the entire matched string.
        # match.group(1) the first matched string.
        # match.groups() the matched list.
        word = match.group(1)
        exists = DBSession.query(Page).filter_by(name=word).all()
        if exists:
            view_url = request.route_url('view_page', pagename=word)
            return '<a href="%s">%s</a>' % (view_url, word)
        else:
            add_url = request.route_url('add_page', pagename=word)
            return '<a href="%s">%s</a>' % (add_url, word)

    content = publish_parts(page.data, writer_name='html')['html_body']
    # jiawzhang:
    # check function will be invoked the same times as the content is matched. e.g. if content is 'ZHANG JIAWEI', it will be invoked twice.
    # first time, word = match.group(1) above is 'ZHANG', the second time, word = match.group(1) above is 'JIAWEI'.
    # Hence, the content will be '<a href="/add_page/ZHANG">ZHANG</a> <a href="/add_page/JIAWEI">JIAWEI</a>' (suppose, "exists" above is False)
    content = wikiwords.sub(check, content)
    # the edit_url for current page.
    edit_url = request.route_url('edit_page', pagename=pagename)
    # logged_in is available if you've invoking "headers = remember(request, login)" in login.py
    return dict(page=page, content=content, edit_url=edit_url, logged_in=authenticated_userid(request))

# add 'edit' permission on add_page.
@view_config(route_name='add_page', renderer='templates/edit.pt', permission='edit')
def add_page(request):
    name = request.matchdict['pagename']
    if 'form.submitted' in request.params:
        body = request.params['body']
        page = Page(name, body)
        DBSession.add(page)
        return HTTPFound(location = request.route_url('view_page',
                                                      pagename=name))
    save_url = request.route_url('add_page', pagename=name)
    page = Page('', '')
    # logged_in is available if you've invoking "headers = remember(request, login)" in login.py
    return dict(page=page, save_url=save_url, logged_in=authenticated_userid(request))

# add 'edit' permission on edit_page.
@view_config(route_name='edit_page', renderer='templates/edit.pt', permission='edit')
def edit_page(request):
    name = request.matchdict['pagename']
    page = DBSession.query(Page).filter_by(name=name).one()
    if 'form.submitted' in request.params:
        page.data = request.params['body']
        DBSession.add(page)
        return HTTPFound(location = request.route_url('view_page',
                                                      pagename=name))
    # logged_in is available if you've invoking "headers = remember(request, login)" in login.py
    return dict(
        page=page,
        save_url = request.route_url('edit_page', pagename=name),
        logged_in=authenticated_userid(request)
        )

# @forbidden_view_config here means when the permission is not enough, forbidden exception will be raised, and the view above with forbidden predication will make redirecting to login page happen.
@view_config(route_name='login', renderer='templates/login.pt')
@forbidden_view_config(renderer='templates/login.pt')
def login(request):
    login_url = request.route_url('login')
    referrer = request.url
    if referrer == login_url:
        referrer = '/' # never use the login form itself as came_from
    came_from = request.params.get('came_from', referrer)
    message = ''
    login = ''
    password = ''
    if 'form.submitted' in request.params:
        login = request.params['login']
        password = request.params['password']
        if USERS.get(login) == password:
            # jiawzhang: remember, check out http://docs.pylonsproject.org/projects/pyramid/en/1.3-branch/api/security.html
            headers = remember(request, login)
            return HTTPFound(location = came_from,
                             headers = headers)
        message = 'Failed login'

    return dict(
        message = message,
        url = request.application_url + '/login',
        came_from = came_from,
        login = login,
        password = password,
        logged_in = authenticated_userid(request)
        )

@view_config(route_name='logout')
def logout(request):
    # jiawzhang: forget, check out http://docs.pylonsproject.org/projects/pyramid/en/1.3-branch/api/security.html
    headers = forget(request)
    return HTTPFound(location = request.route_url('view_wiki'),
                     headers = headers)

