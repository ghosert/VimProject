import re

from docutils.core import publish_parts

from pyramid.httpexceptions import HTTPFound
from pyramid.security import authenticated_userid
from pyramid.url import route_url

from myproject.models import DBSession
from myproject.models import Page

# regular expression used to find WikiWords
wikiwords = re.compile(r"\b([A-Z]\w+[A-Z]+\w+)")

def view_wiki(request):
    return HTTPFound(location = route_url('view_page', request,
                                          pagename='FrontPage'))

def view_page(request):
    matchdict = request.matchdict
    session = DBSession()
    page = session.query(Page).filter_by(name=matchdict['pagename']).one()

    def check(match):
        # match.group() the entire matched string.
        # match.group(0) the entire matched string.
        # match.group(1) the first matched string.
        # match.groups() the matched list.
        word = match.group(1)
        exists = session.query(Page).filter_by(name=word).all()
        if exists:
            view_url = route_url('view_page', request, pagename=word)
            return '<a href="%s">%s</a>' % (view_url, word)
        else:
            add_url = route_url('add_page', request, pagename=word)
            return '<a href="%s">%s</a>' % (add_url, word)

    content = publish_parts(page.data, writer_name='html')['html_body']
    # jiawzhang:
    # check function will be invoked the same times as the content is matched. e.g. if content is 'ZHANG JIAWEI', it will be invoked twice.
    # first time, word = match.group(1) above is 'ZHANG', the second time, word = match.group(1) above is 'JIAWEI'.
    # Hence, the content will be '<a href="/add_page/ZHANG">ZHANG</a> <a href="/add_page/JIAWEI">JIAWEI</a>' (suppose, "exists" above is False)
    content = wikiwords.sub(check, content)
    # the edit_url for current page.
    edit_url = route_url('edit_page', request, pagename=matchdict['pagename'])
    # logged_in is available if you've invoking "headers = remember(request, login)" in login.py
    logged_in = authenticated_userid(request)
    return dict(page=page, content=content, edit_url=edit_url, logged_in=logged_in)


def add_page(request):
    name = request.matchdict['pagename']
    if 'form.submitted' in request.params:
        session = DBSession()
        body = request.params['body']
        page = Page(name, body)
        session.add(page)
        return HTTPFound(location = route_url('view_page', request,
                                              pagename=name))
    save_url = route_url('add_page', request, pagename=name)
    page = Page('', '')
    # logged_in is available if you've invoking "headers = remember(request, login)" in login.py
    logged_in = authenticated_userid(request)
    return dict(page=page, save_url=save_url, logged_in=logged_in)
    
def edit_page(request):
    name = request.matchdict['pagename']
    session = DBSession()
    page = session.query(Page).filter_by(name=name).one()
    if 'form.submitted' in request.params:
        page.data = request.params['body']
        session.add(page)
        return HTTPFound(location = route_url('view_page', request,
                                              pagename=name))
    # logged_in is available if you've invoking "headers = remember(request, login)" in login.py
    logged_in = authenticated_userid(request)
    return dict(
        page=page,
        save_url = route_url('edit_page', request, pagename=name),
        logged_in=logged_in
        )

