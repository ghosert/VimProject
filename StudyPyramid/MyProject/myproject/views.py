import re
from docutils.core import publish_parts

from pyramid.httpexceptions import (
    HTTPFound,
    HTTPNotFound,
    )
from pyramid.view import view_config

from .models import (
    DBSession,
    Page,
    )

# regular expression used to find WikiWords
wikiwords = re.compile(r"\b([A-Z]\w+[A-Z]+\w+)")

@view_config(route_name='view_wiki')
def view_wiki(request):
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
    return dict(page=page, content=content, edit_url=edit_url)

@view_config(route_name='add_page', renderer='templates/edit.pt')
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
    return dict(page=page, save_url=save_url)

@view_config(route_name='edit_page', renderer='templates/edit.pt')
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
        )

