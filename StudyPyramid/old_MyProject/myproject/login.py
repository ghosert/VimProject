from pyramid.httpexceptions import HTTPFound
from pyramid.security import remember
from pyramid.security import forget
from pyramid.url import route_url

from myproject.security import USERS

def login(request):
    login_url = route_url('login', request)
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
            # jiawzhang: remember, check out http://docs.pylonsproject.org/projects/pyramid/1.0/api/security.html
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
        )
    
def logout(request):
    # jiawzhang: forget, check out http://docs.pylonsproject.org/projects/pyramid/1.0/api/security.html
    headers = forget(request)
    return HTTPFound(location = route_url('view_wiki', request),
                     headers = headers)

