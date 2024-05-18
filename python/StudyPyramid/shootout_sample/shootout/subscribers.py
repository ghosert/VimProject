from pyramid.httpexceptions import HTTPForbidden
from pyramid.renderers import get_renderer
from pyramid.events import (
    subscriber,
    BeforeRender,
    NewRequest,
    )

@subscriber(BeforeRender)
def add_base_template(event):
    base = get_renderer('templates/base.pt').implementation()
    event.update({'base': base})

@subscriber(NewRequest)
def csrf_validation(event):
    if event.request.method == "POST":
        token = event.request.POST.get("_csrf")
        if token is None or token != event.request.session.get_csrf_token():
            raise HTTPForbidden('CSRF token is missing or invalid')
