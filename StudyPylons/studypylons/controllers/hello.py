import logging

from pylons import request, response, session, tmpl_context as c, url
from pylons.controllers.util import abort, redirect

from studypylons.lib.base import BaseController, render

log = logging.getLogger(__name__)

# Customizing controller name:
# name with 'Controller' is default mechanism, you can change 'HelloController' to 'Hello' and then set variable '__controller__' below to customize controller name.
#__controller__ = 'Hello'

class HelloController(BaseController):

    def index(self):
        # Return a rendered template
        #return render('/hello.mako')
        # or, return a string
        return 'Hello World'

    # Prefix __ to keep method private.
    def __edit_generic(self):
        "I can't be called from web!"
        return "I can't be called from web!"

    # Special method.
    def __before__(self):
        print "I'm __before__ method will be run before any action."

    # Special method.
    def __after__(self):
        print "I'm __after__ method will be run after any action, even if it raises an Exception or redirects."

