import logging

from pylons import request, response, session, tmpl_context as c, url
from pylons.controllers.util import abort, redirect

from studypylons.lib.base import BaseController, render

log = logging.getLogger(__name__) # __name__ here is full qualified name: 'studypylons.controllers.hello'

# Customizing controller name:
# name with 'Controller' is default mechanism, you can change 'HelloController' to 'Hello' and then set variable '__controller__' below to customize controller name.
#__controller__ = 'Hello'

class HelloController(BaseController):

    def index(self):
        content_type = 'text/plain'
        content = 'Hello World!'

        log.debug('Returning: %s (content-type: %s)', content, content_type)
        response.content_type = content_type
        return content

    # To use mako template, create a folder 'studypylons/templates' and put the file 'sample.mako' in it.
    # see more default template variables in 'sample.mako' file.
    # Template engine can be configured in config/environment.py
    # Dynamic web content is located at studypylons/templates
    # Static files is located at studypylons/public
    def sample(self):
        c.name = 'Zhang Jiawei'
        return render('/sample.mako')

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

