import logging

from pylons import request, response, session, tmpl_context as c, url
from pylons.controllers.util import abort, redirect

from studypylons.lib.base import BaseController, render

log = logging.getLogger(__name__)

from pylons import config
from paste.deploy.converters import asbool

class ShowconfigController(BaseController):

    def index(self):
        c.config = config
        debug = asbool(config['debug']) # convert string to boolean here.
        c.config['debug'] = debug
        return render('/showconfig.mako')
