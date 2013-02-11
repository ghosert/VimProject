from pyramid.config import Configurator
from pyramid.authentication import AuthTktAuthenticationPolicy
from pyramid.authorization import ACLAuthorizationPolicy

from sqlalchemy import engine_from_config

from myproject.security import groupfinder

from .models import DBSession

def main(global_config, **settings):
    """ This function returns a Pyramid WSGI application.
    """
    # jiawzhang: Create a SQLAlchemy database engine from the sqlalchemy. prefixed settings in the development.ini file's [app:main] section. This will be a URI (something like sqlite://). "settings" here can access any key/value pair in development.ini file's [app:main] section.
    engine = engine_from_config(settings, 'sqlalchemy.')
    DBSession.configure(bind=engine)

    authn_policy = AuthTktAuthenticationPolicy(
        'sosecret', callback=groupfinder)
    authz_policy = ACLAuthorizationPolicy()


    # jiawzhang: settings contains deployment-related values such as reload_templates, db_string, etc.
    # The root_factory, authentication_policy and authorization_policy is optional if you don't care authentication and authorization.
    config = Configurator(settings=settings,
                          root_factory='myproject.models.RootFactory')
    config.set_authentication_policy(authn_policy)
    config.set_authorization_policy(authz_policy)

    config.add_static_view('static', 'static', cache_max_age=3600)
    config.add_route('view_wiki', '/')
    config.add_route('login', '/login')
    config.add_route('logout', '/logout')
    config.add_route('view_page', '/{pagename}')
    config.add_route('add_page', '/add_page/{pagename}')
    config.add_route('edit_page', '/{pagename}/edit_page')

    config.scan()
    return config.make_wsgi_app()

