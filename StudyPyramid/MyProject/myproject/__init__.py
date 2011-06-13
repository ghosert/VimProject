from pyramid.config import Configurator
from pyramid.authentication import AuthTktAuthenticationPolicy
from pyramid.authorization import ACLAuthorizationPolicy

from sqlalchemy import engine_from_config

from myproject.models import initialize_sql

from myproject.security import groupfinder

def main(global_config, **settings):
    """ This function returns a WSGI application.
    """
    # jiawzhang: Create a SQLAlchemy database engine from the sqlalchemy. prefixed settings in the development.ini file's [app:myproject] section. This will be a URI (something like sqlite://).
    engine = engine_from_config(settings, 'sqlalchemy.')
    initialize_sql(engine)

    authn_policy = AuthTktAuthenticationPolicy(
        'sosecret', callback=groupfinder)
    authz_policy = ACLAuthorizationPolicy()

    # jiawzhang: settings contains deployment-related values such as reload_templates, db_string, etc.
    # The root_factory, authentication_policy and authorization_policy is optional if you don't care authentication and authorization.
    config = Configurator(settings=settings,
                          root_factory='myproject.models.RootFactory',
                          authentication_policy=authn_policy,
                          authorization_policy=authz_policy)

    config.add_static_view('static', 'myproject:static')
    config.add_route('login', '/login',
                     view='myproject.login.login',
                     view_renderer='myproject:templates/login.pt')
    config.add_route('logout', '/logout',
                     view='myproject.login.logout')
    config.add_route('view_wiki', '/', view='myproject.views.view_wiki')
    config.add_route('view_page', '/{pagename}',
                     view='myproject.views.view_page',
                     view_renderer='myproject:templates/view.pt')
    # add 'edit' permission required on add_page.
    config.add_route('add_page', '/add_page/{pagename}',
                     view='myproject.views.add_page',
                     view_renderer='myproject:templates/edit.pt',
                     view_permission='edit')
    # add 'edit' permission required on edit_page.
    config.add_route('edit_page', '/{pagename}/edit_page',
                     view='myproject.views.edit_page',
                     view_renderer='myproject:templates/edit.pt',
                     view_permission='edit')
    # when the permission is not enough, forbidden exception will be raised, and the view above with forbidden predication will make redirecting to login page happen.
    config.add_view('myproject.login.login',
                    renderer='myproject:templates/login.pt',
                    context='pyramid.exceptions.Forbidden')
    return config.make_wsgi_app()

