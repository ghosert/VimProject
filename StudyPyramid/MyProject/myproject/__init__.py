from pyramid.config import Configurator
from sqlalchemy import engine_from_config

from myproject.models import initialize_sql

def main(global_config, **settings):
    """ This function returns a WSGI application.
    """
    # jiawzhang: Create a SQLAlchemy database engine from the sqlalchemy. prefixed settings in the development.ini file's [app:myproject] section. This will be a URI (something like sqlite://).
    engine = engine_from_config(settings, 'sqlalchemy.')
    initialize_sql(engine)
    # jiawzhang: settings contains deployment-related values such as reload_templates, db_string, etc.
    config = Configurator(settings=settings)
    config.add_static_view('static', 'myproject:static')
    config.add_route('view_wiki', '/', view='myproject.views.view_wiki')
    config.add_route('view_page', '/{pagename}',
                     view='myproject.views.view_page',
                     view_renderer='myproject:templates/view.pt')
    config.add_route('add_page', '/add_page/{pagename}',
                     view='myproject.views.add_page',
                     view_renderer='myproject:templates/edit.pt')
    config.add_route('edit_page', '/{pagename}/edit_page',
                     view='myproject.views.edit_page',
                     view_renderer='myproject:templates/edit.pt')
    return config.make_wsgi_app()

