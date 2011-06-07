from pyramid.config import Configurator
from sqlalchemy import engine_from_config

from myproject.models import initialize_sql

def main(global_config, **settings):
    """ This function returns a Pyramid WSGI application.
    """
    # jiawzhang: Create a SQLAlchemy database engine from the sqlalchemy. prefixed settings in the development.ini file’s [app:tutorial] section. This will be a URI (something like sqlite://).
    engine = engine_from_config(settings, 'sqlalchemy.')
    initialize_sql(engine)
    # jiawzhang: settings contains deployment-related values such as reload_templates, db_string, etc.
    config = Configurator(settings=settings)
    config.add_static_view('static', 'myproject:static')
    config.add_route('home', '/', view='myproject.views.my_view',
                     view_renderer='templates/mytemplate.pt')
    return config.make_wsgi_app()

