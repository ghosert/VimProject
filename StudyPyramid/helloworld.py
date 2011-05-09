from pyramid.config import Configurator
from pyramid.response import Response
from paste.httpserver import serve

from pyramid.view import view_config

def hello_world(request):
    return Response('Hello world!')

def goodbye_world(request):
    return Response('Goodbye world!')

@view_config(name='hello')
def hello(request):
    return Response('Hello Declarative Configuration!')

if __name__ == '__main__':
    config = Configurator()
    config.add_view(hello_world)
    config.add_view(goodbye_world, name='goodbye')
    # config.scan() will take care all the method with @view_config annotation, this is called declartive configuration.
    # And it has same side effect to imperative way: config.add_view(hello, name='hello')
    config.scan()
    app = config.make_wsgi_app()
    serve(app, host='0.0.0.0')

