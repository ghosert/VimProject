from wsgiref.simple_server import make_server
from pyramid.config import Configurator
from pyramid.response import Response
from pyramid.view import view_config

def hello_world(request):
    return Response('Hello world!')

def goodbye_world(request):
    return Response('Goodbye world!')

@view_config(name = 'hello')
def hello(request):
    return Response('Hello Declarative Configuration!')

if __name__ == '__main__':
    config = Configurator()
    config.add_view(hello_world) # Imperative Configuration, without 'name' specified, it means root context '/'.
    config.add_view(goodbye_world, name='goodbye')
    # config.scan() will take care all the method with @view_config annotation, this is called declarative configuration.
    # And it has same side effect to imperative way: config.add_view(hello, name='hello')
    config.scan() # Declarative Configuration
    app = config.make_wsgi_app()
    server = make_server('0.0.0.0', 8080, app)
    server.serve_forever()

