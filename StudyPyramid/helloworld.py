from pyramid.config import Configurator
from pyramid.response import Response
from paste.httpserver import serve

def hello_world(request):
    return Response('Hello world!')

def goodbye_world(request):
    return Response('Goodbye world!')

if __name__ == '__main__':
    config = Configurator()
    config.add_view(hello_world)
    config.add_view(goodbye_world, name='goodbye')
    app = config.make_wsgi_app()
    serve(app, host='0.0.0.0')

