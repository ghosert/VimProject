"""Routes configuration

The more specific and detailed routes should be defined first so they
may take precedent over the more generic routes. For more information
refer to the routes manual at http://routes.groovie.org/docs/
"""
from routes import Mapper

def make_map(config):
    """Create, configure and return the routes Mapper"""
    map = Mapper(directory=config['pylons.paths']['controllers'],
                 always_scan=config['debug'])
    map.minimization = False
    map.explicit = False

    # The ErrorController route (handles 404/500 error pages); it should
    # likely stay at the top, ensuring it can always be resolved
    map.connect('/error/{action}', controller='error')
    map.connect('/error/{action}/{id}', controller='error')

    # CUSTOM ROUTES HERE

    map.connect('/{controller}/{action}')
    map.connect('/{controller}/{action}/{id}')


    # The default mapping above will lead to match below:
    # /hello/index      >> controller: hello, action: index
    # /entry/view/4     >> controller: entry, action: view, id: 4
    # /comment/edit/2   >> controller: comment, action: edit, id: 2


    # regular express sample, match digits only:
    # map.connect('/{controller}/{action}/{id:\d+}')


    # If the desired regular expression includes the {}, for example: To limit the {id} to only match at least 2-4 digits:
    # map.connect('/{controller}/{action}/{id}',  requirements=dict(id='\d{2,4}'))


    # The controller and action can also be specified as keyword arguments so that they don't need to included in the URL:
    #
    # Archives by 2 digit year -> /archives/08
    # map.connect('/archives/{year:\d\d}', controller='articles',  action='archives')
    #
    # class ArticlesController(BaseController):
    #     def archives(self, year):
    #         ...
    # Then year above is 08


    # Controllers can be organized into directories as well:
    # paster controller admin/comments
    # /admin/comments/index    >> controller: admin/comments, action: index


    # Adding a route to match /
    # map.connect('/', controller = 'main', action = 'index')


    # Generating URLs
    # from pylons import url
    # url(controller = 'content', action = 'view', id = 2) >> Generates /content/view/2
    # url.current() >> Generates /content/view/3 during a request for /content/view/3
    # url.current(id = 2) >> Generates /content/view/2 during a request for /content/view/3


    # routes for PersonController in person.py, create person by name and email info.
    map.connect('/{controller}/{action}/{name}/{email}')


    # This is dedicated for RpctestController class in studypylons/controllers/rpctest.py
    map.connect('/{controller}/')


    return map

