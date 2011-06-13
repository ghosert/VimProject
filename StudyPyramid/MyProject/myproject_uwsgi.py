# This py will be invoked in uwsgi.xml

from pyramid.paster import get_app
application = get_app(
  '/home/jiawzhang/VimProject/StudyPyramid/MyProject/production.ini', 'main')

