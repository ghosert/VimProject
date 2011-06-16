# This py will be invoked in uwsgi.xml

# Testing: For uwsgi file logging from pyramid, the location of the log file is defined in production.ini
from paste.script.util.logging_config import fileConfig
fileConfig('/home/jiawzhang/VimProject/StudyPyramid/MyProject/production.ini')

from pyramid.paster import get_app
application = get_app(
  '/home/jiawzhang/VimProject/StudyPyramid/MyProject/production.ini', 'main')


