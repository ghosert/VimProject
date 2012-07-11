# This py will be invoked in uwsgi.xml

ini_file = '/home/jiawzhang/VimProject/StudyPyramid/MyProject/production.ini'

# [[[jiawzhang TODO: After reading logging chapter in narrative docs of Pyramid, make sure the comments below is still valid or not, since there is no log file location defined in proudction.ini anymore.
# And paste is not one of the pre-installed module for Pyramid anymore.]]]
# For uwsgi file logging from pyramid, the location of the log file is defined in production.ini
# Otherwise, starting application from uwsgi will not log anything to the log file which is defined in production.ini
# from paste.script.util.logging_config import fileConfig
# fileConfig(ini_file)

# Construct application here.
from pyramid.paster import get_app
application = get_app(ini_file, 'main')

