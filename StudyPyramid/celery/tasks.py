# coding: utf-8

"""
Running
$ celery -A tasks worker --loglevel=info
>>> from tasks import add
>>> add.delay(4, 4)

Running in daemon
$ sudo service celeryd/celerybeat start
>>> from tasks import add
>>> add.delay(4, 4)
Notes 1:
    Make sure you have CELERY_INCLUDE defined in celeryconfig.py otherwise daemon not working
Notes 2:
    celery configuration goes to celeryconfig.py
    celeryd/celerybeat configuration goes to /etc/default/celeryd
Notes 3:
    if you change .py like tasks.py or celeryconfig.py make sure you restart like below:
    $ sudo service celeryd restart
    Otherwise the change will not be applied.
"""

from celery import Celery

# Set no backend
#
# celery = Celery('tasks', broker='amqp://guest@localhost//')
#
# You can't play with result, if there is no backend set like below

# Set backend if you want to store or send the states somewhere
#
# celery = Celery('tasks', backend='amqp', broker='amqp://guest@localhost//')
#
# Thus, you could do this:
# >>> result = add.deplay(4, 4)
# >>> result.ready()
# >>> result.get(timeout=1)
# >>> result.get(propagate=True)
# >>> result.traceback

import logging

# Go with configuration
celery = Celery('tasks')
celery.config_from_object('celeryconfig') # load configuration from celeryconfig.py

@celery.task
def add(x, y):
    logging.info('Caculate adding expression x={0}, y={1}'.format(x, y))
    return x + y

