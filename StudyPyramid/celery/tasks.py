# coding: utf-8

"""
Running
$ celery -A tasks worker --loglevel=info
>>> from tasks import add
>>> add.delay(4, 4)
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

# Go with configuration
celery = Celery('tasks')
celery.config_from_object('celeryconfig') # load configuration from celeryconfig.py

@celery.task
def add(x, y):
    return x + y

