# coding: utf-8

from kombu import Queue

# configuration doc:
# http://docs.celeryproject.org/en/latest/configuration.html

BROKER_URL = 'amqp://'

# You don't have to specify result_backend if you don't expect to get the result
CELERY_RESULT_BACKEND = 'amqp://'

# This is necessary if you are going with daemon mode
CELERY_INCLUDE = 'tasks'

# For tasks.add to go with the queue: test_tasks_queue
CELERY_ROUTES = {
            'tasks.add': {'queue': 'test_tasks_queue'}
        }

# For tasks.py to listen the queue: test_tasks_queue
CELERY_QUEUES = (
            Queue('test_tasks_queue'),
        )

# acknowledged after the task has been executed, False by default
CELERY_ACKS_LATE = True

# 'pickle', 'json', 'yaml', 'msgpack' are supported for serialization/unserialization
CELERY_TASK_SERIALIZER = 'pickle'
CELERY_RESULT_SERIALIZER = 'pickle'

CELERY_TIMEZONE = 'Asia/Shanghai'
CELERY_ENABLE_UTC = True


# celerybeat
from datetime import timedelta
from celery.schedules import crontab

CELERYBEAT_SCHEDULE = {
    'add-every-30-seconds': {
        'task': 'tasks.add',
        'schedule': timedelta(seconds=30),
        # 'schedule': crontab(hour=7, minute=30, day_of_week=1),
        'args': (16, 16)
    },
}
# All the crontab definition goes here:
# http://docs.celeryproject.org/en/latest/userguide/periodic-tasks.html
# Ensure a task is only executed one at a time
# http://docs.celeryproject.org/en/latest/tutorials/task-cookbook.html#cookbook-task-serial

