# coding: utf-8

from kombu import Queue

# configuration doc:
# http://docs.celeryproject.org/en/latest/configuration.html

BROKER_URL = 'amqp://'

# You don't have to specify result_backend if you don't expect to get the result
CELERY_RESULT_BACKEND = 'amqp://'

# This is necessary if you are going with daemon mode
CELERY_INCLUDE = 'tasks'

# For test_tasks.py to go with the queue: test_tasks_queue
CELERY_ROUTES = {
            'tasks.add': {'queue': 'test_tasks_queue'}
        }

# For tasks.py to listen the queue: test_tasks_queue
CELERY_QUEUES = (
            Queue('test_tasks_queue'),
        )

CELERY_TASK_SERIALIZER = 'json'
CELERY_RESULT_SERIALIZER = 'json'
CELERY_TIMEZONE = 'Asia/Shanghai'
CELERY_ENABLE_UTC = True
