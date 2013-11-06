# coding: utf-8

from kombu import Queue
from kombu.entity import Exchange

# configuration doc:
# http://docs.celeryproject.org/en/latest/configuration.html

BROKER_URL = 'amqp://'

# You don't have to specify result_backend if you don't expect to get the result
CELERY_RESULT_BACKEND = 'amqp://'

# This is necessary if you are going with daemon mode
CELERY_INCLUDE = 'tasks'

# tasks.add will go with the queue: tasks_queue for both producer and consumer
CELERY_ROUTES = {
            'tasks.add': {'queue': 'tasks_queue', 'routing_key': 'tasks_routing'},
            'tasks.mul': {'queue': 'tasks_queue', 'routing_key': 'tasks_routing'},
            #'tasks.add_1': {'queue': 'tasks_queue', 'routing_key': 'tasks_routing_1'},
            #'tasks.add_2': {'queue': 'tasks_queue_2', 'routing_key': 'tasks_routing_2'},
        }

# define exchanges explicitly, change type here requires reset queue/exchange: 'celery amqp queue.delete tasks_queue' and 'celery amqp exchange.delete tasks_exchange'
tasks_exchange = Exchange('tasks_exchange', type='direct') # fanout/direct/topic

# For tasks.py to listen the queue: tasks_queue
CELERY_QUEUES = (
            Queue('tasks_queue', tasks_exchange, routing_key='tasks_routing'),
            # Queue('tasks_queue', tasks_exchange, routing_key='tasks_routing_1'),
            # Queue('tasks_queue_2', tasks_exchange, routing_key='tasks_routing_2'),
            # routing_key could be 'tasks.#', '*.tasks.*' if exchange type is 'topic'
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

