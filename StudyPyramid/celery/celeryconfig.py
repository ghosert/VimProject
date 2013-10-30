# coding: utf-8

# configuration doc:
# http://docs.celeryproject.org/en/latest/configuration.html

BROKER_URL = 'amqp://'
# You don't have to specify result_backend if you don't expect to get the result
CELERY_RESULT_BACKEND = 'amqp://'

CELERY_TASK_SERIALIZER = 'json'
CELERY_RESULT_SERIALIZER = 'json'
CELERY_TIMEZONE = 'Asia/Shanghai'
CELERY_ENABLE_UTC = True
