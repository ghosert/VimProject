# coding: utf-8

# configuration doc:
# http://docs.celeryproject.org/en/latest/configuration.html#std:setting-CELERY_ANNOTATIONS

BROKER_URL = 'amqp://'
CELERY_RESULT_BACKEND = 'amqp://'

CELERY_TASK_SERIALIZER = 'json'
CELERY_RESULT_SERIALIZER = 'json'
CELERY_TIMEZONE = 'Asia/Shanghai'
CELERY_ENABLE_UTC = True
