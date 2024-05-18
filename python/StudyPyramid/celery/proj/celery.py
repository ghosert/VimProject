from __future__ import absolute_import

from celery import Celery

celery = Celery('proj.celery',
                broker='amqp://',
                # You don't have to set backend, if you don't care result
                backend='amqp://',
                # add tasks module here to make sure this worker is able to find them
                include=['proj.tasks'])

# Optional configuration, see the application user guide.
celery.conf.update(
    CELERY_TASK_RESULT_EXPIRES=3600,
)

if __name__ == '__main__':
    celery.start()

