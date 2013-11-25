# coding: utf-8

"""
Running
$ celery -A tasks worker --loglevel=info
>>> from tasks import add
>>> add.delay(4, 4)
$ celery beat
Start beat as the producer and make sure celeryd is working as the consumer.

Running in daemon
    $ sudo service celeryd/celerybeat start
    >>> from tasks import add
    >>> add.delay(4, 4)
Notes 1:
    Make sure you have CELERY_INCLUDE defined in celeryconfig.py otherwise daemon not working
Notes 2:
    celery/celerybeat configuration goes to celeryconfig.py
    celeryd/celerybeat configuration goes to /etc/default/celeryd
Notes 3:
    if you change .py like tasks.py or celeryconfig.py make sure you restart like below:
    $ sudo service celeryd/celerybeat restart
    Otherwise the change will not be applied.

Remote Control
celery -A tasks inspect active
celery -A tasks status

# lookup current events
celery -A tasks control enable_events
celery -A tasks events / celery -A tasks events --dump
celery -A tasks control disable_events
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
# >>> result.get(propagate=True) # default is True equals to result.get()
# >>> result.traceback

# Go with configuration
celery = Celery('tasks') # specify the module name 'tasks' which is same to current module name
# load config from celeryconfig.py, if there is package with module, check https://groups.google.com/forum/#!topic/celery-users/D-5PtgAqdLI otherwise error happens with celerybeat
celery.config_from_object('celeryconfig')

# Set some configurations as list here:
# celery.conf.update(
    # CELERYD_PREFETCH_MULTIPLIER = 1,
# )


from celery.utils.log import get_task_logger
from celery import current_task

logger = get_task_logger(__name__)

# Put task decorator the first one if there are multiple decorators, which means @celery.task will be excuted last.
@celery.task(max_retries=3) # default max_retries=3, default_retry_delay=180, rate_limite='1/s' '1/m' '1/h', second, minute, hour
def add(x, y):
    try:
        request = current_task.request
        logger.info('==================add retries: {0}======================'.format(request.retries))
        logger.info('add.name={0}'.format(add.name))
        logger.info('Caculate adding expression x={0}, y={1}'.format(x, y)) 
        logger.info('request.delivery_info={0} request.retries={1} request.hostname={2}'.format(request.delivery_info, request.retries, request.hostname))
        # fail deliberately until retry = 3
        if request.retries < 3:
            raise Exception('ERROR HAPPENS HERE.')
        return x + y
    except Exception as exc:
        raise add.retry(exc=exc, countdown=5)
        # 1. overwrite 180s above to 5s to retry, 'Retry' will be logged into logs
        # 2. worker will notify the queue to resend message for retry
        # 3. if it excceds max_retries, the detail exception(exc here) will be thrown and logged.

@celery.task
def mul(x, y):
    logger.info('========================mul=================================')
    logger.info('mul.name={0}'.format(mul.name))
    logger.info('Caculate mul expression x={0}, y={1}'.format(x, y)) 
    return x * y



# There is a race condition if the task starts executing before the transaction has been committed; The database object does not exist yet!

# The solution is to always commit transactions before sending tasks depending on state from the current transaction:

# Django sample
# @transaction.commit_manually
# def create_article(request):
    # try:
        # article = Article.objects.create(...)
    # except:
        # transaction.rollback()
        # raise
    # else:
        # transaction.commit()
        # expand_abbreviations.delay(article.pk)

