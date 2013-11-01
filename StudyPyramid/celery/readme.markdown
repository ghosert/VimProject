# Celery --- Distributed Task Queue

------

## Officiel Docs

http://docs.celeryproject.org/en/latest/

## Install Prerequisite

```sh
$ sudo apt-get install rabbitmq-server
$ source ~/devenv/bin/activate # go to virtualenv
$ pip install librabbitmq # for performance
$ easy_install pytz
$ easy_install Celery
```

## Sample Running

```sh
$ celery -A tasks worker -l info
>>> from tasks import add
>>> add.delay(4, 4)
```

## Run in Daemon Mode

0. sudo adduser celery && sudo adduser celery sudo
1. mkdir /var/log/pyramid/celery && chown celery:celery /var/log/pyarmid/celery && chmod 775 /var/log/pyramid/celery
2. sudo cp ./env/celerybeat /etc/init.d/
3. sudo cp ./env/celeryd /etc/init.d/
4. sudo cp ./env/conf/celeryd /etc/default/celeryd # change this **celeryd** if needed.
5. sudo service celeryd start/stop/restart/status
6. sudo service celerybeat start/stop/restart
7. sudo service celeryd status && tail -f /var/log/pyramid/celery/w1.log to check if everything is ok.

### Notes:

1. if you have any py changes, make sure to sudo service celeryd restart
2. See more sample configurations for daemon [here][1]
3. ./env/* are coming from **extra/generic-init.d/** from [here][1]

### Operating:

### Delete message/queue
```sh
celery amqp queue.purge queue_name
celery amqp queue.delete queue_name
```

## Help

```
$ celery worker --help # look up command help, e.g. 'worker' here
$ celery --help # list other commands
```

[1]: http://docs.celeryproject.org/en/latest/tutorials/daemonizing.html#daemonizing
