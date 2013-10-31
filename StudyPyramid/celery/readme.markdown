# Celery --- Distributed Task Queue

------

## Officiel Docs

http://docs.celeryproject.org/en/latest/

## Install Prerequisite

```sh
$ sudo apt-get install rabbitmq-server
$ easy_install Celery
$ easy_install pytz
```

## Sample Running

```sh
$ celery -A tasks worker -l info
>>> from tasks import add
>>> add.delay(4, 4)
```

## Run in Daemon Mode

1. See details and download **extra/generic-init.d/** from [here][1] to ./env
2. sudo cp ./env/celerybeat /etc/init.d/
3. sudo cp ./env/celeryd /etc/init.d/
4. sudo cp ./env/conf/celeryd /etc/default/celeryd # check this config file if needed.
5. mkdir /var/log/pyramid/celery/
6. sudo service celeryd start/stop/restart/status
7. sudo service celerybeat start/stop/restart
8. sudo service celeryd status && tail -f /var/log/pyramid/celery/w1.log to check if everything is ok.
9. if you have any py changes, make sure to sudo service celeryd restart
10. See more sample configurations [here][1]


## Help

```
$ celery worker --help # look up command help, e.g. 'worker' here
$ celery --help # list other commands
```

[1]: http://docs.celeryproject.org/en/latest/tutorials/daemonizing.html#daemonizing
