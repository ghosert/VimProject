# Celery --- Distributed Task Queue

------

## Officiel Docs

http://docs.celeryproject.org/en/latest/

## Install Prerequisite

Followed by ~/Dropbox/ubuntu_install_guide.sh to install celery

## Sample Running

```sh
$ celery -A tasks worker -l info
>>> from tasks import add
>>> add.delay(4, 4)
$ celery beat
```

## Run in Daemon Mode

Followed by ~/productproject/SETUP.markdown
Except: 
sudo cp ./env-conf/celeryd /etc/default/celeryd # change this **celeryd** if needed.

See more sample configurations for daemon [here][1]

## Help

```
$ celery worker --help # look up command help, e.g. 'worker' here
$ celery --help # list other commands
$ celeryd --help
```

[1]: http://docs.celeryproject.org/en/latest/tutorials/daemonizing.html#daemonizing
