## 1. Prerequisite:

```sh
sudo apt-get install rabbitmq-server
sudo easy_install pika==0.9.8
# make sure it's running now
sudo service rabbitmq-server status
```

## 2. Look up rabbitmq

```sh
# list queues
sudo rabbitmqctl list_queues
# list queues, unacknowledged messages
sudo rabbitmqctl list_queues name messages_ready messages_unacknowledged
```

## 3. Configuration

http://www.rabbitmq.com/configure.html#config-items
