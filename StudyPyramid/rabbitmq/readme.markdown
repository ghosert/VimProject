## 1. Prerequisite:

Followed by ~/Dropbox/ubuntu_install_guide.sh to install rabbitmq

## 2. Look up rabbitmq

```sh
# list queues
sudo rabbitmqctl list_queues
# list queues, unacknowledged messages
sudo rabbitmqctl list_queues name messages_ready messages_unacknowledged
# list exchanges
sudo rabbitmqctl list_exchanges
# list bindings
sudo rabbitmqctl list_bindings
```

## 3. Configuration

http://www.rabbitmq.com/configure.html#config-items
