## 1. Prerequisite:

```sh
`sudo vi /etc/apt/sources.list` to add the line below:
deb http://www.rabbitmq.com/debian/ testing main
wget http://www.rabbitmq.com/rabbitmq-signing-key-public.asc
sudo apt-key add rabbitmq-signing-key-public.asc
sudo apt-get update
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
# list exchanges
sudo rabbitmqctl list_exchanges
# list bindings
sudo rabbitmqctl list_bindings
```

## 3. Configuration

http://www.rabbitmq.com/configure.html#config-items
