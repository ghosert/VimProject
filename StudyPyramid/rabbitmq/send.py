# coding: utf-8

# prerequisite:
# sudo apt-get install rabbitmq-server
# sudo easy_install pika==0.9.8
# sudo rabbitmqctl list_queues

# Run
# make sure `sudo service rabbitmq-server status` is running
# `python send.py`
# sudo rabbitmqctl list_queues to see the queue 'hello': should be 1 message
# `python receive.py`
# sudo rabbitmqctl list_queues to see the queue 'hello': should be 0 message

# Configuration
# http://www.rabbitmq.com/configure.html#config-items

import pika

connection = pika.BlockingConnection(pika.ConnectionParameters(
               'localhost'))
channel = connection.channel()

channel.queue_declare(queue='hello')

channel.basic_publish(exchange='',
                      routing_key='hello',
                      body='Hello World!')
print " [x] Sent 'Hello World!'"

connection.close()
