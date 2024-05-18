# coding: utf-8

"""
python 1_hello_send.py
# see the queue 'hello': should be 1 message
sudo rabbitmqctl list_queues
python 1_hello_receive.py
# see the queue 'hello': should be 0 message
sudo rabbitmqctl list_queues
"""

import pika

connection = pika.BlockingConnection(pika.ConnectionParameters(
               'localhost'))
channel = connection.channel()

channel.queue_declare(queue='hello')

channel.basic_publish(exchange='', # empty string means: messages are routed to the queue with the name specified by routing_key, here 'hello', otherwise it's used for routing.
                      routing_key='hello',
                      body='Hello World!')
print " [x] Sent 'Hello World!'"

connection.close()
