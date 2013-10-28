# coding: utf-8

"""
# Both receiver below should receive the message.
python 3_publishSubscribe_receive_logs.py
python 3_publishSubscribe_receive_logs.py
python 3_publishSubscribe_emit_log.py
# should list two 'logs' exchanges, since we started two above
sudo rabbitmqctl list_bindings
"""

import pika
import sys

connection = pika.BlockingConnection(pika.ConnectionParameters(
        host='localhost'))
channel = connection.channel()

# define exchange type 'fanout': broadcasts all the messages it receives to all the queues it knows.
channel.exchange_declare(exchange='logs',
                         type='fanout')

message = ' '.join(sys.argv[1:]) or "info: Hello World!"
channel.basic_publish(exchange='logs', # publish to named exchange
                      routing_key='', # leave the routing_key empty here, 'fanout' type ignore this since there is no routing for this type. Message will be discarded if no consumer is listening, sometimes acceptable.
                      body=message)
print " [x] Sent %r" % (message,)
connection.close()
