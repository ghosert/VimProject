# coding: utf-8

import pika

connection = pika.BlockingConnection(pika.ConnectionParameters(
        host='localhost'))
channel = connection.channel()

channel.exchange_declare(exchange='logs',
                         type='fanout')

# 1. generate a random queue name
# 2. 'exclusive=True' means delete the queue once the consumer is disconnected.
result = channel.queue_declare(exclusive=True)
queue_name = result.method.queue

# binding exchange and queue
channel.queue_bind(exchange='logs',
                   queue=queue_name)

print ' [*] Waiting for logs. To exit press CTRL+C'

def callback(ch, method, properties, body):
    print " [x] %r" % (body,)

channel.basic_consume(callback,
                      queue=queue_name, # set queue name here
                      no_ack=True) # no ackowlegment is required

channel.start_consuming()
