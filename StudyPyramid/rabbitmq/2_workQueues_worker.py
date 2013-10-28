# coding: utf-8

import pika
import time

connection = pika.BlockingConnection(pika.ConnectionParameters(
        host='localhost'))
channel = connection.channel()

channel.queue_declare(queue='task_queue', durable=True) # make queue persistent
print ' [*] Waiting for messages. To exit press CTRL+C'

def callback(ch, method, properties, body):
    print " [x] Received %r" % (body,)
    time.sleep( body.count('.') )
    print " [x] Done"
    ch.basic_ack(delivery_tag = method.delivery_tag) # send back the message acknowledgement to tell RabbitMQ so that the message is free to delete in RabbitMQ, DON'T miss this line if you turn on "no_ack=False" by default, otherwise this message will never be deleted. `sudo rabbitmqctl list_queues name messages_ready messages_unacknowledged` to list unacknowledged message.

channel.basic_qos(prefetch_count=1) # not to give more than one message to a worker at a time to make sure don't dispatch a new message to a worker until it has acknowledged the previous one.
channel.basic_consume(callback,
                      queue='task_queue') # remove no_ack=True here to use acknowledged message to make sure a message is never lost

channel.start_consuming()
