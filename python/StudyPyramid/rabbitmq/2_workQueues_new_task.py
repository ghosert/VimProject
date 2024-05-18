# coding: utf-8

"""
# start two workers
python 2_workQueues_worker.py
python 2_workQueues_workder.py
# start new task with 'dot' to indicate the heavy task, one dot means one second.
python 2_workQueues_new_task.py First message.
python 2_workQueues_new_task.py Second message..
python 2_workQueues_new_task.py Third message...
python 2_workQueues_new_task.py Fourth message....
python 2_workQueues_new_task.py Fifth message.....
"""

import pika
import sys

connection = pika.BlockingConnection(pika.ConnectionParameters(
        host='localhost'))
channel = connection.channel()

channel.queue_declare(queue='task_queue', durable=True) # make queue persistent

message = ' '.join(sys.argv[1:]) or "Hello World!"
channel.basic_publish(exchange='',
                      routing_key='task_queue',
                      body=message,
                      properties=pika.BasicProperties(
                         delivery_mode = 2, # make message persistent not strong guarantee, if you need strong guarantee to persist message, wrap publishing in a transaction.
                      ))
print " [x] Sent %r" % (message,)
connection.close()
