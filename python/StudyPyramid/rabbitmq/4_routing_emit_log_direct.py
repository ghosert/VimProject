# coding: utf-8

"""
# subscribe only to a subset of the messages with the routing_key
# worker1 below will pick up warning/error while worker2 will pick up info/warning/error
python 4_routing_receive_logs_direct.py warning error
python 4_routing_receive_logs_direct.py info warning error
# emit error message and observe the worker1/worker2
python 4_routing_emit_log_direct.py error "Run. Run. Or it will explode."
"""

import pika
import sys

connection = pika.BlockingConnection(pika.ConnectionParameters(
        host='localhost'))
channel = connection.channel()

channel.exchange_declare(exchange='direct_logs',
                         type='direct') # use exchange type 'direct'

severity = sys.argv[1] if len(sys.argv) > 1 else 'info'
message = ' '.join(sys.argv[2:]) or 'Hello World!'
channel.basic_publish(exchange='direct_logs',
                      routing_key=severity, # send message with routing_key
                      body=message)
print " [x] Sent %r:%r" % (severity, message)
connection.close()
