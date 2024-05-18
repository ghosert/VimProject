# coding: utf-8

"""
# subscribe more interested routing_key by matching sign '#' and '*'

# '*' can substitute for exactly one word
# '#' can substitute for zero or more words
# "quick.orange.rabbit" matches both "*.*.rabbit" and "lazy.#", while "lazy.orange.male.rabbit" matches only "lazy.#"

# Topic exchange type:
# "#" actually will behave like 'fanout' exchange type
# if "#" and "*" aren't used, it will behave like 'direct' exchange type
python receive_logs_topic.py "#"

python 5_topics_receive_logs_topic.py "#"
python 5_topics_receive_logs_topic.py "kern.*"
python 5_topics_receive_logs_topic.py "*.critical"
python 5_topics_receive_logs_topic.py "kern.*" "*.critical"
python 5_topics_emit_log_topic.py "kern.critical" "A critical kernel error"
"""

import pika
import sys

connection = pika.BlockingConnection(pika.ConnectionParameters(
        host='localhost'))
channel = connection.channel()

channel.exchange_declare(exchange='topic_logs',
                         type='topic') # use exchange type 'topic' here

routing_key = sys.argv[1] if len(sys.argv) > 1 else 'anonymous.info'
message = ' '.join(sys.argv[2:]) or 'Hello World!'
channel.basic_publish(exchange='topic_logs',
                      routing_key=routing_key,
                      body=message)
print " [x] Sent %r:%r" % (routing_key, message)
connection.close()
