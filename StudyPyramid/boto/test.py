# coding: utf-8

# The sample is coming from here
# http://docs.pythonboto.org/en/latest/ses_tut.html
# Amazon SES reference
# http://docs.pythonboto.org/en/latest/ref/ses.html

import boto.ses
from email.header import Header
# open debug mode
# boto.set_stream_logger('boto')

conn = boto.ses.connect_to_region('us-east-1')

recipient_address = 'support@zybuluo.com'
# recipient_address = 'ghosert@gmail.com'
# recipient_address = 'success@simulator.amazonses.com'
print conn.send_email('{0} <support@zybuluo.com>'.format(Header('作业部落', 'utf-8')), u'测试标题', '<h1>测试文本内容</h1>', [recipient_address], format='text', reply_addresses=recipient_address, return_path=recipient_address, html_body='<h1>测试 html 内容</h1>')

# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['success@simulator.amazonses.com'])
# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['bounce@simulator.amazonses.com'])
# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['ooto@simulator.amazonses.com'])
# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['complaint@simulator.amazonses.com'])
# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['suppressionlist@simulator.amazonses.com'])

