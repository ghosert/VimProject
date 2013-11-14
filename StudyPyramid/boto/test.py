# coding: utf-8

# The sample is coming from here
# http://docs.pythonboto.org/en/latest/ses_tut.html
# Amazon SES reference
# http://docs.pythonboto.org/en/latest/ref/ses.html

# jiawzhang NOTICE: one time one recipient
# jiawzhang NOTICE: do not send emails to hard bounced or complained recipient, you will know this by email sent to return_path above from ISP, check them and remove these recipient from sending list.
# jiawzhang NOTCIE: attachment is 10MB for amazon SES
# jiawzhang NOTICE: check https://console.aws.amazon.com/ses/home?region=us-west-1# to see 'Sending Quota' and 'Max Send Rate'


import boto.ses
from email.header import Header
# open debug mode
# boto.set_stream_logger('boto')

conn = boto.ses.connect_to_region('us-east-1')

recipient_address = 'support@zybuluo.com'
# recipient_address = 'ghosert@gmail.com'
# recipient_address = 'success@simulator.amazonses.com'

# Text body only
# print conn.send_email('{0} <support@zybuluo.com>'.format(Header('作业部落', 'utf-8')), u'测试标题', '<h1>测试文本内容</h1>', [recipient_address], format='text', reply_addresses=recipient_address, return_path=recipient_address)

# Html body only
# print conn.send_email('{0} <support@zybuluo.com>'.format(Header('作业部落', 'utf-8')), u'测试标题', '<h1>测试文本内容</h1>', [recipient_address], format='html', reply_addresses=recipient_address, return_path=recipient_address)

# Both Text/Html body
print conn.send_email('{0} <support@zybuluo.com>'.format(Header('作业部落', 'utf-8')), u'测试标题', '<h1>测试文本内容</h1>', [recipient_address], format='text', reply_addresses=recipient_address, return_path=recipient_address, html_body='<h1>测试 html 内容</h1>')

# Successful response:
# {u'SendEmailResponse': {u'ResponseMetadata': {u'RequestId': u'e9baa432-4d1d-11e3-87ec-e54f48b5bac8'}, u'SendEmailResult': {u'MessageId': u'000001425651ab7e-a5232174-7d40-44c9-ae6d-903097f3989c-000000'}}}

# Error response:
# {u'ErrorResponse': {u'Error': {u'Type': u'Sender', u'Code': u'ValidationError', u'Message': u"Value null at 'message.subject' failed to satisfy constraint: Member must not be null"}, u'RequestId': '42d59b56-7407-4c4a-be0f-4c88daeea257'}}

# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['success@simulator.amazonses.com'])
# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['bounce@simulator.amazonses.com'])
# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['ooto@simulator.amazonses.com'])
# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['complaint@simulator.amazonses.com'])
# print conn.send_email('support@zybuluo.com', 'Test title', 'Test body', ['suppressionlist@simulator.amazonses.com'])


