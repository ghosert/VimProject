# coding: utf-8

import traceback
import thread_util
import feedparser

import re
import time
import calendar

def utcToLocalString(format, st):
    seconds = calendar.timegm(st)
    st = time.localtime(seconds)
    return time.strftime(format, st)

def convertGmailPublished(published):
    """Convert hours field from 24 to 00, otherwise, it may cause problem when comparing time.
    And return time.struct_time
    """
    published = re.sub("T24:", "T00:", published)
    
    # convert 2009-10-31T06:08:22Z to time.struct_time
    return time.strptime(published, "%Y-%m-%dT%H:%M:%SZ")

class FeedProducer(thread_util.Producer):
    def __init__(self, timerSeconds, currentEntry = None):
        "startCount should be a number string or None."
        thread_util.Producer.__init__(self, timerSeconds)
        self.__currentEntry = currentEntry
        
    def process(self, channel):
        try:
            atom = feedparser.parse('https://{0}:{1}@mail.google.com/mail/feed/atom/{2}'.format(account, password, tag_name))
            
            # Return if nothing in feed.
            if atom == None or len(atom.entries) == 0:
                return
            
            if self.__currentEntry == None:
                self.__currentEntry = atom.entries[0]
            else:
                # Find the new entries from feed and store them to newlist.
                newlist = []
                for newEntry in atom.entries:
                    if convertGmailPublished(newEntry.published) > convertGmailPublished(self.__currentEntry.published):
                        newlist.append(newEntry)
                    elif convertGmailPublished(newEntry.published) == convertGmailPublished(self.__currentEntry.published) and newEntry != self.__currentEntry:
                        newlist.append(newEntry)
                    else:
                        break
                    
                if len(newlist) <= 0: return # Quit if no new entry.
                
                # Set the newest mail entry to current entry
                self.__currentEntry = newlist[0]
                
                # Report the number of new entries.
                if len(newlist) == len(atom.entries):
                    print 'You have at least {0} new entries'.format(len(newlist))
                else:
                    print 'You have {0} new entry(ies)'.format(len(newlist))
                        
                # Print new entries.
                for newEntry in newlist:
                    print newEntry.title
                    print utcToLocalString("%Y %m %d %H:%M:%S", convertGmailPublished(newEntry.published))
                    print newEntry.summary
                    print newEntry.link
                    print newEntry.author_detail.name
                    print newEntry.author_detail.email
                    print
                    
        except Exception:
            print "I'm handling the error."
            print traceback.format_exc()
        
account = raw_input("Your google account:")
password = raw_input("Your google password:")
tag_name = raw_input("Your google tag_name(ignore for inbox):")

channel = thread_util.Channel()
channel.startProducer([FeedProducer(10, None)])

print 'checking your gmail every 10 seconds...\n'

#Gmail - Inbox for ghosert@gmail.com
#New messages in your Gmail Inbox
#1667
#200
#https://mail.google.com/mail/feed/atom
#20
#恭喜您获得卡巴斯基国庆大礼！
#2009-10-31T06:08:22Z
#怎么使用优惠劵） 优惠券可以多次使用，欢迎推荐给朋友 一年一度的 ...
#http://mail.google.com/mail?account_id=ghosert%40gmail.com&message_id=124a93572b318400&view=conv&extsrc=atom
#卡巴斯基
#noreply3@kaba365.com
#This Week @ your library

#for key, value in d.iteritems():
#    print key, value
#
#print '====================================================================='
#
#print d.feed.title
#print d.feed.subtitle
#print d.feed.fullcount
#print d.status
#print d.href
#print len(d.entries)
#for entrie in d.entries:
#    print entrie.title
#    print entrie.published
#    print entrie.summary
#    print entrie.link
#    print entrie.author_detail.name
#    print entrie.author_detail.email
