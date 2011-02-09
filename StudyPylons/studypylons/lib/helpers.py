"""Helper functions

Consists of functions to typically be used within templates, but also
available to Controllers. This module is available to templates as 'h'.
"""
# Import helpers as desired, or define your own, ie:
#from webhelpers.html.tags import checkbox, password

# jiawzhang: after importing these, you can then use 'h' in templates like 'h.form', 'h.url'
from webhelpers.html import escape, HTML, literal, url_escape
from webhelpers.html.tags import *
from pylons import url
