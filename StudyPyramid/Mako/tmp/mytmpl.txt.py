# -*- encoding:ascii -*-
from mako import runtime, filters, cache
UNDEFINED = runtime.UNDEFINED
__M_dict_builtin = dict
__M_locals_builtin = locals
_magic_number = 6
_modified_time = 1362064564.025671
_template_filename='/home/jiawzhang/VimProject/StudyPyramid/Mako/test/mytmpl.txt'
_template_uri='mytmpl.txt'
_template_cache=cache.Cache(__name__, _modified_time)
_source_encoding='ascii'
_exports = []


def render_body(context,**pageargs):
    context.caller_stack._push_frame()
    try:
        __M_locals = __M_dict_builtin(pageargs=pageargs)
        name = context.get('name', UNDEFINED)
        __M_writer = context.writer()
        # SOURCE LINE 1
        __M_writer(unicode(name))
        __M_writer(u' hello world!\n')
        return ''
    finally:
        context.caller_stack._pop_frame()


