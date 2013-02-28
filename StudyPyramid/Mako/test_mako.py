from mako.template import Template
mytemplate = Template(filename='test/mytmpl.txt', module_directory='/home/jiawzhang/VimProject/StudyPyramid/Mako')
print mytemplate.render(name='jiawzhang for module_directory')

print '=========================================='

from mako.template import Template
from mako.lookup import TemplateLookup

mylookup = TemplateLookup(directories=['/home/jiawzhang/VimProject/StudyPyramid/Mako/test'])
mytemplate = Template("""<%include file="mytmpl.txt"/> hello world!""", lookup=mylookup)
print mytemplate.render(name='jiawzhang for TemplateLookup')

print '=========================================='

from mako.template import Template
from mako.lookup import TemplateLookup

mylookup = TemplateLookup(directories=['/home/jiawzhang/VimProject/StudyPyramid/Mako/test'], module_directory='/home/jiawzhang/VimProject/StudyPyramid/Mako/tmp')

mytemplate = mylookup.get_template('mytmpl.txt')
print mytemplate.render(name='jiawzhang for TemplateLookup.get_template()')

print '=========================================='
