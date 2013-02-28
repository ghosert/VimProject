## Usage

**If you are using web framework like Pyramid, you can skip this 'Usage' section, otherwise, you'd like to generate you own html by Mako, check this section. See samples in ./test_mako.py**

### Basic Usage

**1. template.render()**

```
from mako.template import Template

mytemplate = Template('hello world!')
print mytemplate.render()
```

**2. template.render(key='value')**

```
from mako.template import Template

mytemplate = Template('hello ${name}!')
print mytemplate.render(name='jack')
```

**3. Call render with your own context**

```
from mako.template import Template
from mako.runtime import Context
from StringIO import StringIO

mytemplate = Template('hello ${name}!')
buf = StringIO()
ctx = Context(buf, name='jack')
mytemplate.render_context(ctx)
print buf.getvalue()
```

### Using File-Based Templates

```
from mako.template import Template

mytemplate = Template(filename='/docs/mytmpl.txt')
print mytemplate.render()
```

For improved performance, a Template which is loaded from a file can also cache the source code to its generated module on the filesystem as a regular Python module file (i.e. a .py file). 

```
from mako.template import Template

mytemplate = Template(filename='docs/mytmpl.txt', module_directory='/tmp/mako_modules')
print mytemplate.render()
```
The value of 'module_directory' above should be a path in 'sys.path' for example, you are running python file above in the '/tmp/mako_modules', this path will be in 'sys.path', thus '/tmp/mako_modules' is a qualified 'module_direcotry'.

When the above code is rendered, a file /tmp/mako_modules/docs/mytmpl.txt.py is created containing the source code for the module. The next time a Template with the same arguments is created, this module file will be automatically re-used.

### Using ``TemplateLookup``

**1. TemplateLookup**

```
from mako.template import Template
from mako.lookup import TemplateLookup

mylookup = TemplateLookup(directories=['/docs'])
mytemplate = Template("""<%include file="header.txt"/> hello world!""", lookup=mylookup)
print mytemplate.render()
```
Above, we created a textual template which includes the file "header.txt". In this way, it will search in the directory "/docs" for the file "header.txt".

**2. TemplateLookup.get_template(templatename)**

```
from mako.template import Template
from mako.lookup import TemplateLookup

mylookup = TemplateLookup(directories=['/docs'], module_directory='/tmp/mako_modules')

def serve_template(templatename, **kwargs):
    mytemplate = mylookup.get_template(templatename)
    print mytemplate.render(**kwargs)
```

TemplateLookup will look for templates in the `/docs` directory, and will store generated module files in the /tmp/mako_modules directory. If given '/etc/beans/info.txt' as templatename, it would search for the file '/docs/etc/beans/info.txt' and create a module file '/tmp/mako_modules/etc/beans/info.txt.py'

**3. Setting the Collection Size**

mylookup = TemplateLookup(directories=['/docs'],
                module_directory='/tmp/mako_modules', collection_size=500)

The above lookup will continue to load templates into memory until it reaches a count of around 500. At that point, it will clean out a certain percentage of templates using a least recently used scheme.

Skip the rest of the part, if you do need to refer to it, check out:
http://docs.makotemplates.org/en/latest/usage.html#basic-usage

