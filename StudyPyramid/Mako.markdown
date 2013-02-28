## Usage

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

mytemplate = Template('hello ${name}!')
buf = StringIO()
ctx = Context(buf, name='jack')
mytemplate.render_context(ctx)
print buf.getvalue()
```



