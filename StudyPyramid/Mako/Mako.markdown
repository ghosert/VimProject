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
The value of ``module_directory`` above should be a path in `sys.path` for example, you are running python file above in the ``/tmp/mako_modules``, this path will be in `sys.path`, thus ``/tmp/mako_modules`` is a qualified ``module_direcotry``.

When the above code is rendered, a file ``/tmp/mako_modules/docs/mytmpl.txt.py`` is created containing the source code for the module. The next time a Template with the same arguments is created, this module file will be automatically re-used.

### Using `TemplateLookup`

**1. TemplateLookup**

```
from mako.template import Template
from mako.lookup import TemplateLookup

mylookup = TemplateLookup(directories=['/docs'])
mytemplate = Template("""<%include file="header.txt"/> hello world!""", lookup=mylookup)
print mytemplate.render()
```
Above, we created a textual template which includes the file `header.txt`. In this way, it will search in the directory `/docs` for the file `header.txt`.

**2. TemplateLookup.get_template(templatename)**

```
from mako.template import Template
from mako.lookup import TemplateLookup

mylookup = TemplateLookup(directories=['/docs'], module_directory='/tmp/mako_modules')

def serve_template(templatename, **kwargs):
    mytemplate = mylookup.get_template(templatename)
    print mytemplate.render(**kwargs)
```

TemplateLookup will look for templates in the `/docs` directory, and will store generated module files in the ``/tmp/mako_modules`` directory. If given `/etc/beans/info.txt` as templatename, it would search for the file `/docs/etc/beans/info.txt` and create a module file ``/tmp/mako_modules/etc/beans/info.txt.py``

**3. Setting the Collection Size**

mylookup = TemplateLookup(directories=['/docs'],
                module_directory='/tmp/mako_modules', collection_size=500)

The above lookup will continue to load templates into memory until it reaches a count of around 500. At that point, it will clean out a certain percentage of templates using a least recently used scheme.

Skip the rest of the part, if you do need to refer to it, check out: http://docs.makotemplates.org/en/latest/usage.html#basic-usage


## Syntax

### Expression Substitution

```
this  is x: ${x}
```
If x is not supplied, it evaluates to a special value `UNDEFINED`.

The contents within the ${} tag are evaluated by Python directly, so full expressions are OK:

```
pythagorean theorem:  ${pow(x,2) + pow(y,2)}
```

### Expression Escaping

${"this is some text" | u}

The above expression applies URL escaping to the expression, and produces this+is+some+text. The u name indicates URL escaping, whereas h represents HTML escaping, x represents XML escaping, and trim applies a trim function.

### Control Structures

conditionals(`if/else`), loops(`while/for`) as well as things like `try/except`

```
% if x==5:
    this is some output
% endif
```
the % can appear anywhere on the line as long as no text precedes it; indentation is not significant. The full range of Python "colon" expressions are allowed here, including if/elif/else, while, for, and even def, although Mako has a built-in tag for defs which is more full-featured.

```
% for a in ['one', 'two', 'three', 'four', 'five']:
    % if a[0] == 't':
    its two or three
    % elif a[0] == 'f':
    four/five
    % else:
    one
    % endif
% endfor
```

The % sign can also be "escaped", if you actually want to emit a percent sign as the first non whitespace character on a line, by escaping it as in %%:

```
%% some text

    %% some more text
```

### The Loop Context

```
<ul>
% for a in ("one", "two", "three"):
    <li>Item ${loop.index}: ${a}</li>
% endfor
</ul>
```

### Comments

Comments come in two varieties. The single line comment uses ## as the first non-space characters on a line:

```
## this is a comment.
...text ...
```

A multiline version exists using <%doc> ...text... </%doc>:

```
<%doc>
    these are comments
    more comments
</%doc>
```

### Newline Filters

```
here is a line that goes onto \
another line.
```

The above text evaluates to:

```
here is a line that goes onto another line.
```

### Python Blocks

Any arbitrary block of python can be dropped in using the `<% %>` tags:

```
this is a template
<%
    x = db.get_resource('foo')
    y = [z.element for z in x if x.frobnizzle==5]
%>
% for elem in y:
    element: ${elem}
% endfor
```

### Module-level Blocks

A variant on <% %> is the module-level code block, denoted by <%! %>.

```
<%!
	import mylib
    import re

    def filter(text):
        return re.sub(r'^@', '', text)
%>
```

The code doesn't have access to the template's context and is only executed when the template is loaded into memory (which can be only once per application, or more, depending on the runtime environment). Use the <%! %> tags to declare your template's imports, as well as any pure-Python functions you might want to declare:

### Tags

The tag is closed either by a contained slash character, or an explicit closing tag:

```
<%include file="foo.txt"/>

<%def name="foo" buffered="True">
    this is a def
</%def>
```

All tags have a set of attributes which are defined for each tag. Some of these attributes are required. Also, many attributes support evaluation, meaning you can embed an expression (using ${}) inside the attribute text:

```
<%include file="/foo/bar/${myfile}.txt"/>
```

#### `<%page>`

This tag defines general characteristics of the template, including caching arguments, and optional lists of arguments which the template expects when invoked.

```
<%page args="x, y, z='default'"/>
```

Or a page tag that defines caching characteristics:

```
<%page cached="True" cache_type="memory"/>
```

Currently, 0.7.4 only one `<%page>` tag gets used per template, the rest get ignored. See more below.

#### `<%include>`

```
<%include file="header.html"/>

    hello world

<%include file="footer.html"/>
```

Include also accepts arguments which are available as <%page> arguments in the receiving template:

```
<%include file="toolbar.html" args="current_section='members', username='ed'"/>
```

#### `<%def>`

The %def tag defines a Python function which contains a set of content, that can be called at some other point in the template. The basic idea is simple:

```
<%def name="myfunc(x)">
    this is myfunc, x is ${x}
</%def>

${myfunc(7)}
```

#### `<%block>`

`%block` is a tag that is close to a %def, except executes itself immediately in its base-most scope, and can also be anonymous (i.e. with no name):

```
<%block filter="h">
    some <html> stuff.
</%block>
```

jiawzhang: Not sure what is the `<%block>` for so far, see below for details.

#### `<%namespace>`

`%namespace` is Mako's equivalent of Python's import statement. It allows access to all the rendering functions and metadata of other template files, plain Python modules, as well as locally defined "packages" of functions.

```
<%namespace file="functions.html" import="*"/>
```

See below for details.

#### `<%inherit>`

Inherit allows templates to arrange themselves in inheritance chains.

```
<%inherit file="base.html"/>
```

#### `<%nsname:defname>`

user-defined "tag"

```
<%mynamespace:somedef param="some value">
    this is the body
</%mynamespace:somedef>
```

#### `<%call>`

user-defined "tag" and is roughly equivalent to the `<%nsname:defname>` syntax described above.

#### `<%doc>`

For multiline comments, see above.

#### `<%text>`

This tag suspends the Mako lexer's normal parsing of Mako template directives, and returns its entire body contents as plain text. It is used pretty much to write documentation about Mako:

```
<%text filter="h">
    heres some fake mako ${syntax}
    <%def name="x()">${x}</%def>
</%text>
```

#### Returning Early from a Template

Sometimes you want to stop processing a template or <%def> method in the middle and just use the text you've accumulated so far. You can use a return statement inside a Python block to do that.

```
% if not len(records):
    No records found.
    <% return %>
% endif
```

Or perhaps:

```
<%
    if not len(records):
        return
%>
```

## Defs and Blocks

Whereas <%def> provides a construct that is very much like a named Python def, the <%block> is more layout oriented.

### Using Defs

```
<%def name="hello()">
    hello world
</%def>
```
To invoke the `<%def>`

```
the def: ${hello()}
```

**top level def**(the `<%def>` is not nested inside of another `<%def>`) can be accessed anywhere in the template, including above where it was defined.

All defs have access to the current contextual namespace in exactly the same way their containing template does.

```
Hello there ${username}, how are ya.  Lets see what your account says:

${account()}

<%def name="account()">
    Account for ${username}:<br/>

    % for row in accountdata:
        Value: ${row}<br/>
    % endfor
</%def>
```

The `username` and `accountdata` variables are present within the main template body as well as the body of the account() def.

Since defs are just Python functions, you can define and pass arguments to them as well:

${account(accountname='john')}

<%def name="account(accountname, type='regular')">
    account name: ${accountname}, type: ${type}
</%def>

#### Calling Defs from Other Files

Calling a <%def> from another template is something like using an <%include> – except you are calling a specific function within the template, not the whole template.

To import another template, use the <%namespace> tag:

```
<%namespace name="mystuff" file="mystuff.html"/>
```

The above tag adds a local variable mystuff to the current scope.

Then, just call the defs off of mystuff:

```
${mystuff.somedef(x=5,y=7)}
```

The <%namespace> tag also supports some of the other semantics of Python's import statement, including pulling names into the local variable space, or using * to represent all names, using the import attribute:

<%namespace file="mystuff.html" import="foo, bar"/>

#### Calling Defs Programmatically

You can call defs programmatically from any Template object using the get_def() method, which returns a DefTemplate object.

```
from mako.template import Template

template = Template("""
    <%def name="hi(name)">
        hi ${name}!
    </%def>

    <%def name="bye(name)">
        bye ${name}!
    </%def>
""")

print template.get_def("hi").render(name="ed")
print template.get_def("bye").render(name="ed")
```

#### Defs within Defs

<%def> is basically def keyword in python to define function, so that nested <%def> is allowed.

```
<%def name="mydef()">
    <%def name="subdef()">
        a sub def
    </%def>

    i'm the def, and the subcomponent is ${subdef()}
</%def>
```

Just like Python, names that exist outside the inner <%def> exist inside it as well:

```
<%
    x = 12
%>
<%def name="outer()">
    <%
        y = 15
    %>
    <%def name="inner()">
        inner, x is ${x}, y is ${y}
    </%def>

    outer, x is ${x}, y is ${y}
</%def>
```

#### Calling a Def with Embedded Content and/or Other Defs

```
<%def name="buildtable()">
    <table>
        <tr><td>
            ${caller.body()}
        </td></tr>
    </table>
</%def>

<%self:buildtable>
    I am the table body.
</%self:buildtable>
```

This produces the output (whitespace formatted):

```
<table>
    <tr><td>
        I am the table body.
    </td></tr>
</table>
```

Using the older %call syntax looks like:

```
<%def name="buildtable()">
    <table>
        <tr><td>
            ${caller.body()}
        </td></tr>
    </table>
</%def>

<%call expr="buildtable()">
    I am the table body.
</%call>
```

The body() can be executed multiple times or not at all. This means you can use def-call-with-content to build iterators, conditionals, etc:

```
<%def name="lister(count)">
    % for x in range(count):
        ${caller.body()}
    % endfor
</%def>

<%self:lister count="${3}">
    hi
</%self:lister>
```

Produces:

```
hi
hi
hi
```

Notice above we pass 3 as a Python expression, so that it remains as an integer.

A custom "conditional" tag:

```
<%def name="conditional(expression)">
    % if expression:
        ${caller.body()}
    % endif
</%def>

<%self:conditional expression="${4==4}">
    i'm the result
</%self:conditional>
```

Produces:

```
i'm the result
```

The body() function also can handle arguments, which will augment the local namespace of the body callable. The caller must define the arguments which it expects to receive from its target def using the `args` attribute(`args="col"` below), which is a comma-separated list of argument names, we defined "col" below in caller's body as argument and then pass the value from customize tag `layoutdata` by `${caller.body(col=col)}`:

```
<%def name="layoutdata(somedata)">
    <table>
    % for item in somedata:
        <tr>
        % for col in item:
            <td>${caller.body(col=col)}</td>
        % endfor
        </tr>
    % endfor
    </table>
</%def>

<%self:layoutdata somedata="${[[1,2,3],[4,5,6],[7,8,9]]}" args="col">\
Body data: ${col}\
</%self:layoutdata>
```

Produces:

```
<table>
    <tr>
        <td>Body data: 1</td>
        <td>Body data: 2</td>
        <td>Body data: 3</td>
    </tr>
    <tr>
        <td>Body data: 4</td>
        <td>Body data: 5</td>
        <td>Body data: 6</td>
    </tr>
    <tr>
        <td>Body data: 7</td>
        <td>Body data: 8</td>
        <td>Body data: 9</td>
    </tr>
</table>
```

You don't have to stick to calling just the body() function. The caller can define any number of callables, allowing the <%call> tag to produce whole layouts:

```
<%def name="layout()">
    ## a layout def
    <div class="mainlayout">
        <div class="header">
            ${caller.header()}
        </div>

        <div class="sidebar">
            ${caller.sidebar()}
        </div>

        <div class="content">
            ${caller.body()}
        </div>
    </div>
</%def>

\#\# calls the layout def
<%self:layout>
    <%def name="header()">
        I am the header
    </%def>
    <%def name="sidebar()">
        <ul>
            <li>sidebar 1</li>
            <li>sidebar 2</li>
        </ul>
    </%def>

        this is the body
</%self:layout>
```

The above layout would produce:

```
<div class="mainlayout">
    <div class="header">
    I am the header
    </div>

    <div class="sidebar">
    <ul>
        <li>sidebar 1</li>
        <li>sidebar 2</li>
    </ul>
    </div>

    <div class="content">
    this is the body
    </div>
</div>
```

Basically anything you'd do with a "custom tag" or tag library in some other system, Mako provides via <%def> tags and plain Python callables which are invoked via <%namespacename:defname> or <%call>.


### Using Blocks

The `<%block>` tag compared with `<%def>` is more closely tailored towards layout.

```
<html>
    <body>
        <%block>
            this is a block.
        </%block>
    </body>
</html>
```

The block renders its content in the place that it's defined. Since there is no block `name` above, it's **anonymous block** which will produce:

```
<html>
    <body>
            this is a block.
    </body>
</html>
```

So in fact the above block has absolutely no effect. Its usefulness comes when we start using modifiers. Such as, we can apply a filter to our block:

```
<html>
    <body>
        <%block filter="h">
            <html>this is some escaped html.</html>
        </%block>
    </body>
</html>
```

`<html>this is some escaped html.</html>` will be converted to `&lt;html&gt;this is some escaped html.&lt;/html&gt;`

or perhaps a caching directive:

```
<html>
    <body>
        <%block cached="True" cache_timeout="60">
            This content will be cached for 60 seconds.
        </%block>
    </body>
</html>
```

Blocks also work in iterations, conditionals, just like defs:

```
% if some_condition:
    <%block>condition is met</%block>
% endif
```

Anonymous blocks are defined as closures in the local rendering body, so have access to local variable scope:

```
% for i in range(1, 4):
    <%block>i is ${i}</%block>
% endfor
```

#### Using Named Blocks

Possibly the more important area where blocks are useful is when we do actually give them names. In sharp contrast to the <%def> tag, the name given to a block is global for the entire template regardless of how deeply it's nested:

```
<html>
<%block name="header">
    <head>
        <title>
            <%block name="title">Title</%block>
        </title>
    </head>
</%block>
<body>
    ${next.body()}
</body>
</html>
```

The above example has two named blocks "header" and "title", both of which can be referred to by an inheriting template. A detailed walkthrough of this usage can be found at Inheritance.

Note above that named blocks don't have any argument declaration the way defs do.

```
<div name="page">
    <%block name="pagecontrol">
        <a href="">previous page</a> |
        <a href="">next page</a>
    </%block>

    <table>
        ## some content
    </table>

    ${pagecontrol()}
</div>
```

The content referenced by pagecontrol above will be rendered both above and below the <table> tags.

<%block> name should be unique to other <%block> and top level <%def>
A named <%block> cannot be defined within a <%def>, or inside the body of a "call", i.e. <%call> or <%namespacename:defname> tag. Anonymous blocks can, however.

#### Using Page Arguments in Named Blocks

Using arguments with the <%page> tag is described in the section [The body() Method](http://docs.makotemplates.org/en/latest/namespaces.html#namespaces-body), to allow a named block to share the same arguments passed to the page, the `args` attribute can be used:

```
<%page args="post"/>

<a name="${post.title}" />

<span class="post_prose">
    <%block name="post_prose" args="post">
        ${post.content}
    </%block>
</span>
```

Where above, if the template is called via a directive like <%include file="post.mako" args="post=post" />, the post variable is available both in the main body as well as the post_prose block.

Similarly, the **pageargs variable is present, in named blocks only, for those arguments not explicit in the <%page> tag:

```
<%block name="post_prose">
    ${pageargs['post'].content}
</%block>
```

The args attribute is only allowed with named blocks. With anonymous blocks, the Python function is always rendered in the same scope as the call itself, so anything available directly outside the anonymous block is available inside as well.


## The Mako Runtime Environment

### Context

### The Buffer

Occasionally, you want to programmatically send content to the output stream, such as within a `<% %>` block.
```
<%
    context.write("some programmatic text")
%>
```

### Context Variables

If you think UNDEFINE makes it hard to find what name is missing in Context, secify the option "strict_undefined=True" to the Template or TemplateLookup. This will cause any non-present variables to raise an immediate `NameError`, `UNDEFINED` is not used.

```
% if someval is UNDEFINED:
    someval is: no value
% else:
    someval is: ${someval}
% endif
```

what if I want to set values that are global to everyone within a template request?

Running the template looks like:

```
output = template.render(attributes={})
```

Within a template, just reference the dictionary:

```
<%
    attributes['foo'] = 'bar'
%>
'foo' attribute is: ${attributes['foo']}
```

### Context Methods and Accessors

* context[key] / context.get(key, default=None)
* context.keys - all the names defined within this context.
* context.kwargs - this returns a copy of the context's dictionary of variables. This is useful when you want to propagate the variables in the current context to a function as keyword arguments, i.e.:
```
${next.body(**context.kwargs)}
```
* context.write(text) - write some text to the current output stream.
* context.lookup - returns the `TemplateLookup` instance that is used for all file-lookups within the current execution.

### The Loop Context

Within `% for` blocks, the reserved name `loop` is available.

```
<ul>
% for a in ("one", "two", "three"):
    <li>Item ${loop.index}: ${a}</li>
% endfor
</ul>
```

#### Iterations

* `loop.index` - 0-indexed iteration count
* `loop.even` / `loop.odd` bools - loop parity
* `loop.first` bool - whether the loop is on its first iteration

If your iterable provides a `__len__` method, the below two attributes are available:

* loop.reverse_index - a count of iterations remaining
* loop.last - a bool indicating whether the loop is on its last iteration
Accessing them without `__len__` will raise a `TypeError`

#### Cycling

Cycling is available regardless of whether the iterable you’re using provides a __len__ method.

```
<ul>
% for i, item in enumerate(('spam', 'ham', 'eggs')):
  <li class="${'odd' if i % 2 else 'even'}">${item}</li>
% endfor
</ul>
```

With loop.cycle, you get the same results with cleaner code and less prep work:

```
<ul>
% for item in ('spam', 'ham', 'eggs'):
  <li class="${loop.cycle('even', 'odd')}">${item}</li>
% endfor
</ul>
```

Both approaches produce output like the following:

```
<ul>
  <li class="even">spam</li>
  <li class="odd">ham</li>
  <li class="even">eggs</li>
</ul>
```

#### Parent Loops

Access the parent loop context through `loop.parent` or `loop.parent.parent....`

```
<table>
% for consonant in 'pbj':
  <tr>
  % for vowel in 'iou':
    <td class="${'black' if (loop.parent.even == loop.even) else 'red'}">
      ${consonant + vowel}t
    </td>
  % endfor
  </tr>
% endfor
</table>
```

#### All the Built-in Names

See more details [here](http://docs.makotemplates.org/en/latest/runtime.html)

#### Reserved Names

See more details [here](http://docs.makotemplates.org/en/latest/runtime.html)

#### API Reference

See more details [here](http://docs.makotemplates.org/en/latest/runtime.html)


## Namespaces

If the file components.html defines these two defs:

```
 ## components.html
<%def name="comp1()">
    this is comp1
</%def>

<%def name="comp2(x)">
    this is comp2, x is ${x}
</%def>
```

you can make another file, for example index.html, that pulls those two defs into a namespace called comp:

```
 ## index.html
<%namespace name="comp" file="components.html"/>

Here's comp1:  ${comp.comp1()}
Here's comp2:  ${comp.comp2(x=5)}
```

Use the `import` attribute.

```
<%namespace file="components.html" import="comp1, comp2"/>

Heres comp1:  ${comp1()}
Heres comp2:  ${comp2(x=5)}
```

import also supports the “*” operator:

```
<%namespace file="components.html" import="*"/>

Heres comp1:  ${comp1()}
Heres comp2:  ${comp2(x=5)}
```

The file argument allows expressions – if looking for context variables, the context must be named explicitly:

```
<%namespace name="dyn" file="${context['namespace_name']}"/>
```

instead of being named implicitly like:

```
<%namespace name="dyn" file="${namespace_name}"/>
```

### Ways to Call Namespaces

**1. expressions like any other function:**

${mynamespace.somefunction('some arg1', 'some arg2', arg3='some arg3', arg4='some arg4')}

**2. "custom" Mako tag, with the function arguments passed in using named attributes:**

<%mynamespace:somefunction arg1="some arg1" arg2="some arg2" arg3="some arg3" arg4="some arg4"/>

**3. To embed Python expressions as arguments, use the embedded expression format:**

<%mynamespace:somefunction arg1="${someobject.format()}" arg2="${somedef(5, 12)}"/>

**4. The "custom tag" format is intended mainly for namespace functions which recognize body content(like `<%self:buildtable>` above, there should be a `caller.body()` in namespace function):**

<%mynamespace:somefunction arg1="some argument" args="x, y">
    Some record: ${x}, ${y}
</%mynamespace:somefunction>

### Namespaces from Regular Python Modules

Make sure the callables need to take at least one argument, `context`, an instance of `Context`. A module file `some/module.py` might contain the callable:

```
def my_tag(context):
    context.write("hello world")
    return ''
```

A template can use this module via:

```
<%namespace name="hw" module="some.module"/>

${hw.my_tag()}
```

The return value of `def` coming with `context.write()` is rendered after the `def` completes. So that "my_tag(context)" above should return '' instead of `None` if nothing should be rendered from "return"

To make sure your def is to be called in an "embedded content" context like `<%self:buildtable>` above, Add "@supports_caller" on your def and get body by conext['caller'].body():

```
from mako.runtime import supports_caller

@supports_caller
def my_tag(context):
    context.write("<div>")
    context['caller'].body()
    context.write("</div>")
    return ''
```

Capturing of output is available as well like this:

```
from mako.runtime import supports_caller, capture

@supports_caller
def my_tag(context):
    return "<div>%s</div>" % \
            capture(context, context['caller'].body, x="foo", y="bar")
```

### Declaring Defs in Namespaces

```
 ## define a namespace
<%namespace name="stuff">
    <%def name="comp1()">
        comp1
    </%def>
</%namespace>

 ## then call it
${stuff.comp1()}
```

### The `body()` Method

```
 ## base.html
<%page args="x, y, someval=8, scope='foo', **kwargs"/>
```

So above, the body might be called as:

```
 ## somefile.html
<%inherit file="base.html"/>

${self.body(5, y=10, someval=15, delta=7)}
```

The Context object also supplies a kwargs accessor, for cases when you’d like to pass along whatever is in the context to a body() callable:

${next.body(**context.kwargs)}

### Built-in Namespaces

`local`

The local namespace is basically the namespace for the currently executing template. This means that all of the top level defs defined in your template, as well as your template’s body() function, are also available off of the local namespace.

The local namespace is also where properties like uri, filename, and module and the get_namespace method can be particularly useful.

`self`

The self namespace, in the case of a template that does not use inheritance, is synonymous with local. If inheritance is used, then self references the topmost template in the inheritance chain. The `self` above represents the `base.html`

### Inheritable Namespaces

The <%namespace> tag includes an optional attribute inheritable="True", which will cause the namespace to be attached to the self namespace.

```
 ## base.html
<%namespace name="foo" file="foo.html" inheritable="True"/>

${next.body()}

 ## somefile.html
<%inherit file="base.html"/>

${self.foo.bar()}
```

### Namespace API Usage Example - Static Dependencies

#### Version One - Use Namespace.attr

The Namespace.attr attribute allows us to locate any variables declared in the <%! %> of a template.

```
 ## base.mako
 ## base-most template, renders layout etc.
<html>
<head>
 ## traverse through all namespaces present,
 ## look for an attribute named 'includes'
% for ns in context.namespaces.values():
    % for incl in getattr(ns.attr, 'includes', []):
        ${incl}
    % endfor
% endfor
</head>
<body>
${next.body()}
</body
</html>

 ## library.mako
 ## library functions.
<%!
    includes = [
        '<link rel="stylesheet" type="text/css" href="mystyle.css"/>',
        '<script type="text/javascript" src="functions.js"></script>'
    ]
%>

<%def name="mytag()">
    <form>
        ${caller.body()}
    </form>
</%def>

 ## index.mako
 ## calling template.
<%inherit file="base.mako"/>
<%namespace name="foo" file="library.mako"/>

<%foo:mytag>
    a form
</%foo:mytag>
```

#### Version Two - Use a specific named def

In this version, we put the includes into a <%def> that follows a naming convention.

```
 ## base.mako
 ## base-most template, renders layout etc.
<html>
<head>
 ## traverse through all namespaces present,
 ## look for a %def named 'includes'
% for ns in context.namespaces.values():
    % if hasattr(ns, 'includes'):
        ${ns.includes()}
    % endif
% endfor
</head>
<body>
${next.body()}
</body
</html>

 ## library.mako
 ## library functions.

<%def name="includes()">
    <link rel="stylesheet" type="text/css" href="mystyle.css"/>
    <script type="text/javascript" src="functions.js"></script>
</%def>

<%def name="mytag()">
    <form>
        ${caller.body()}
    </form>
</%def>

 ## index.mako
 ## calling template.
<%inherit file="base.mako"/>
<%namespace name="foo" file="library.mako"/>

<%foo:mytag>
    a form
</%foo:mytag>
```

### API Reference
See more details [here](http://docs.makotemplates.org/en/latest/namespaces.html#the-body-method)


