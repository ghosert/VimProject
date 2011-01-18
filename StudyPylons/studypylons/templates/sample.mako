<%inherit file="/base.mako" />

<%def name="head_tags()">
  <!-- add some head tags here -->
</%def>

<h1>Hi there ${c.name}!</h1>

<h3>Default Template Variables which can be used in mako template files.</h3>

<ul>
	<li> c – Template context object (Alias for tmpl_context)</li>
	<li> tmpl_context – Template context object</li>
	<li> config – Pylons PylonsConfig object (acts as a dict)</li>
	<li> g – Project application globals object (Alias for app_globals)</li>
	<li> app_globals – Project application globals object</li>
	<li> h – Project helpers module reference</li>
	<li> request – Pylons Request object for this request</li>
	<li> response – Pylons Response object for this request</li>
	<li> session – Pylons session object (unless Sessions are removed)</li>
	<li> translator – Gettext translator object configured for current locale</li>
	<li> ungettext() – Unicode capable version of gettext’s ngettext function (handles plural translations)</li>
	<li> _() – Unicode capable gettext translate function</li>
	<li> N_() – gettext no-op function to mark a string for translation, but doesn’t actually translate</li>
	<li> url – An instance of the routes.util.URLGenerator configured for this request.</li>
<ul>
