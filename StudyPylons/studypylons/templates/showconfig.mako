<%inherit file="/base.mako" />

<%def name="head_tags()">
  <!-- add some head tags here -->
</%def>

<h3>The current configs in INI file in StudyPylons, for example: development.ini</h3>

<ul>
	% for key, value in c.config.iteritems():
		% if key == 'debug':
			<li><span style="font-weight: bold; color: red;">${key}: ${value}</span></li>
		% else:
			<li> ${key}: ${value} </li>
		% endif
	% endfor
<ul>
