<html>
<head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
<style type="text/css">
    tr td {
		padding: 10px;
	}
	form {
		margin-top: 20px;
	}
</style>
</head>
<body>
	<h3>Index page for Persons:</h3>

	% if c.persons:
	<table>
		<tr><th>Id</th><th>Name</th><th>Email</th><th>Update</th><th>Delete</th></tr>
	% for person in c.persons:
		<form method="post">
			<tr>
				<td>${person.id}</td>
				<td><input name="name" type="text" value="${person.name}"/></td>
				<td><input name="email" type="text" value="${person.email}"/></td>
				<td><input type="button" value="Update" onclick="update_person(this.form, '${person.id}')"/></td>
				<td><input type="button" value="Delete" onclick="delete_person(this.form, '${person.id}')"/></td>
			</tr>
		</form>
	% endfor
	</table>
	% endif

	<form method="post">
		<span> name: </span>
		<input type="text" name="name"/>
		<span> email:</span>
		<input type="text" name="email"/>
		<input type="submit" value="Create new person" onclick="create_person(this.form, this.form.name.value, this.form.email.value);" />
	</form>

	<h3>Test on person pagination:</h3>

	<div>The pagination bar will only show when the pages are more than 1, means more than 10 persons created.</div>
	<div> ${c.persons.pager('Page $page: $link_previous $link_next ~4~')} </div>

	<h3>Pagination info about pagination page 1:</h3>
	<pre>
		${c.page2}
	</pre>

	<div>print c.page2 in a for loop</div>
	% for i in c.page2:
		<p>This is the ${i}</p>
	% endfor

	<p>
		<div>print default c.page2.pager() </div>
		<div>${c.page2.pager()}</div>
	</p>

	<p>
		<div>print c.page2.pager('~4~') </div>
		<div>${c.page2.pager('~4~')}</div>
	</p>

	<p>
		<div>print c.page2.pager('Page $page of $page_count - ~3~') </div>
		<div>${c.page2.pager('Page $page of $page_count - ~3~')}</div>
	</p>

	<p>
		<div>print c.page2.pager('$link_previous $link_next ~2~') </div>
		<div>${c.page2.pager('$link_previous $link_next ~2~')}</div>
	</p>

	<p>
		<div>print c.page2.pager('Item $first_item - $last_item / ~2~') </div>
		<div>${c.page2.pager('Item $first_item - $last_item / ~2~')}</div>
	</p>

	<h3>Test on File Upload:</h3>
	<div>
		<form method="post" action="/person/upload" enctype="multipart/form-data">
		    <div><input type="file" name="myfile"/></div>
			<div><input type="text" name="description"/></div>
			<div><input type="submit"/></div>
		</form>
	</div>

	<h3>Test on Form validation:</h3>
	<h3>Input something which is not email address here to see the error message, and form validation here only works on method='post' not 'get':</h3>
	<h3>And inputbox name in html: "email_validation_field" should be unique for all the other forms in html, otherwise, wired stuff will happen.</h3>
	<h3>And "email_validation_field" should be same name in controller: person.py and validator: form.py</h3>
	<div>
		<form name="test" method="post" action="/person/email">
			Email Address: <input type="text" name="email_validation_field" />
			<input type="submit" name="submit" value="Submit" />
		</form>
	</div>

	<h3>This form is produced by using Helpers (Should add 'from webhelpers.html.tags import *' first for leveraging h.form h.text below')</h3>
	<h3>Which is equivalent to the original form definition above.</h3>
	${h.form(h.url(controller='person', action='email'), method='post')}
	Email Address: ${h.text('email_validation_field')}
	${h.submit('idSubmit', 'Submit')}
	${h.end_form()}


</body>

<script type="text/javascript">
	function create_person(form, name, email) {
		form.action = "/person/create/" + name + "/" + email;
		form.submit();
	}
	function update_person(form, id, name, email) {
		form.action = "/person/save/" + id;
		form.submit();
	}
	function delete_person(form, id) {
		form.action = "/person/delete/" + id;
		form.submit();
	}
</script>

</html>
