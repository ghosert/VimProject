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
				<td><input name="name" type="textbox" value="${person.name}"/></td>
				<td><input name="email" type="textbox" value="${person.email}"/></td>
				<td><input type="button" value="Update" onclick="update_person(this.form, '${person.id}')"/></td>
				<td><input type="button" value="Delete" onclick="delete_person(this.form, '${person.id}')"/></td>
			</tr>
		</form>
	% endfor
	</table>
	% endif

	<div>The pagination bar will only show when the pages are more than 1, means more than 10 persons created.</div>
	<div> ${c.persons.pager('Page $page: $link_previous $link_next ~4~')} </div>

	<form method="post">
		<span> name: </span>
		<input type="textbox" name="name"/>
		<span> email:</span>
		<input type="textbox" name="email"/>
		<input type="submit" value="Create new person" onclick="create_person(this.form, this.form.name.value, this.form.email.value);" />
	</form>

	<div>Pagination info about pagination page 1:</div>
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
