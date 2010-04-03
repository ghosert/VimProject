// Example 21-7. Building an HTML table from XML data
/**
* Extract data from the specified XML document and format it as an HTML table.
* Append the table to the specified HTML element. (If element is a string,
* it is taken as an element ID, and the named element is looked up.)
*
* The schema argument is a JavaScript object that specifies what data is to
* be extracted and how it is to be displayed. The schema object must have a
* property named "rowtag" that specifies the tag name of the XML elements that
* contain the data for one row of the table. The schema object must also have
* a property named "columns" that refers to an array. The elements of this
* array specify the order and content of the columns of the table. Each
* array element may be a string or a JavaScript object. If an element is a
* string, that string is used as the tag name of the XML element that contains
* table data for the column, and also as the column header for the column.
* If an element of the columns[] array is an object, it must have one property
* named "tagname" and one named "label". The tagname property is used to
* extract data from the XML document and the label property is used as the
* column header text. If the tagname begins with an @ character, it is
* an attribute of the row element rather than a child of the row.
*/
function makeTable(xmldoc, schema, element) {
	// Create the <table> element
	var table = document.createElement("table");
	// Create the header row of <th> elements in a <tr> in a <thead>
	var thead = document.createElement("thead");
	var header = document.createElement("tr");
	for(var i = 0; i < schema.columns.length; i++) {
		var c = schema.columns[i];
		var label = (typeof c == "string")?c:c.label;
		var cell = document.createElement("th");
		cell.appendChild(document.createTextNode(label));
		header.appendChild(cell);
	}
	// Put the header into the table
	thead.appendChild(header);
	table.appendChild(thead);
	// The remaining rows of the table go in a <tbody>
	var tbody = document.createElement("tbody");
	table.appendChild(tbody);
	// Now get the elements that contain our data from the xml document
	var xmlrows = xmldoc.getElementsByTagName(schema.rowtag);
	// Loop through these elements. Each one contains a row of the table.
	for(var r=0; r < xmlrows.length; r++) {
		// This is the XML element that holds the data for the row
		var xmlrow = xmlrows[r];
		// Create an HTML element to display the data in the row
		var row = document.createElement("tr");
		// Loop through the columns specified by the schema object
		for(var c = 0; c < schema.columns.length; c++) {
			var sc = schema.columns[c];
			var tagname = (typeof sc == "string")?sc:sc.tagname;
			var celltext;
			if (tagname.charAt(0) == '@') {
				// If the tagname begins with '@', it is an attribute name
				celltext = xmlrow.getAttribute(tagname.substring(1));
			}
			else {
				// Find the XML element that holds the data for this column
				var xmlcell = xmlrow.getElementsByTagName(tagname)[0];
				// Assume that element has a text node as its first child
				var celltext = xmlcell.firstChild.data;
			}
			// Create the HTML element for this cell
			var cell = document.createElement("td");
			// Put the text data into the HTML cell
			cell.appendChild(document.createTextNode(celltext));
			// Add the cell to the row
			row.appendChild(cell);
		}
		// And add the row to the tbody of the table
		tbody.appendChild(row);
	}
	// Set an HTML attribute on the table element by setting a property.
	// Note that in XML we must use setAttribute( ) instead.
	table.frame = "border";
	// Now that we've created the HTML table, add it to the specified element.
	// If that element is a string, assume it is an element ID.
	if (typeof element == "string") element = document.getElementById(element);
	element.appendChild(table);
}

/**
* This XML.Transformer class encapsulates an XSL stylesheet.
* If the stylesheet parameter is a URL, we load it.
* Otherwise, we assume it is an appropriate DOM Document.
*/
XML.Transformer = function(stylesheet) {
	// Load the stylesheet if necessary.
	if (typeof stylesheet == "string") stylesheet = XML.load(stylesheet);
	this.stylesheet = stylesheet;
	// In Mozilla-based browsers, create an XSLTProcessor object and
	// tell it about the stylesheet.
	if (typeof XSLTProcessor != "undefined") {
		this.processor = new XSLTProcessor( );
		this.processor.importStylesheet(this.stylesheet);
	}
};
/**
* This is the transform( ) method of the XML.Transformer class.
* It transforms the specified xml node using the encapsulated stylesheet.
* The results of the transformation are assumed to be HTML and are used to
* replace the content of the specified element.
*/
XML.Transformer.prototype.transform = function(node, element) {
	// If element is specified by id, look it up.
	if (typeof element == "string") element = document.getElementById(element);
	if (this.processor) {
		// If we've created an XSLTProcessor (i.e., we're in Mozilla) use it.
		// Transform the node into a DOM DocumentFragment.
		var fragment = this.processor.transformToFragment(node, document);
		// Erase the existing content of element.
		element.innerHTML = "";
		// And insert the transformed nodes.
		element.appendChild(fragment);
	}
	else if ("transformNode" in node) {
		// If the node has a transformNode( ) function (in IE), use that.
		// Note that transformNode( ) returns a string.
		element.innerHTML = node.transformNode(this.stylesheet);
	}
	else {
		// Otherwise, we're out of luck.
		throw "XSLT is not supported in this browser";
	}
};
/**
* This is an XSLT utility function that is useful when a stylesheet is
* used only once.
*/
XML.transform = function(xmldoc, stylesheet, element) {
	var transformer = new XML.Transformer(stylesheet);
	transformer.transform(xmldoc, element);
}

