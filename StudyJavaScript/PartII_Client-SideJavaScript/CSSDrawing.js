/**
* This constructor function creates a div element into which a
* CSS-based figure can be drawn. Instance methods are defined to draw
* lines and boxes and to insert the figure into the document.
*
* The constructor may be invoked using two different signatures:
*
* new CSSDrawing(x, y, width, height, classname, id)
*
* In this case a <div> is created using position:absolute at the
* specified position and size.
*
* The constructor may also be invoked with only a width and height:
*
* new CSSDrawing(width, height, classname, id)
*
* In this case, the created <div> has the specified width and height
* and uses position:relative (which is required so that the child
* elements used to draw lines and boxes can use absolute positioning).
*
* In both cases, the classname and id arguments are optional. If specified,
* they are used as the value of the class and id attributes of the created
* <div> and can be used to associate CSS styles, such as borders with
* the figure.
*/
function CSSDrawing(/* variable arguments, see above */) {
	// Create and remember the <div> element for the drawing
	var d = this.div = document.createElement("div");
	var next;
	// Figure out whether we have four numbers or two numbers, sizing and
	// positioning the div appropriately
	if (arguments.length >= 4 && typeof arguments[3] == "number") {
	d.style.position = "absolute";
	d.style.left = arguments[0] + "px";
	d.style.top = arguments[1] + "px";
	d.style.width = arguments[2] + "px";
	d.style.height = arguments[3] + "px";
	next = 4;
	}
	else {
	d.style.position = "relative"; // This is important
	d.style.width = arguments[0] + "px";
	d.style.height = arguments[1] + "px";
	next = 2;
	}
	// Set class and id attributes if they were specified.
	if (arguments[next]) d.className = arguments[next];
	if (arguments[next+1]) d.id = arguments[next+1];
}
/**
* Add a box to the drawing.
*
* x, y, w, h: specify the position and size of the box.
* content: a string of text or HTML that will appear in the box
* classname, id: optional class and id values for the box. Useful to
* associate styles with the box for color, border, etc.
* Returns: The <div> element created to display the box
*/
CSSDrawing.prototype.box = function(x, y, w, h, content, classname, id) {
	var d = document.createElement("div");
	if (classname) d.className = classname;
	if (id) d.id = id;
	d.style.position = "absolute";
	d.style.left = x + "px";
	d.style.top = y + "px";
	d.style.width = w + "px";
	d.style.height = h + "px";
	d.innerHTML = content;
	this.div.appendChild(d);
	return d;
};
/**
* Add a horizontal line to the drawing.
*
* x, y, width: specify start position and width of the line
* classname, id: optional class and id values for the box. At least one
* must be present and must specify a border style which
* will be used for the line style, color, and thickness.
* Returns: The <div> element created to display the line
*/
CSSDrawing.prototype.horizontal = function(x, y, width, classname, id) {
	var d = document.createElement("div");
	if (classname) d.className = classname;
	if (id) d.id = id;
	d.style.position = "absolute";
	d.style.left = x + "px";
	d.style.top = y + "px";
	d.style.width = width + "px";
	d.style.height = 1 + "px";
	d.style.borderLeftWidth = d.style.borderRightWidth =
	d.style.borderBottomWidth = "0px";
	this.div.appendChild(d);
	return d;
};
/**
* Add a vertical line to the drawing.
* See horizontal( ) for details.
*/
CSSDrawing.prototype.vertical = function(x, y, height, classname, id) {
	var d = document.createElement("div");
	if (classname) d.className = classname;
	if (id) d.id = id;
	d.style.position = "absolute";
	d.style.left = x + "px";
	d.style.top = y + "px";
	d.style.width = 1 + "px";
	d.style.height = height + "px";
	d.style.borderRightWidth = d.style.borderBottomWidth =
	d.style.borderTopWidth = "0px";
	this.div.appendChild(d);
	return d;
};
/** Add the drawing to the document as a child of the specified container */
CSSDrawing.prototype.insert = function(container) {
	if (typeof container == "string")
	container = document.getElementById(container);
	container.appendChild(this.div);
}
/** Add the drawing to the document by replacing the specified element */
CSSDrawing.prototype.replace = function(elt) {
	if (typeof elt == "string") elt = document.getElementById(elt);
	elt.parentNode.replaceChild(this.div, elt);
}

