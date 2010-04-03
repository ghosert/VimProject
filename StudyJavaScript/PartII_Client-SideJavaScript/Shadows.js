/**
* Shadows.js: shadowed text with CSS.
*
* This module defines a single global object named Shadows.
* The properties of this object are two utility functions.
*
* Shadows.add(element, shadows):
* Add the specified shadows to the specified element. The first argument
* is a document element or element id. This element must have a single
* text node as its child. This child is the one that will be shadowed.
* Shadows are specified with a string argument whose syntax is explained
* below.
*
* Shadows.addAll(root, tagname):
* Find all descendants of the specified root element that have the
* specified tagname. If any of these elements have an attribute named
* shadow, then call Shadows.add() for the element and the value of its
* shadow attribute. If tagname is not specified, all elements are checked.
* If root is not specified, the document object is used. This function is
* intended to be called once, when a document is first loaded.
*
* Shadow Syntax
*
* Shadows are specified by a string of the form [x y color]+. That is, one
* or more triplets specifying an x offset, a y offset, and a color. Each of
* these values must be in legal CSS format. If more than one shadow is
* specified, then the first shadow specified is on the bottom, overlapped
* by subsequent shadows. For example: "4px 4px #ccc 2px 2px #aaa"
*/
var Shadows = {};
// Add shadows to a single specified element
Shadows.add = function(element, shadows) {
if (typeof element == "string")
element = document.getElementById(element);
// Break the shadows string up at whitespace, first stripping off
// any leading and trailing spaces.
shadows = shadows.replace(/^\s+/, "").replace(/\s+$/, "");
var args = shadows.split(/\s+/);
// Find the text node that we are going to shadow.
// This module would be more robust if we shadowed all children.
// For simplicity, though, we're only going to do one.
var textnode = element.firstChild;
// Give the container element relative positioning, so that
// shadows can be positioned relative to it.
// We'll learn about scripting the style property in this way later.
element.style.position = "relative";
// Create the shadows
var numshadows = args.length/3; // how many shadows?
for(var i = 0; i < numshadows; i++) { // for each one
var shadowX = args[i*3]; // get the X offset
var shadowY = args[i*3 + 1]; // the Y offset
var shadowColor = args[i*3 + 2]; // and the color arguments
// Create a new <span> to hold the shadow
var shadow = document.createElement("span");
// Use its style attribute to specify offset and color
shadow.setAttribute("style", "position:absolute; " +
"left:" + shadowX + "; " +
"top:" + shadowY + "; " +
"color:" + shadowColor + ";");
// Add a copy of the text node to this shadow span
shadow.appendChild(textnode.cloneNode(false));
// And add the span to the container
element.appendChild(shadow);
}
// Now we put the text on top of the shadow. First, create a <span>
var text = document.createElement("span");
text.setAttribute("style", "position: relative"); // position it
text.appendChild(textnode); // Move the original text node to this span
element.appendChild(text); // And add this span to the container
};
// Scan the document tree at and beneath the specified root element for
// elements with the specified tagname. If any have a shadow attribute,
// pass it to the Shadows.add() method above to create the shadow.
// If root is omitted, use the document object. If tagname is omitted,
// search all tags.
Shadows.addAll = function(root, tagname) {
if (!root) root = document; // Use whole document if no root
if (!tagname) tagname = '*'; // Use any tag if no tagname specified
var elements = root.getElementsByTagName(tagname); // Find all tags
for(var i = 0; i < elements.length; i++) { // For each tag
var shadow = elements[i].getAttribute("shadow"); // If it has a shadow
if (shadow) Shadows.add(elements[i], shadow); // create the shadow
}
};

