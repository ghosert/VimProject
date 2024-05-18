/*
* listanchors.js: Create a simple table of contents with document.anchors[].
*
* The function listanchors() is passed a document as its argument and opens
* a new window to serve as a "navigation window" for that document. The new
* window displays a list of all anchors in the document. Clicking on any
* anchor in the list causes the document to scroll to that anchor.
*/
function listanchors(d) {
// Open a new window
var newwin = window.open("", "navwin",
"menubar=yes,scrollbars=yes,resizable=yes," +
"width=500,height=300");
// Give it a title
newwin.document.write("<h1>Navigation Window: " + d.title + "</h1>");
// List all anchors
for(var i = 0; i < d.anchors.length; i++) {
// For each anchor object, determine the text to display.
// First, try to get the text between <a> and </a> using a
// browser-dependent property. If none, use the name instead.
var a = d.anchors[i];
var text = null;
if (a.text) text = a.text; // Netscape 4
else if (a.innerText) text = a.innerText; // IE 4+
if ((text == null) || (text == '')) text = a.name; // Default
// Now output that text as a link. The href property of this link
// is never used: the onclick handler does the work, setting the
// location.hash property of the original window to make that
// window jump to display the named anchor. See Window.opener,
// Window.location and Location.hash, and Link.onclick.
newwin.document.write('<a href="#' + a.name + '"' +
' onclick="opener.location.hash=\'' + a.name +
'\'; return false;">');
newwin.document.write(text);
newwin.document.write('</a><br>');
}
newwin.document.close(); // Never forget to close the document!
}

