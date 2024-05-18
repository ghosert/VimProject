/*
* Log.js: Unobtrusive logging facility
*
* This module defines a single global symbol: a function named log().
* Log a message by calling this function with 2 or 3 arguments:
*
* category: the type of the message. This is required so that messages
* of different types can be selectively enabled or disabled and so
* that they can be styled independently. See below.
*
* message: the text to be logged. May be empty if an object is supplied
*
* object: an object to be logged. This argument is optional. If passed,
* the properties of the object will be logged in the form of a table.
* Any property whose value is itself an object may be logged recursively.
*
* Utility Functions:
*
* The log.debug() and log.warn() functions are utilities that simply
* call the log() function with hardcoded categories of "debug" and
* "warning". It is trivial to define a utility that replaces the built-in
* alert() method with one that calls log().
*
* Enabling Logging
*
* Log messages are *not* displayed by default. You can enable the
* display of messages in a given category in one of two ways. The
* first is to create a <div> or other container element with an id
* of "<category>_log". For messages whose category is "debug", you might
* place the following in the containing document:
*
* <div id="debug_log"></div>
*
* In this case, all messages of the specified category are appended
* to this container, which can be styled however you like.
*
* The second way to enable messages for a given category is to
* set an appropriate logging option. To enable the category
* "debug", you'd set log.options.debugEnabled = true. When you
* do this, a <div class="log"> is created for the logging messages.
* If you want to disable the display of log messages, even if a container
* with a suitable id exists, set another option:
* log.options.debugDisabled=true. Set this option back to false to
* re-enable log messages of that category.
*
* Styling Log Messages
*
* In addition to styling the log container, you can use CSS to
* style the display of individual log messages. Each log message
* is placed in a <div> tag, and given a CSS class of
* <category>_message. Debugging messages would have a class "debug_message"
*
* Log Options
*
* Logging behavior can be altered by setting properties of the log.options
* object, such as the options described earlier to enable or disable logging
* for given categories. A few other options are available:
*
* log.options.timestamp: If this property is true, each log message
* will have the date and time added to it.
*
* log.options.maxRecursion: An integer that specifies the maximum number
* of nested tables to display when logging objects. Set this to 0 if
* you never want a table within a table.
*
* log.options.filter: A function that filters properties out when logging
* an object. A filter function is passed the name and value of
* a property and returns true if the property should appear in the
* object table or false otherwise.
*/
function log(category, message, object) {
// If this category is explicitly disabled, do nothing
if (log.options[category + "Disabled"]) return;
// Find the container
var id = category + "_log";
var c = document.getElementById(id);
// If there is no container, but logging in this category is enabled,
// create the container.
if (!c && log.options[category + "Enabled"]) {
c = document.createElement("div");
c.id = id;
c.className = "log";
document.body.appendChild(c);
}
// If still no container, we ignore the message
if (!c) return;
// If timestamping is enabled, add the timestamp
if (log.options.timestamp)
message = new Date() + ": " + (message?message:"");
// Create a <div> element to hold the log entry
var entry = document.createElement("div");
entry.className = category + "_message";

if (message) {
// Add the message to it
entry.appendChild(document.createTextNode(message));
}
if (object && typeof object == "object") {
entry.appendChild(log.makeTable(object, 0));
}
// Finally, add the entry to the logging container
c.appendChild(entry);
}
// Create a table to display the properties of the specified object
log.makeTable = function(object, level) {
// If we've reached maximum recursion, return a Text node instead.
if (level > log.options.maxRecursion)
return document.createTextNode(object.toString());
// Create the table we'll be returning
var table = document.createElement("table");
table.border = 1;
// Add a Name|Type|Value header to the table
var header = document.createElement("tr");
var headerName = document.createElement("th");
var headerType = document.createElement("th");
var headerValue = document.createElement("th");
headerName.appendChild(document.createTextNode("Name"));
headerType.appendChild(document.createTextNode("Type"));
headerValue.appendChild(document.createTextNode("Value"));
header.appendChild(headerName);
header.appendChild(headerType);
header.appendChild(headerValue);
table.appendChild(header);
// Get property names of the object and sort them alphabetically
var names = [];
for(var name in object) names.push(name);
names.sort();
// Now loop through those properties
for(var i = 0; i < names.length; i++) {
var name, value, type;
name = names[i];
try {
value = object[name];
type = typeof value;
}
catch(e) { // This should not happen, but it can in Firefox
value = "<unknown value>";
type = "unknown";
};
// Skip this property if it is rejected by a filter
if (log.options.filter && !log.options.filter(name, value)) continue;
// Never display function source code: it takes up too much room
if (type == "function") value = "{/*source code suppressed*/}";
// Create a table row to display property name, type and value
var row = document.createElement("tr");
row.vAlign = "top";
var rowName = document.createElement("td");
var rowType = document.createElement("td");
var rowValue = document.createElement("td");
rowName.appendChild(document.createTextNode(name));
rowType.appendChild(document.createTextNode(type));
// For objects, recurse to display them as tables
if (type == "object")
rowValue.appendChild(log.makeTable(value, level+1));
else
rowValue.appendChild(document.createTextNode(value));
// Add the cells to the row, and add the row to the table
row.appendChild(rowName);
row.appendChild(rowType);
row.appendChild(rowValue);
table.appendChild(row);
}
// Finally, return the table.
return table;
}
// Create an empty options object
log.options = {};
// Utility versions of the function with hardcoded categories
log.debug = function(message, object) { log("debug", message, object); };
log.warn = function(message, object) { log("warning", message, object); };
// Uncomment the following line to convert alert() dialogs to log messages
// function alert(msg) { log("alert", msg); }

