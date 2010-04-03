/**
* InputFilter.js: unobtrusive filtering of keystrokes for <input> tags
*
* This module finds all <input type="text"> elements in the document that
* have a nonstandard attribute named "allowed". It registers an onkeypress
* event handler for any such element to restrict the user's input so that
* only characters that appear in the value of the allowed attribute may be
* entered. If the <input> element also has an attribute named "messageid",
* the value of that attribute is taken to be the id of another document
* element. If the user types a character that is not allowed, the messageid
* element is made visible. If the user types a character that is allowed,
* the messageid element is hidden. This message id element is intended to
* offer an explanation to the user of why her keystroke was rejected. It
* should typically be styled with CSS so that it is initially invisible.
*
* Here is some sample HTML that uses this module.
* Zipcode:
* <input id="zip" type="text" allowed="0123456789" messageid="zipwarn">
* <span id="zipwarn" style="color:red;visibility:hidden">Digits only</SPAN> *
* In browsers such as IE, which do not support addEventListener( ), the
* keypress handler registered by this module overwrites any keypress handler
* defined in HTML.
*
* This module is purely unobtrusive: it does not define any symbols in
* the global namespace.
*/
(function( ) { // The entire module is within an anonymous function
// When the document finishes loading, call the init( ) function below
if (window.addEventListener) window.addEventListener("load", init, false);
else if (window.attachEvent) window.attachEvent("onload", init);
// Find all the <input> tags we need to register an event handler on
function init( ) {
var inputtags = document.getElementsByTagName("input");
for(var i = 0 ; i < inputtags.length; i++) { // Loop through all tags
var tag = inputtags[i];
if (tag.type != "text") continue; // We only want text fields
var allowed = tag.getAttribute("allowed");
if (!allowed) continue; // And only if they have an allowed attr
// Register our event handler function on this input tag
if (tag.addEventListener)
tag.addEventListener("keypress", filter, false);
else {
// We don't use attachEvent because it does not invoke the
// handler function with the correct value of the this keyword.
tag.onkeypress = filter;
}
}
}
// This is the keypress handler that filters the user's input
function filter(event) {
// Get the event object and character code in a portable way
var e = event || window.event; // Key event object
var code = e.charCode || e.keyCode; // What key was pressed
// If this keystroke is a function key of any kind, do not filter it
if (e.charCode == 0) return true; // Function key (Firefox only)
if (e.ctrlKey || e.altKey) return true; // Ctrl or Alt held down
if (code < 32) return true; // ASCII control character
// Now look up information we need from this input element
var allowed = this.getAttribute("allowed"); // Legal chars
var messageElement = null; // Message to hide/show
var messageid = this.getAttribute("messageid"); // Message id, if any
if (messageid) // If there is a message id, get the element
messageElement = document.getElementById(messageid);
// Convert the character code to a character
var c = String.fromCharCode(code);
// See if the character is in the set of allowed characters
if (allowed.indexOf(c) != -1) {
// If c is a legal character, hide the message, if any
if (messageElement) messageElement.style.visibility = "hidden";
return true; // And accept the character
}
else {
// If c is not in the set of allowed characters, display message
if (messageElement) messageElement.style.visibility = "visible";
// And reject this keypress event
if (e.preventDefault) e.preventDefault( );
if (e.returnValue) e.returnValue = false;
return false;
}
}
})( ); // Finish anonymous function and invoke it.

