/*
* Keymap.js: bind key events to handler functions.
*
* This module defines a Keymap class. An instance of this class represents a
* mapping of key identifiers (defined below) to handler functions. A
* Keymap can be installed on an HTML element to handle keydown and keypress
* events. When such an event occurs, the Keymap uses its mapping to invoke
* the appropriate handler function.
*
* When you create a Keymap, pass a JavaScript object that represents the
* initial set of bindings for the Keymap. The property names of this object
* are key identifers, and the property values are the handler functions.
*
* After a Keymap has been created, you can add new bindings by passing a key
* identifer and handler function to the bind( ) method.
You can remove a
* binding by passing a key identifier to the unbind( ) method.
*
* To make use of a Keymap, call its install( ) method, passing an HTML element,
* such as the document object. install( ) adds an onkeypress and onkeydown
* event handler to the specified object, replacing any handlers previously set
* on those properties. When these handlers are invoked, they determine the
* key identifier from the key event and invoke the handler function, if any,
* bound to that key identifier. If there is no mapping for the event, it uses
* the default handler function (see below), if one is defined. A single
* Keymap may be installed on more than one HTML element.
*
* Key Identifiers
*
* A key identifier is a case-insensitive string representation of a key plus
* any modifier keys that are held down at the same time. The key name is the
* name of the key: this is often the text that appears on the physical key of
* an English keyboard. Legal key names include "A", "7", "F2", "PageUp",
* "Left", "Delete", "/", "~". For printable keys, the key name is simply the
* character that the key generates. For nonprinting keys, the names are
* derived from the KeyEvent.DOM_VK_ constants defined by Firefox. They are
* simply the constant name, with the "DOM_VK_" portion and any underscores
* removed. For example, the KeyEvent constant DOM_VK_BACK_SPACE becomes
* BACKSPACE. See the Keymap.keyCodeToFunctionKey object in this module for a
* complete list of names.
*
* A key identifier may also include modifier key prefixes. These prefixes are
* Alt_, Ctrl_, and Shift_. They are case-insensitive, but if there is more
* than one, they must appear in alphabetical order. Some key identifiers that
* include modifiers include "Shift_A", "ALT_F2", and "alt_ctrl_delete". Note
* that "ctrl_alt_delete" is not legal because the modifiers are not in
* alphabetical order.
*
* Shifted punctuation characters are normally returned as the appropriate
* character. Shift-2 generates a key identifier of "@", for example. But if
* Alt or Ctrl is also held down, the unshifted symbol is used instead.
* We get a key identifier of Ctrl_Shift_2 instead of Ctrl_@, for example.
*
* Handler Functions
*
* When a handler function is invoked, it is passed three arguments:
* 1) the HTML element on which the key event occurred
* 2) the key identifier of the key that was pressed
* 3) the event object for the keydown event
*
* Default Handler
*
* The reserved key name "default" may be mapped to a handler function. That
* function will be invoked when no other key-specific binding exists.
*
* Limitations
*
* It is not possible to bind a handler function to all keys. The operating
* system traps some key sequences (Alt-F4, for example). And the browser
* itself may trap others (Ctrl-S, for example). This code is browser, OS,
* and locale-dependent. Function keys and modified function keys work well,
* and unmodified printable keys work well. The combination of Ctrl and Alt
* with printable characters, and particularly with punctuation characters, is
* less robust.
*/
// This is the constructor function
function Keymap(bindings) {
this.map = {}; // Define the key identifier->handler map
if (bindings) { // Copy initial bindings into it, converting to lowercase
for(name in bindings) this.map[name.toLowerCase( )] = bindings[name];
}
}
// Bind the specified key identifier to the specified handler function
Keymap.prototype.bind = function(key, func) {
this.map[key.toLowerCase( )] = func;
};
// Delete the binding for the specified key identifier
Keymap.prototype.unbind = function(key) {
delete this.map[key.toLowerCase( )];
};
// Install this Keymap on the specified HTML element
Keymap.prototype.install = function(element) {
// This is the event-handler function
var keymap = this;
function handler(event) { return keymap.dispatch(event); }
// Now install it
if (element.addEventListener) {
element.addEventListener("keydown", handler, false);
element.addEventListener("keypress", handler, false);
}
else if (element.attachEvent) {
element.attachEvent("onkeydown", handler);
element.attachEvent("onkeypress", handler);
}
else {
element.onkeydown = element.onkeypress = handler;
}
};
// This object maps keyCode values to key names for common nonprinting
// function keys. IE and Firefox use mostly compatible keycodes for these.
// Note, however that these keycodes may be device-dependent and different
// keyboard layouts may have different values.
Keymap.keyCodeToFunctionKey = {
8:"backspace", 9:"tab", 13:"return", 19:"pause", 27:"escape", 32:"space",
33:"pageup", 34:"pagedown", 35:"end", 36:"home", 37:"left", 38:"up",
39:"right", 40:"down", 44:"printscreen", 45:"insert", 46:"delete",
112:"f1", 113:"f2", 114:"f3", 115:"f4", 116:"f5", 117:"f6", 118:"f7",
119:"f8", 120:"f9", 121:"f10", 122:"f11", 123:"f12",
144:"numlock", 145:"scrolllock"
};
// This object maps keydown keycode values to key names for printable
// characters. Alphanumeric characters have their ASCII code, but
// punctuation characters do not. Note that this may be locale-dependent
// and may not work correctly on international keyboards.
Keymap.keyCodeToPrintableChar = {
48:"0", 49:"1", 50:"2", 51:"3", 52:"4", 53:"5", 54:"6", 55:"7", 56:"8",
57:"9", 59:";", 61:"=", 65:"a", 66:"b", 67:"c", 68:"d",
69:"e", 70:"f", 71:"g", 72:"h", 73:"i", 74:"j", 75:"k", 76:"l", 77:"m",
78:"n", 79:"o", 80:"p", 81:"q", 82:"r", 83:"s", 84:"t", 85:"u", 86:"v",
87:"w", 88:"x", 89:"y", 90:"z", 107:"+", 109:"-", 110:".", 188:",",
190:".", 191:"/", 192:"'", 219:"[", 220:"\\", 221:"]", 222:"\""
};
// This method dispatches key events based on the keymap bindings.
Keymap.prototype.dispatch = function(event) {
var e = event || window.event; // Handle IE event model
// We start off with no modifiers and no key name
var modifiers = ""
var keyname = null;
if (e.type == "keydown") {
var code = e.keyCode;
// Ignore keydown events for Shift, Ctrl, and Alt
if (code == 16 || code == 17 || code == 18) return;
// Get the key name from our mapping
keyname = Keymap.keyCodeToFunctionKey[code];
// If this wasn't a function key, but the ctrl or alt modifiers are
// down, we want to treat it like a function key
if (!keyname && (e.altKey || e.ctrlKey))
keyname = Keymap.keyCodeToPrintableChar[code];
// If we found a name for this key, figure out its modifiers.
// Otherwise just return and ignore this keydown event.
if (keyname) {
if (e.altKey) modifiers += "alt_";
if (e.ctrlKey) modifiers += "ctrl_";
if (e.shiftKey) modifiers += "shift_";
}
else return;
}
else if (e.type == "keypress") {
// If ctrl or alt are down, we've already handled it.
if (e.altKey || e.ctrlKey) return;
// In Firefox we get keypress events even for nonprinting keys.
// In this case, just return and pretend it didn't happen.
if (e.charCode != undefined && e.charCode == 0) return;
// Firefox gives us printing keys in e.charCode, IE in e.charCode
var code = e.charCode || e.keyCode;
// The code is an ASCII code, so just convert to a string.
keyname=String.fromCharCode(code);
// If the key name is uppercase, convert to lower and add shift
// We do it this way to handle CAPS LOCK; it sends capital letters
// without having the shift modifier set.
var lowercase = keyname.toLowerCase( );
if (keyname != lowercase) {
keyname = lowercase; // Use the lowercase form of the name
modifiers = "shift_"; // and add the shift modifier.
}
}
// Now that we've determined the modifiers and key name, we look for
// a handler function for the key and modifier combination
var func = this.map[modifiers+keyname];
// If we didn't find one, use the default handler, if it exists
if (!func) func = this.map["default"];
if (func) { // If there is a handler for this key, handle it
// Figure out what element the event occurred on
var target = e.target; // DOM standard event model
if (!target) target = e.srcElement; // IE event model
// Invoke the handler function
func(target, modifiers+keyname, e);
// Stop the event from propagating, and prevent the default action for
// the event. Note that preventDefault doesn't usually prevent
// top-level browser commands like F1 for help.
if (e.stopPropagation) e.stopPropagation( ); // DOM model
else e.cancelBubble = true; // IE model
if (e.preventDefault) e.preventDefault( ); // DOM
else e.returnValue = false; // IE
return false; // Legacy event model
}
};

