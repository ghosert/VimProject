/*
* Handler.js -- Portable event-handler registration functions
*
* This module defines event-handler registration and deregistration functions
* Handler.add( ) and Handler.remove( ). Both functions take three arguments:
*
* element: the DOM element, document, or window on which the handler
* is to be added or removed.
*
* eventType: a string that specifies the type of event for which the
* handler is to be invoked. Use DOM-standard type names, which do
* not include an "on" prefix. Examples: "click", "load", "mouseover".
*
* handler: The function to be invoked when an event of the specified type
* occurs on the specified element. This function will be invoked as
* a method of the element on which it is registered, and the "this"
* keyword will refer to that element. The handler function will be
* passed an event object as its sole argument. This event object will
* either be a DOM-standard Event object or a simulated one. If a
* simulated event object is passed, it will have the following DOM-
* compliant properties: type, target, currentTarget, relatedTarget,
* eventPhase, clientX, clientY, screenX, screenY, altKey, ctrlKey,
* shiftKey, charCode, stopPropagation( ), and preventDefault( )
*
* Handler.add( ) and Handler.remove( ) have no return value.
*
* Handler.add( ) ignores duplicate registrations of the same handler for
* the same event type and element. Handler.remove( ) does nothing if called
* to remove a handler that has not been registered.
*
* Implementation notes:
*
* In browsers that support the DOM standard addEventListener( ) and
* removeEventListener( ) event-registration functions, Handler.add( ) and
* Handler.remove( ) simply invoke these functions, passing false as the
* third argument (meaning that the event handlers are never registered as
* capturing event handlers).
*
* In versions of Internet Explorer that support attachEvent( ), Handler.add( )
* and Handler.remove() use attachEvent( ) and detachEvent( ). To
* invoke the handler function with the correct this keyword, a closure is
* used. Since closures of this sort cause memory leaks in Internet Explorer,
* Handler.add( ) automatically registers an onunload handler to deregister
* all event handlers when the page is unloaded. To keep track of
* registered handlers, Handler.add( ) creates a property named _allHandlers on
* the window object and creates a property named _handlers on any element on
* which a handler is registered.
*/
var Handler = {};
// In DOM-compliant browsers, our functions are trivial wrappers around
// addEventListener( ) and removeEventListener( ).
if (document.addEventListener) {
Handler.add = function(element, eventType, handler) {
element.addEventListener(eventType, handler, false);
};
Handler.remove = function(element, eventType, handler) {
element.removeEventListener(eventType, handler, false);
};
}
// In IE 5 and later, we use attachEvent( ) and detachEvent( ), with a number of
// hacks to make them compatible with addEventListener and removeEventListener.
else if (document.attachEvent) {
Handler.add = function(element, eventType, handler) {
// Don't allow duplicate handler registrations
// _find( ) is a private utility function defined below.
if (Handler._find(element, eventType, handler) != -1) return;
// To invoke the handler function as a method of the
// element, we've got to define this nested function and register
// it instead of the handler function itself.
var wrappedHandler = function(e) {
if (!e) e = window.event;
// Create a synthetic event object with partial compatibility
// with DOM events.
var event = {
_event: e, // In case we really want the IE event object
type: e.type, // Event type
target: e.srcElement, // Where the event happened
currentTarget: element, // Where we're handling it
relatedTarget: e.fromElement?e.fromElement:e.toElement,
eventPhase: (e.srcElement==element)?2:3,
// Mouse coordinates
clientX: e.clientX, clientY: e.clientY,
screenX: e.screenX, screenY: e.screenY,
// Key state
altKey: e.altKey, ctrlKey: e.ctrlKey,
shiftKey: e.shiftKey, charCode: e.keyCode,
// Event-management functions
stopPropagation: function( ) {this._event.cancelBubble = true;},
preventDefault: function( ) {this._event.returnValue = false;}
}
// Invoke the handler function as a method of the element, passing
// the synthetic event object as its single argument.
// Use Function.call( ) if defined; otherwise do a hack
if (Function.prototype.call)
handler.call(element, event);
else {
// If we don't have Function.call, fake it like this.
element._currentHandler = handler;
element._currentHandler(event);
element._currentHandler = null;
}
};
// Now register that nested function as our event handler.
element.attachEvent("on" + eventType, wrappedHandler);
// Now we must do some record keeping to associate the user-supplied
// handler function and the nested function that invokes it.
// We have to do this so that we can deregister the handler with the
// remove( ) method and also deregister it automatically on page unload.
// Store all info about this handler into an object.
var h = {
element: element,
eventType: eventType,
handler: handler,
wrappedHandler: wrappedHandler
};
// Figure out what document this handler is part of.
// If the element has no "document" property, it is not
// a window or a document element, so it must be the document
// object itself.
var d = element.document || element;
// Now get the window associated with that document.
var w = d.parentWindow;
// We have to associate this handler with the window,
// so we can remove it when the window is unloaded.
var id = Handler._uid( ); // Generate a unique property name
if (!w._allHandlers) w._allHandlers = {}; // Create object if needed
w._allHandlers[id] = h; // Store the handler info in this object
// And associate the id of the handler info with this element as well.
if (!element._handlers) element._handlers = [];
element._handlers.push(id);
// If there is not an onunload handler associated with the window,
// register one now.
if (!w._onunloadHandlerRegistered) {
w._onunloadHandlerRegistered = true;
w.attachEvent("onunload", Handler._removeAllHandlers);
}
};
Handler.remove = function(element, eventType, handler) {
// Find this handler in the element._handlers[] array.
var i = Handler._find(element, eventType, handler);
if (i == -1) return; // If the handler was not registered, do nothing
// Get the window of this element.
var d = element.document || element;
var w = d.parentWindow;
// Look up the unique id of this handler.
var handlerId = element._handlers[i];
// And use that to look up the handler info.
var h = w._allHandlers[handlerId];
// Using that info, we can detach the handler from the element.
element.detachEvent("on" + eventType, h.wrappedHandler);
// Remove one element from the element._handlers array.
element._handlers.splice(i, 1);
// And delete the handler info from the per-window _allHandlers object.
delete w._allHandlers[handlerId];
};
// A utility function to find a handler in the element._handlers array
// Returns an array index or -1 if no matching handler is found
Handler._find = function(element, eventType, handler) {
var handlers = element._handlers;
if (!handlers) return -1; // if no handlers registered, nothing found
// Get the window of this element
var d = element.document || element;
var w = d.parentWindow;
// Loop through the handlers associated with this element, looking
// for one with the right type and function.
// We loop backward because the most recently registered handler
// is most likely to be the first removed one.
for(var i = handlers.length-1; i >= 0; i--) {
var handlerId = handlers[i]; // get handler id
var h = w._allHandlers[handlerId]; // get handler info
// If handler info matches type and handler function, we found it.
if (h.eventType == eventType && h.handler == handler)
return i;
}
return -1; // No match found
};
Handler._removeAllHandlers = function( ) {
// This function is registered as the onunload handler with
// attachEvent. This means that the this keyword refers to the
// window in which the event occurred.
var w = this;
// Iterate through all registered handlers
for(id in w._allHandlers) {
// Get handler info for this handler id
var h = w._allHandlers[id];
// Use the info to detach the handler
h.element.detachEvent("on" + h.eventType, h.wrappedHandler);
// Delete the handler info from the window
delete w._allHandlers[id];
}
}
// Private utility to generate unique handler ids
Handler._counter = 0;
Handler._uid = function( ) { return "h" + Handler._counter++; };
}

