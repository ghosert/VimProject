/**
* DataEvent.js: send and receive ondataavailable events.
*
* This module defines two functions, DataEvent.send( ) and DataEvent.receive( ),
* for dispatching synthetic dataavailable events and registering event
* handlers for those events. The code is written to work in Firefox and other
* DOM-compliant browsers, and also in IE.
*
* The DOM event model allows synthetic events of any type, but the IE model
* supports only synthetic events of predefined types. dataavailable events
* are the most generic predefined type supported by IE and are used here.
*
* Note that events dispatched with DataEvent.send( ) are not queued the way
* real events would be. Instead, registered handlers are invoked immediately.
*/
var DataEvent = {};
/**
* Send a synthetic ondataavailable event to the specified target.
* The event object will include properties named datatype and data
* that have the specified values. datatype is intended to be a string
* or other primitive value (or null) identifying the type of this message,
* and data can be any JavaScript value, including an object or array.
*/
DataEvent.send = function(target, datatype, data) {
if (typeof target == "string") target = document.getElementById(target);
// Create an event object. If we can't create one, return silently
if (document.createEvent) { // DOM event model
// Create the event, specifying the name of the event module.
// For a mouse event, we'd use "MouseEvents".
var e = document.createEvent("Events");
// Initialize the event object, using a module-specific init method.
// Here we specify the event type, bubbling, and noncancelable.
// See Event.initEvent, MouseEvent.initMouseEvent, and UIEvent.initUIEvent
e.initEvent("dataavailable", true, false);
}
else if (document.createEventObject) { // IE event model
// In the IE event model, we just call this simple method
var e = document.createEventObject( );
}
else return; // Do nothing in other browsers
// Here we add some custom properties to the event object.
// We could set existing properties as well.
e.datatype = datatype;
e.data = data;
// Dispatch the event to the specified target.
if (target.dispatchEvent) target.dispatchEvent(e); // DOM
else if (target.fireEvent) target.fireEvent("ondataavailable", e); // IE
};
/**
* Register an event handler for an ondataavailable event on the specified
* target element.
*/
DataEvent.receive = function(target, handler) {
if (typeof target == "string") target = document.getElementById(target);
if (target.addEventListener)
target.addEventListener("dataavailable", handler, false);
else if (target.attachEvent)
target.attachEvent("ondataavailable", handler);
};

