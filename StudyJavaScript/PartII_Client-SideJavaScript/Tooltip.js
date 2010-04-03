/**
* Tooltip.js: simple CSS tool tips with drop shadows.
*
* This module defines a Tooltip class. Create a Tooltip object with the
* Tooltip() constructor. Then make it visible with the show() method.
* When done, hide it with the hide() method.
*
* Note that this module must be used with appropriate CSS class definitions
* to display correctly. The following are examples:
*
* .tooltipShadow {
* background: url(shadow.png); /* translucent shadow * /
* }
*
* .tooltipContent {
* left: -4px; top: -4px; /* how much of the shadow shows * /
* background-color: #ff0; /* yellow background * /
* border: solid black 1px; /* thin black border * /
* padding: 5px; /* spacing between text and border * /
* font: bold 10pt sans-serif; /* small bold font * /
* }
*
* In browsers that support translucent PNG images, it is possible to display
* translucent drop shadows. Other browsers must use a solid color or
* simulate transparency with a dithered GIF image that alternates solid and
* transparent pixels.
*/
function Tooltip() { // The constructor function for the Tooltip class
this.tooltip = document.createElement("div"); // create div for shadow
this.tooltip.style.position = "absolute"; // absolutely positioned
this.tooltip.style.visibility = "hidden"; // starts off hidden
this.tooltip.className = "tooltipShadow"; // so we can style it
this.content = document.createElement("div"); // create div for content
this.content.style.position = "relative"; // relatively positioned
this.content.className = "tooltipContent"; // so we can style it
this.tooltip.appendChild(this.content); // add content to shadow
}
// Set the content and position of the tool tip and display it
Tooltip.prototype.show = function(text, x, y) {
this.content.innerHTML = text; // Set the text of the tool tip.
this.tooltip.style.left = x + "px"; // Set the position.
this.tooltip.style.top = y + "px";
this.tooltip.style.visibility = "visible"; // Make it visible.
// Add the tool tip to the document if it has not been added before
if (this.tooltip.parentNode != document.body)
document.body.appendChild(this.tooltip);
};
// Hide the tool tip
Tooltip.prototype.hide = function() {
this.tooltip.style.visibility = "hidden"; // Make it invisible.
};

// The following values are used by the schedule( ) method below.
// They are used like constants but are writable so that you can override
// these default values.
Tooltip.X_OFFSET = 25; // Pixels to the right of the mouse pointer
Tooltip.Y_OFFSET = 15; // Pixels below the mouse pointer
Tooltip.DELAY = 500; // Milliseconds after mouseover
/**
* This method schedules a tool tip to appear over the specified target
* element Tooltip.DELAY milliseconds from now. The argument e should
* be the event object of a mouseover event. This method extracts the
* mouse coordinates from the event, converts them from window
* coordinates to document coordinates, and adds the offsets above.
* It determines the text to display in the tool tip by querying the
* "tooltip" attribute of the target element. This method
* automatically registers and unregisters an onmouseout event handler
* to hide the tool tip or cancel its pending display.
*/
Tooltip.prototype.schedule = function(target, e) {
// Get the text to display. If none, we don't do anything.
var text = target.getAttribute("tooltip");
if (!text) return;
// The event object holds the mouse position in window coordinates.
// We convert these to document coordinates using the Geometry module.
var x = e.clientX + Geometry.getHorizontalScroll( );
var y = e.clientY + Geometry.getVerticalScroll( );
// Add the offsets so the tool tip doesn't appear right under the mouse.
x += Tooltip.X_OFFSET;
y += Tooltip.Y_OFFSET;
// Schedule the display of the tool tip.
var self = this; // We need this for the nested functions below
var timer = window.setTimeout(function( ) { self.show(text, x, y); },
Tooltip.DELAY);
// Also, register an onmouseout handler to hide a tool tip or cancel
// the pending display of a tool tip.
if (target.addEventListener) target.addEventListener("mouseout", mouseout,
false);
else if (target.attachEvent) target.attachEvent("onmouseout", mouseout);
else target.onmouseout = mouseout;
// Here is the implementation of the event listener
function mouseout( ) {
self.hide( ); // Hide the tool tip if it is displayed,
window.clearTimeout(timer); // cancel any pending display,
// and remove ourselves so we're called only once
if (target.removeEventListener)
target.removeEventListener("mouseout", mouseout, false);
else if (target.detachEvent) target.detachEvent("onmouseout",mouseout);
else target.onmouseout = null;
}
}

