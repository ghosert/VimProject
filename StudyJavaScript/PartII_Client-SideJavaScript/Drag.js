/**
* Drag.js: drag absolutely positioned HTML elements.
*
* This module defines a single drag( ) function that is designed to be called
* from an onmousedown event handler. Subsequent mousemove events will
* move the specified element. A mouseup event will terminate the drag.
* If the element is dragged off the screen, the window does not scroll.
* This implementation works with both the DOM Level 2 event model and the
* IE event model.
*
* Arguments:
*
* elementToDrag: the element that received the mousedown event or
* some containing element. It must be absolutely positioned. Its
* style.left and style.top values will be changed based on the user's
* drag.
*
* event: the Event object for the mousedown event.
**/
function drag(elementToDrag, event) {
	// The mouse position (in window coordinates)
	// at which the drag begins
	var startX = event.clientX, startY = event.clientY;
	// The original position (in document coordinates) of the
	// element that is going to be dragged. Since elementToDrag is
	// absolutely positioned, we assume that its offsetParent is the
	// document body.
	var origX = elementToDrag.offsetLeft, origY = elementToDrag.offsetTop;
	// Even though the coordinates are computed in different
	// coordinate systems, we can still compute the difference between them
	// and use it in the moveHandler( ) function. This works because
	// the scrollbar position never changes during the drag.
	var deltaX = startX - origX, deltaY = startY - origY;
	// Register the event handlers that will respond to the mousemove events
	// and the mouseup event that follow this mousedown event.
	if (document.addEventListener) { // DOM Level 2 event model
		// Register capturing event handlers
		document.addEventListener("mousemove", moveHandler, true);
		document.addEventListener("mouseup", upHandler, true);
	}
	else if (document.attachEvent) { // IE 5+ Event Model
		// In the IE event model, we capture events by calling
		// setCapture( ) on the element to capture them.
		elementToDrag.setCapture( );
		elementToDrag.attachEvent("onmousemove", moveHandler);
		elementToDrag.attachEvent("onmouseup", upHandler);
		// Treat loss of mouse capture as a mouseup event.
		elementToDrag.attachEvent("onlosecapture", upHandler);
	}
	else { // IE 4 Event Model
		// In IE 4 we can't use attachEvent( ) or setCapture( ), so we set
		// event handlers directly on the document object and hope that the
		// mouse events we need will bubble up.
		var oldmovehandler = document.onmousemove; // used by upHandler( )
		var olduphandler = document.onmouseup;
		document.onmousemove = moveHandler;
		document.onmouseup = upHandler;
	}
	// We've handled this event. Don't let anybody else see it.
	if (event.stopPropagation) event.stopPropagation( ); // DOM Level 2
	else event.cancelBubble = true; // IE
	// Now prevent any default action.
	if (event.preventDefault) event.preventDefault( ); // DOM Level 2
	else event.returnValue = false; // IE

	/**
	* This is the handler that captures mousemove events when an element
	* is being dragged. It is responsible for moving the element.
	**/
	function moveHandler(e) {
		if (!e) e = window.event; // IE Event Model
		// Move the element to the current mouse position, adjusted as
		// necessary by the offset of the initial mouse-click.
		elementToDrag.style.left = (e.clientX - deltaX) + "px";
		elementToDrag.style.top = (e.clientY - deltaY) + "px";
		// And don't let anyone else see this event.
		if (e.stopPropagation) e.stopPropagation( ); // DOM Level 2
		else e.cancelBubble = true; // IE
	}

	/**
	* This is the handler that captures the final mouseup event that
	* occurs at the end of a drag.
	**/
	function upHandler(e) {
		if (!e) e = window.event; // IE Event Model
		// Unregister the capturing event handlers.
		if (document.removeEventListener) { // DOM event model
			document.removeEventListener("mouseup", upHandler, true);
			document.removeEventListener("mousemove", moveHandler, true);
		}
		else if (document.detachEvent) { // IE 5+ Event Model
			elementToDrag.detachEvent("onlosecapture", upHandler);
			elementToDrag.detachEvent("onmouseup", upHandler);
			elementToDrag.detachEvent("onmousemove", moveHandler);
			elementToDrag.releaseCapture( );
		}
		else { // IE 4 Event Model
			// Restore the original handlers, if any
			document.onmouseup = olduphandler;
			document.onmousemove = oldmovehandler;
		}
		// And don't let the event propagate any further.
		if (e.stopPropagation) e.stopPropagation( ); // DOM Level 2
		else e.cancelBubble = true; // IE
	}
}

