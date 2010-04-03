/*
addEvent function found at http://www.scottandrew.com/weblog/articles/cbs-events
*/
function addEvent(obj, evType, fn) {
	if (obj.addEventListener) {
		obj.addEventListener(evType, fn, true);
		return true;
	} else if (obj.attachEvent) {
		var r = obj.attachEvent("on"+evType, fn);
		return r;
	} else {
		return false;
	}
}

/*
createElement function found at http://simon.incutio.com/archive/2003/06/15/javascriptWithXML
*/
function createElement(element) {
	if (typeof document.createElementNS != 'undefined') {
		return document.createElementNS('http://www.w3.org/1999/xhtml', element);
	}
	if (typeof document.createElement != 'undefined') {
		return document.createElement(element);
	}
	return false;
}

function insertTop(obj) {
	// Create the two div elements needed for the top of the box
	d=createElement("div");
	d.className="bt"; // The outer div needs a class name
    d2=createElement("div");
    d.appendChild(d2);
	obj.insertBefore(d,obj.firstChild);
}

function insertBottom(obj) {
	// Create the two div elements needed for the bottom of the box
	d=createElement("div");
	d.className="bb"; // The outer div needs a class name
    d2=createElement("div");
    d.appendChild(d2);
	obj.appendChild(d);
}

function initCB()
{
	// Find all div elements
	var divs = document.getElementsByTagName('div');
	var cbDivs = [];
	for (var i = 0; i < divs.length; i++) {
	// Find all div elements with cbb in their class attribute while allowing for multiple class names
		if (/\bcbb\b/.test(divs[i].className))
			cbDivs[cbDivs.length] = divs[i];
	}
	// Loop through the found div elements
	var thediv, outer, i1, i2;
	for (var i = 0; i < cbDivs.length; i++) {
	// Save the original outer div for later
		thediv = cbDivs[i];
	// 	Create a new div, give it the original div's class attribute, and replace 'cbb' with 'cb'
		outer = createElement('div');
		outer.className = thediv.className;
		outer.className = thediv.className.replace('cbb', 'cb');
	// Change the original div's class name and replace it with the new div
		thediv.className = 'i3';
		thediv.parentNode.replaceChild(outer, thediv);
	// Create two new div elements and insert them into the outermost div
		i1 = createElement('div');
		i1.className = 'i1';
		outer.appendChild(i1);
		i2 = createElement('div');
		i2.className = 'i2';
		i1.appendChild(i2);
	// Insert the original div
		i2.appendChild(thediv);
	// Insert the top and bottom divs
		insertTop(outer);
		insertBottom(outer);
	}
}

if(document.getElementById && document.createTextNode)
{
	addEvent(window, 'load', initCB);
}