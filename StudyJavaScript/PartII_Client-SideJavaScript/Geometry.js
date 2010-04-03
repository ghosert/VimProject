/**
* Geometry.js: portable functions for querying window and document geometry
*
* This module defines functions for querying window and document geometry.
*
* getWindowX/Y( ): return the position of the window on the screen
* getViewportWidth/Height( ): return the size of the browser viewport area
* getDocumentWidth/Height( ): return the size of the document
* getHorizontalScroll( ): return the position of the horizontal scrollbar
* getVerticalScroll( ): return the position of the vertical scrollbar
*
* Note that there is no portable way to query the overall size of the
* browser window, so there are no getWindowWidth/Height( ) functions.
*
* IMPORTANT: This module must be included in the <body> of a document
* instead of the <head> of the document.
*/
var Geometry = {};
if (window.screenLeft) { // IE and others
Geometry.getWindowX = function( ) { return window.screenLeft; };
Geometry.getWindowY = function( ) { return window.screenTop; };
}
else if (window.screenX) { // Firefox and others
Geometry.getWindowX = function( ) { return window.screenX; };
Geometry.getWindowY = function( ) { return window.screenY; };
}
if (window.innerWidth) { // All browsers but IE
Geometry.getViewportWidth = function( ) { return window.innerWidth; };
Geometry.getViewportHeight = function( ) { return window.innerHeight; };
Geometry.getHorizontalScroll = function( ) { return window.pageXOffset; };
Geometry.getVerticalScroll = function( ) { return window.pageYOffset; };
}
else if (document.documentElement && document.documentElement.clientWidth) {
// These functions are for IE 6 when there is a DOCTYPE
Geometry.getViewportWidth =
function( ) { return document.documentElement.clientWidth; };
Geometry.getViewportHeight =
function( ) { return document.documentElement.clientHeight; };
Geometry.getHorizontalScroll =
function( ) { return document.documentElement.scrollLeft; };
Geometry.getVerticalScroll =
function( ) { return document.documentElement.scrollTop; };
}
else if (document.body.clientWidth) {
// These are for IE4, IE5, and IE6 without a DOCTYPE
Geometry.getViewportWidth =
function( ) { return document.body.clientWidth; };
Geometry.getViewportHeight =
function( ) { return document.body.clientHeight; };
Geometry.getHorizontalScroll =
function( ) { return document.body.scrollLeft; };
Geometry.getVerticalScroll =
function( ) { return document.body.scrollTop; };
}
// These functions return the size of the document. They are not window
// related, but they are useful to have here anyway.
if (document.documentElement && document.documentElement.scrollWidth) {
Geometry.getDocumentWidth =
function( ) { return document.documentElement.scrollWidth; };
Geometry.getDocumentHeight =
function( ) { return document.documentElement.scrollHeight; };
}
else if (document.body.scrollWidth) {
Geometry.getDocumentWidth =
function( ) { return document.body.scrollWidth; };
Geometry.getDocumentHeight =
function( ) { return document.body.scrollHeight; };
}

