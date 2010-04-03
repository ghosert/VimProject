/**
* flanagan/Class.js: A module of utility functions for working with classes.
*
* This module creates a single global symbol named "flanagan" if it
* does not already exist. It then creates a namespace object and stores
* it in the Class property of the flanagan object. All utility functions
* are placed in the flanagan.Class namespace.
**/

var flanagan; // Declare a single global symbol "flanagan"
if (!flanagan) flanagan = {}; // If undefined, make it an object
flanagan.Class = {} // Now create the flanagan.Class namespace
// Now populate the namespace with our utility methods
flanagan.Class.define = function(data) { /* code here */ };
flanagan.Class.provides = function(o, c) { /* code here */ };

