/**
* Class.js: A module of utility functions for working with classes.
*
* This module defines a single global symbol named "Class".
* Class refers to a namespace object, and all utility functions
* are stored as properties of this namespace.
**/

// Create your own modules and namespaces, avoid defining global variables, see below:
// Create an empty object as our namespace
// This single global symbol will hold all of our other symbols
var Class = {}; // this is a object as namespace, not ordinary object,
                // remember we should define a real object class like 'function Person(name) {this.name = name;}'
// Define functions within the namespace
Class.define = function(data) { /* code goes here */ }
Class.provides = function(o, c) { /* code goes here */ }
// Conclusion: The FIRST rule: a module should never add more than a single symbol to the global namespace.

