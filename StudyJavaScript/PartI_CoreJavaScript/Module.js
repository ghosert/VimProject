/**
* Module.js: module and namspace utilities
*
* This is a module of module-related utility functions that are
* compatible with JSAN-type modules.
* This module defines the namespace Module.
*/

// Make sure we haven't already been loaded
var Module;
if (Module && (typeof Module != "object" || Module.NAME))
	throw new Error("Namespace 'Module' already exists");

// Create our namespace
Module = {};

// This is some metainformation about this namespace
Module.NAME = "Module"; // The name of this namespace
Module.VERSION = 0.1; // The version of this namespace

// This is the list of public symbols that we export from this namespace.
// These are of interest to programers who use modules.
Module.EXPORT = ["require", "importSymbols"];

// These are other symbols we are willing to export. They are ones normally
// used only by module authors and are not typically imported.
Module.EXPORT_OK = ["createNamespace", "isDefined",
"registerInitializationFunction",
"runInitializationFunctions",
"modules", "globalNamespace"];

// Now start adding symbols to the namespace
Module.globalNamespace = this; // So we can always refer to the global scope
Module.modules = { "Module": Module }; // Module name->namespace map.

/**
* This function creates and returns a namespace object for the
* specified name and does useful error checking to ensure that the
* name does not conflict with any previously loaded module. It
* throws an error if the namespace already exists or if any of the
* property components of the namespace exist and are not objects.
*
* Sets a NAME property of the new namespace to its name.
* If the version argument is specified, set the VERSION property
* of the namespace.
*
* A mapping for the new namespace is added to the Module.modules object
*/
Module.createNamespace = function(name, version) {
	// Check name for validity. It must exist, and must not begin or
	// end with a period or contain two periods in a row.
	if (!name) throw new Error("Module.createNamespace( ): name required");
	if (name.charAt(0) == '.' ||
	name.charAt(name.length-1) == '.' ||
	name.indexOf("..") != -1)
		throw new Error("Module.createNamespace( ): illegal name: " + name);
	// Break the name at periods and create the object hierarchy we need
	var parts = name.split('.');
	// For each namespace component, either create an object or ensure that
	// an object by that name already exists.
	var container = Module.globalNamespace;
	for(var i = 0; i < parts.length; i++) {
		var part = parts[i];
		// If there is no property of container with this name, create
		// an empty object.
		if (!container[part]) container[part] = {};
		else if (typeof container[part] != "object") {
			// If there is already a property, make sure it is an object
			var n = parts.slice(0,i).join('.');
			throw new Error(n + " already exists and is not an object");
		}
		container = container[part];
	}
	// The last container traversed above is the namespace we need.
	var namespace = container;
	// It is an error to define a namespace twice. It is okay if our
	// namespace object already exists, but it must not already have a
	// NAME property defined.
	if (namespace.NAME) throw new Error("Module "+name+" is already defined");
	// Initialize name and version fields of the namespace
	namespace.NAME = name;
	if (version) namespace.VERSION = version;
	// Register this namespace in the map of all modules
	Module.modules[name] = namespace;
	// Return the namespace object to the caller
	return namespace;
}

/**
* Test whether the module with the specified name has been defined.
* Returns true if it is defined and false otherwise.
*/
Module.isDefined = function(name) {
	return name in Module.modules;
};

/**
* This function throws an error if the named module is not defined
* or if it is defined but its version is less than the specified version.
* If the namespace exists and has a suitable version, this function simply
* returns without doing anything. Use this function to cause a fatal
* error if the modules that your code requires are not present.
*/
Module.require = function(name, version) {
	if (!(name in Module.modules)) {
		throw new Error("Module " + name + " is not defined");
	}
	// If no version was specified, there is nothing to check
	if (!version) return;
	var n = Module.modules[name];
	// If the defined version is less than the required version or if
	// the namespace does not declare any version, throw an error.
	if (!n.VERSION || n.VERSION < version)
		throw new Error("Module " + name + " has version " +
		n.VERSION + " but version " + version +
		" or greater is required.");
};

/**
* This function imports symbols from a specified module. By default, it
* imports them into the global namespace, but you may specify a different
* destination as the second argument.
*
* If no symbols are explicitly specified, the symbols in the EXPORT
* array of the module will be imported. If no such array is defined,
* and no EXPORT_OK is defined, all symbols from the module will be imported.
*
* To import an explicitly specified set of symbols, pass their names as
* arguments after the module and the optional destination namespace. If the
* modules defines an EXPORT or EXPORT_OK array, symbols will be imported
* only if they are listed in one of those arrays.
*/
Module.importSymbols = function(from) {
	// Make sure that the module is correctly specified. We expect the
	// module's namespace object but will try with a string, too
	if (typeof from == "string") from = Module.modules[from];
	if (!from || typeof from != "object")
		throw new Error("Module.importSymbols( ): " + "namespace object required");

	// The source namespace may be followed by an optional destination
	// namespace and the names of one or more symbols to import;
	var to = Module.globalNamespace; // Default destination
	var symbols = []; // No symbols by default
	var firstsymbol = 1; // Index in arguments of first symbol name
	// See if a destination namespace is specified
	if (arguments.length > 1 && typeof arguments[1] == "object") {
		if (arguments[1] != null) to = arguments[1];
		firstsymbol = 2;
	}
	// Now get the list of specified symbols
	for(var a = firstsymbol; a < arguments.length; a++)
		symbols.push(arguments[a]);

	// If we were not passed any symbols to import, import a set defined
	// by the module, or just import all of them.
	if (symbols.length == 0) {
		// If the module defines an EXPORT array, import
		// the symbols in that array.
		if (from.EXPORT) {
			for(var i = 0; i < from.EXPORT.length; i++) {
				var s = from.EXPORT[i];
				to[s] = from[s];
			}
			return;
		}
		// Otherwise if the modules does not define an EXPORT_OK array,
		// just import everything in the module's namespace
		else if (!from.EXPORT_OK) {
			for(s in from) to[s] = from[s];
			return;
		}
	}

	// If we get here, we have an explicitly specified array of symbols
	// to import. If the namespace defines EXPORT and/or EXPORT_OK arrays,
	// ensure that each symbol is listed before importing it.
	// Throw an error if a requested symbol does not exist or if
	// it is not allowed to be exported.
	var allowed;
	if (from.EXPORT || from.EXPORT_OK) {
		allowed = {};
		// Copy allowed symbols from arrays to properties of an object.
		// This allows us to test for an allowed symbol more efficiently.
		if (from.EXPORT)
			for(var i = 0; i < from.EXPORT.length; i++)
				allowed[from.EXPORT[i]] = true;
		if (from.EXPORT_OK)
			for(var i = 0; i < from.EXPORT_OK.length; i++)
				allowed[from.EXPORT_OK[i]] = true;
	}
	// Import the symbols
	for(var i = 0; i < symbols.length; i++) {
		var s = symbols[i]; // The name of the symbol to import
		if (!(s in from)) // Make sure it exists
			throw new Error("Module.importSymbols( ): symbol " + s + " is not defined");
		if (allowed && !(s in allowed)) // Make sure it is a public symbol
			throw new Error("Module.importSymbols( ): symbol " + s + " is not public and cannot be imported.");
		to[s] = from[s]; // Import it
	}
};

// Modules use this function to register one or more initialization functions
Module.registerInitializationFunction = function(f) {
	// Store the function in the array of initialization functions
	Module._initfuncs.push(f);
	// If we have not yet registered an onload event handler, do so now.
	Module._registerEventHandler( );
}

// A function to invoke all registered initialization functions.
// In client-side JavaScript, this will automatically be called in
// when the document finished loading. In other contexts, you must
// call it explicitly.
Module.runInitializationFunctions = function( ) {
	// Run each initialization function, catching and ignoring exceptions
	// so that a failure by one module does not prevent other modules
	// from being initialized.
	for(var i = 0; i < Module._initfuncs.length; i++) {
		try { Module._initfuncs[i]( ); }
		catch(e) { /* ignore exceptions */}
	}
	// Erase the array so the functions are never called more than once.
	Module._initfuncs.length = 0;
}

// A private array holding initialization functions to invoke later
Module._initfuncs = [];

// If we are loaded into a web browser, this private function registers an
// onload event handler to run the initialization functions for all loaded
// modules. It does not allow itself to be called more than once.
Module._registerEventHandler = function( ) {
	var clientside = // Check for well-known client-side properties
	"window" in Module.globalNamespace &&
	"navigator" in window;
	if (clientside) {
		if (window.addEventListener) { // W3C DOM standard event registration
			window.addEventListener("load", Module.runInitializationFunctions, false);
		}
		else if (window.attachEvent) { // IE5+ event registration
			window.attachEvent("onload", Module.runInitializationFunctions);
		}
		else {
			// IE4 and old browsers
			// If the <body> defines an onload tag, this event listener
			// will be overwritten and never get called.
			window.onload = Module.runInitializationFunctions;
		}
	}

	// The function overwrites itself with an empty function so it never
	// gets called more than once.
	Module._registerEventHandler = function( ) {};
}

