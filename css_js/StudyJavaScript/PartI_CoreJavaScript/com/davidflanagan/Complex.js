// Example 10-2. A complex-number class as a module
/**
* com/davidflanagan/Complex.js: a class representing complex numbers
*
* This module defines the constructor function com.davidflanagan.Complex( )
* This module requires the com/davidflanagan/Class.js module
**/
// First, check for the Class module
var com; // Declare global symbol before testing for its presence
if (!com || !com.davidflanagan || !com.davidflanagan.Class)
	throw new Error("com/davidflanagan/Class.js has not been loaded");
// We know from this test that the com.davidflanagan namespace
// exists, so we don't have to create it here. We'll just define
// our Complex class within it
com.davidflanagan.Complex = com.davidflanagan.Class.define({
	name: "Complex",
	construct: function(x,y) { this.x = x; this.y = y; },
	methods: {
		add: function(c) {
			return new com.davidflanagan.Complex(this.x + c.x,
			this.y + c.y);
		}
	},
});

