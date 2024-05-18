jQuery(document).ready(function() {
	
	// FIND ME: USING SELECTORS AND EVENTS
	$(document).ready(function()
	{
        $("#orderedlist").addClass("red"); // add class to element.
    });
	
	$(document).ready(function()
	{
        $("#orderedlist li").addClass("blue"); // add class to child element.
    });

    // add and remove the class when the user hovers the li element, but only on the last element in the list.
    $("#orderedlist li:last").hover(function() // $("#orderedlist li:last") equals to $("#orderedlist li").eq(lastIndex)
	{
        $(this).addClass("green"); // add class when hover in.
    },function()
	{
        $(this).removeClass("green"); // remove class when hover out.
    });
	
	// each() iterates over every element and allows further processing.
    $("#orderedlist").find("li").each(function(i) { // equals to $("#orderedlist li").each(...
        $(this).append( " BAM! " + i ); // append here means $(this).text($(this).text() + " BAM! " + i);
    });
	
	// The code above is same to the code snip below:
    var els = $("#orderedlist li");
	for (var i = 0; i < els.length; i++) {
		els.eq(i).append(" bam! " + i);
	}
	
	// use this to reset several forms at once
    $("#reset").click(function()
	{
        $("form").each(function()
		{
            this.reset(); // note: reset() is a js form function not jQuery's, so don't use $(this).reset()
        });
    });
	
	// all li elements get a border, except the one that has a child ul.
	$("li").not(":has(ul)").css("border", "1px solid black"); // This is just a sample I think we should do this in css.
	
	// This adds a background color to all anchor elements with a name attribute.
	$("a[name]").css("background", "#eee" );
	
	// To match only a part of the value, we can use the contains select "*=" instead of an equals ("=")
	$("a[href*=/content/gallery]").click(function() {
        // do something with all links that point somewhere to /content/gallery
    });
	
	// Here we use some chaining to reduce the code size and gain better performance, as '#faq' is only selected once.
	// By using end(), the first find() is undone, so we can start search with the next find() at our #faq element, instead of the dd children.
    $('#faq').find('dd').hide().end().find('dt').click(function() {
        $(this).next().slideToggle(); // $(this).next() to find the next sibling starting from the current dt.
    });
	
	// You can also select parent elements:
    $("a").hover(function()
	{
        $(this).parents("p").addClass("highlight");
    },function()
	{
        $(this).parents("p").removeClass("highlight");
    });
	
	// shortcut for the $(document).ready(callback);
	$(function()
	{
		// code to execute when the DOM is ready
	});
	
	
	// RATE ME: USING AJAX
	// generate markup
    $("#rating").append("Please rate: ");
       
    for ( var i = 1; i <= 5; i++ )
        $("#rating").append("<a href='#'>" + i + "</a> ");
       
    // add markup to container and apply click handlers to anchors
    $("#rating a").click(function(e)
	{
        // stop normal link click
        e.preventDefault();
         
        // send request
        $.post("rate.php", {rating: $(this).html()}, function(xml)
		{
            xml = "<ratings><average>5</average><count>100</count></ratings>"; // mock the xml response.
			
            // format and output result
            $("#rating").html(
                "Thanks for rating, current average: " +
                $("average", xml).text() +
                ", number of votes: " +
                $("count", xml).text()
            );
        });
    });
	
	
	// ANIMATE ME: USING EFFECTS
	// click on "Some link" link on the page to see the effect.
	/*
    $("a").toggle(function()
	{
        $(".stuff").hide('slow');
    },function()
	{
        $(".stuff").show('fast');
    });
    */
	// create any combination of animations with animate(), eg. a slide with a fade:
    $("a").toggle(function()
	{
        $(".stuff").animate({ height: 'hide', opacity: 'hide' }, 'slow');
    },function()
	{
        $(".stuff").animate({ height: 'show', opacity: 'show' }, 'slow');
    });
	// Much fancier effects can be achieved with the interface plugin collection: http://interface.eyecon.ro/ (demos and documentation)
	
	
	// SORT ME: USING THE TABLESORTER PLUGIN
	// 1. download the tablesorter plugin first.
	// 2. add <script src="jquery.tablesorter.js"></script> to starterkit.html.
	// 3. click on the headers of the table and see how it is sorted. Have a try on click with the "shift" key pressed.
	
    $("#large").tablesorter();
	
	/**
	 * Uncomment this code snip to see the effect, can be done by css as well.
	 * 
    $("#large").tablesorter({
        // striping looking
        widgets: ['zebra']	
    });
    **/
	
	// And check out more examples on tablesorter homepage.
	// And you can check out more plugins on jQuery plugin site.
	
	
	// PLUG ME: WRITE YOUR OWN PLUGINS
	// Plugin Naming
    // Find a name for your plugin, lets call our example "foobar". Create a file named jquery.[yourpluginname].js, eg. jquery.foobar.js
	// Adding a Custom Method
	// Create one or more plugin methods by extending the jQuery object, eg.:
    jQuery.fn.foobar = function() {
		alert('hello my plugin.');
		alert(this); // this here means passed in selected jQuery element. Refer to the clause below, it's $("#large").
    };

    // Which will then be accessible by performing: $("...").foobar();
    $("#large").foobar();

    // Default Settings:
	// Create default settings that can be changed by the user, eg.:
    jQuery.fn.foobar2 = function(options) {
        var settings = jQuery.extend({
            value: 5, name: "pete", bar: 655
        }, options);
		alert(settings.value);
    };

    // You can then call the plugin without options, using the defaults: $("...").foobar();
    $("#large").foobar2();

    // Or with some options: $("...").foobar({ value: 123, bar: 9 });
	$("#large").foobar2({ value: 123, bar: 9 });
	
	// Conclustion: See more details on "Tutorials: Getting Started with jQuery" on jQuery official documentation.
	
	
	// NEXT STEPS
	// Develop JavaScript by using FireFox FireBug, it provides a console, a debugger and other useful stuff for the daily JavaScript development.
	
});
