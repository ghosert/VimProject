/**
* BarChart.js:
* This file defines makeBarChart( ), a function that creates a bar chart to
* display the numbers from the data[] array. The overall size of the chart
* is specified by the optional width and height arguments, which include the
* space required for the chart borders and internal padding. The optional
* barcolor argument specifies the color of the bars. The function returns the
* <div> element it creates, so the caller can further manipulate it by
* setting a margin size, for example. The caller must insert the returned
* element into the document in order to make the chart visible.
**/
function makeBarChart(data, width, height, barcolor) {
	// Provide default values for the optional arguments
	if (!width) width = 500;
	if (!height) height = 350;
	if (!barcolor) barcolor = "blue";
	// The width and height arguments specify the overall size of the
	// generated chart. We have to subtract the border and padding
	// sizes from this to get the size of the element we create.
	width -= 24; // Subtract 10px padding and 2px border left and right
	height -= 14; // Subtract 10px top padding and 2px top and bottom border
	// Now create an element to hold the chart. Note that we make the chart
	// relatively positioned so that it can have absolutely positioned children
	// but still appear in the normal element flow.
	var chart = document.createElement("div");
	chart.style.position = "relative"; // Set relative positioning
	chart.style.width = width + "px"; // Set the chart width
	chart.style.height = height + "px"; // Set the chart height
	chart.style.border = "solid black 2px"; // Give it a border
	chart.style.paddingLeft = "10px"; // Add padding on the left
	chart.style.paddingRight = "10px"; // and on the right
	chart.style.paddingTop = "10px"; // and on the top
	chart.style.paddingBottom = "0px"; // but not on the bottom
	chart.style.backgroundColor = "white"; // Make chart background white
	// Compute the width of each bar
	var barwidth = Math.floor(width/data.length);
	// Find largest number in data[]. Note clever use of Function.apply( ).
	var maxdata = Math.max.apply(this, data);
	// The scaling factor for the chart: scale*data[i] gives height of a bar
	var scale = height/maxdata;
	// Now loop through the data array and create a bar for each datum
	for(var i = 0; i < data.length; i++) {
		var bar = document.createElement("div"); // Create div for bar
		var barheight = data[i] * scale; // Compute height of bar
		bar.style.position = "absolute"; // Set bar position and size
		bar.style.left = (barwidth*i+1+10)+"px"; // Add bar border and chart pad
		bar.style.top = height-barheight+10+"px";// Add chart padding
		bar.style.width = (barwidth-2) + "px"; // -2 for the bar border
		bar.style.height = (barheight-1) + "px"; // -1 for the bar top border
		bar.style.border = "solid black 1px"; // Bar border style
		bar.style.backgroundColor = barcolor; // Bar color
		bar.style.fontSize = "0px"; // IE workaround
		chart.appendChild(bar); // Add bar to chart
	}
	// Finally, return the chart element so the caller can manipulate it
	return chart;
}

