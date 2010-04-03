/******************************************************* 
TIMEDATE 
All code by Ryan Parman, unless otherwise noted. 
(c) 1997-2003, Ryan Parman 
http://www.skyzyx.com 
Distributed according to SkyGPL 2.1, http://www.skyzyx.com/license/ 
*******************************************************/ 

/******************************************************* 
CLOCK FUNCTION 
Version 2.12 
*******************************************************/ 
// 1 to Start with dots. 0 to start without dots. 
// Set to "1" for incompatible browsers 
var flashingdots = 1; 

function zClock() 
{ 
    // Comment out for flashing dots 
    //var flashingdots=1; 

    // RUNTIME INITIALIZATION 
    var ampm = 0; 
    var now = new Date(); 
    var hours = now.getHours(); 
    var minutes = now.getMinutes(); 
    var seconds = now.getSeconds(); 
    var ztime; 

    // SET AM OR PM FOR 12-HOUR CLOCK CYCLE 
    if (hours < 12) ampm="am"; 
    else if (hours > 11) ampm="pm"; 

    // SWITCH FROM A 24-HOUR TO A 12-HOUR CLOCK CYCLE 
    if (hours==0) hours=hours+12; // If Midnight is coming up as Zero O'Clock, then fix it. 
    else if (hours > 12) hours=hours-12; // If 1:00pm is coming up as 13 O'Clock, then fix it. 

    // MINUTE FIX 
    if (minutes < 10) minutes="0"+minutes; // If 3:05 is showing up as 3:5, then fix it. 

    // SECOND FIX 
    if (seconds < 10) seconds="0"+seconds; // If 7:25:02 is showing up as 7:25:2, then fix it. 

    // FLASHING DOTS DISPLAY 
    if (flashingdots == 1) { ztime=hours+':'+minutes+ampm; flashingdots=0; } 
    else if (flashingdots == 0) { ztime=hours+'&nbsp;'+minutes+ampm; flashingdots=1; } 
    else { flashingdots=1; } 

    document.getElementById("zClockID").innerHTML=ztime; 

    setTimeout("zClock();", 500); 
} 




/******************************************************* 
DATE FUNCTION 
Version 3.11 
******************************************************** 
IF "disp" IS: 

Lower Case variables... 
10 = Day, Month, Date, Year  [Wednesday, January 15, 2003] 
11 = Month, Date, Year  [January 15, 2003] 
12 = Month, Date  [January 15] 
13 = Day  [Wednesday] 
14 = Month  [January] 
15 = Day, Month, Date, Year  [Wed, Jan 15, 2003] 
16 = Month, Date, Year  [Jan 15, 2003] 
17 = Month, Date  [Jan 15] 
18 = Day  [Wed] 
19 = Month  [Jan] 

Upper Case variables... 
20 = Day, Month, Date, Year  [WEDNESDAY, JANUARY 15, 2003] 
21 = Month, Date, Year  [JANUARY 15, 2003] 
22 = Month, Date  [JANUARY 15] 
23 = Day  [WEDNESDAY] 
24 = Month  [JANUARY] 
25 = Day, Month, Date, Year  [WED, JAN 15, 2003] 
26 = Month, Date, Year  [JAN 15, 2003] 
27 = Month, Date  [JAN 15] 
28 = Day  [WED] 
29 = Month  [JAN] 

Date In Number Format... 
30 = Month, Date, Year (4-Digit)  [1/15/2003] 
31 = Date, Month, Year (4-Digit)  [15/1/2003] 
32 = Month, Date [1/15] 
33 = Date, Month [15/1] 

Numeric-only variables... 
5 = Date  [1] 
6 = Year  [2003] 

Do Not Display 
0 = [No Display] 

************************************************************************** 
These variations are useful when you want to display things differently. 
For example: Displaying "Sunday" in a different font or color than "JANUARY 1, 2000", you 
can call xDate(13) and then xDate(21), which will display the Day (in lowercase format) 
separate from the rest of the date information (where "January" is uppercase). 
Function returns a value to a variable. 
**************************************************************************/ 
function xDate(disp) 
{ 
    // RUNTIME INITIALIZATION 
    rightNow = new Date(); 
    year = rightNow.getFullYear(); 
    date = rightNow.getDate(); 
    thisDay = rightNow.getDay(); 
    thisMonth = rightNow.getMonth(); 
    allDaysLong=new Array('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'); 
    allDaysShort=new Array('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'); 
    allMonthsLong=new Array('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'); 
    allMonthsShort=new Array('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'); 
    var todayis; 
     
    // DATE IN NUMBER FORMAT 
    if (disp==30||disp==31||disp==32||disp==33) thisMonth++; 

    // DATE IN WORD FORMAT (LOWERCASE BY DEFAULT) 
    else 
    { 
        // UPPERCASE NAMES 
        if (parseInt(disp) > 19 && parseInt(disp) < 30) 
        { 
            allMonthsLong[thisMonth]=allMonthsLong[thisMonth].toUpperCase(); 
            allDaysLong[thisDay]=allDaysLong[thisDay].toUpperCase(); 
            allMonthsShort[thisMonth]=allMonthsShort[thisMonth].toUpperCase(); 
            allDaysShort[thisDay]=allDaysShort[thisDay].toUpperCase(); 
        } 
    } 

    // WHAT AND HOW TO DISPLAY INFORMATION 
    if (disp == '0') todayis='&nbsp;'; 
    else if (disp == '10') todayis=allDaysLong[thisDay]+', '+allMonthsLong[thisMonth]+' '+date+', '+year; 
    else if (disp == '11') todayis=allMonthsLong[thisMonth]+' '+date+', '+year; 
    else if (disp == '12') todayis=allMonthsLong[thisMonth]+' '+date; 
    else if (disp == '13') todayis=allDaysLong[thisDay]; 
    else if (disp == '14') todayis=allMonthsLong[thisMonth]; 
    else if (disp == '15') todayis=allDaysShort[thisDay]+', '+allMonthsShort[thisMonth]+' '+date+', '+year; 
    else if (disp == '16') todayis=allMonthsShort[thisMonth]+' '+date+', '+year; 
    else if (disp == '17') todayis=allMonthsShort[thisMonth]+' '+date; 
    else if (disp == '18') todayis=allDaysShort[thisDay]; 
    else if (disp == '19') todayis=allMonthsShort[thisMonth]; 
    else if (disp == '20') todayis=allDaysLong[thisDay]+', '+allMonthsLong[thisMonth]+' '+date+', '+year; 
    else if (disp == '21') todayis=allMonthsLong[thisMonth]+' '+date+', '+year; 
    else if (disp == '22') todayis=allMonthsLong[thisMonth]+' '+date; 
    else if (disp == '23') todayis=allDaysLong[thisDay]; 
    else if (disp == '24') todayis=allMonthsLong[thisMonth]; 
    else if (disp == '25') todayis=allDaysShort[thisDay]+', '+allMonthsShort[thisMonth]+' '+date+', '+year; 
    else if (disp == '26') todayis=allMonthsShort[thisMonth]+' '+date+', '+year; 
    else if (disp == '27') todayis=allMonthsShort[thisMonth]+' '+date; 
    else if (disp == '28') todayis=allDaysShort[thisDay]; 
    else if (disp == '29') todayis=allMonthsShort[thisMonth]; 
    else if (disp == '30') todayis=thisMonth+'/'+date+'/'+year; 
    else if (disp == '31') todayis=date+'/'+thisMonth+'/'+year; 
    else if (disp == '32') todayis=thisMonth+'/'+date; 
    else if (disp == '33') todayis=date+'/'+thisMonth; 
    else if (disp == '5') todayis=date; 
    else if (disp == '6') todayis=year; 
    else todayis='Error: xDate(?);'; 

    return todayis; 
} 
