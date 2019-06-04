/*	Chad R. Bhatti
	01.13.2015
	proc_transpose_example.sas
*/

* In this example we will introduce the concepts of a LONG data format for a data set
* and a WIDE data format for a data set;
* We will also illustrate how to manipulate these data formats from one format to the 
* other format using PROC TRANSPOSE;


*********************************************************************;
* Example of a Long Format data set;
*********************************************************************;

data temp;
* Note that using the datalines option is equivalent to reading in a fixed width text file;
* The length of each variable in the input statement must be exact and include the trailing white space;
input name $9. time 2. value1 4. value2 4.;
datalines;
Chad     1 100 56 
Chad     2 105 56
Chad     3 115 67
Chad     4 135 78
Dan      1 90  12
Dan      2 99  12
Dan      3 120 13
Dan      4 150 16
Benjamin 1 26  45
Benjamin 2 26  48
Benjamin 3 31  49
Benjamin 4 35  51
;
run;


proc print data=temp; run; quit;
proc contents data=temp; run; quit;

*******************************************************************************************;
* Use PROC TRANSPOSE to change the data orientation from LONG FORMAT to WIDE FORMAT;
*******************************************************************************************;
* The PROC TRANSPOSE statement has a BY statement in it;
* Any time that we have a BY statement in SAS, SAS will expect the input data set to be
* sorted by the variable in that BY statement;

proc sort data=temp; by name time; run; quit;

proc transpose data=temp out=wide_data prefix=time;
by name;
id time;
var value1;
run; quit;

proc print data=wide_data; run; quit;


* Note that you can only transpose one variable at a time to a pure wide format;
* This should be obvious in retrospect due to the naming conventions and how PREFIX is used;
* Here is what the output looks like when you transpose to variables in a single step;
proc transpose data=temp out=wide_data2 prefix=time;
by name;
id time;
var value1 value2;
run; quit;

proc print data=wide_data2; run; quit;




*******************************************************************************************;
* Use PROC TRANSPOSE to change the data orientation from WIDE FORMAT to LONG FORMAT;
*******************************************************************************************;
* Now let's use PROC TRANSPOSE to convert our WIDE data set back to a LONG data set;

proc transpose data=wide_data out=long_data;
by name;
var time1-time4;
run; quit;

proc print data=long_data; run; quit;

data long_data_pretty;
set long_data;
time = substr(_name_,5);
run;

proc print data=long_data_pretty; run; quit;


* Note that here you cannot really even try to do 'two' variables at a time since each variable
* is represented by a series of tagged variables;



*******************************************************************************************;
*******************************************************************************************;
