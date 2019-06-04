/*	Chad R. Bhatti
	01.19.2015
	proc_format_example.sas
*/


* Dr. Bhatti's Predict 410 data warehouse;
libname mydata	'/courses/d6fc9ae5ba27fe300/c_3505/SAS_Data/' access=readonly;

proc contents data=mydata.ames_housing_data; run; quit;
******************************************************************************;

* Use DataDocumentation.txt to create predictor variables for your regression model;
* Note that the names have been streamlined on the data set;
* It would be smart to have a print out of the proc contents of the data set when reading the 
* data documentation;

******************************************************************************;

* Why are we interested in learning about PROC FORMAT?;
* Using custom formats defined by PROC FORMAT is extremely useful in data management, 
* reporting, general table making using PROC FREQ, and easy categorization for EDA 
* or model validation;

******************************************************************************;

* Let's start by defining some naive data formats;
* Read more about SAS formats and PROC FORMAT in The Little SAS Book;
	
************************************************************;
* Naive Data Formats;
************************************************************;
proc format;
value price_fmt 
	. = 'Missing'			
	1 -< 100000 = '[1; 100,000)'
	100000 -< 150000 = '[100,000; 150,000)'
	150000 -< 200000 = '[150,000; 200,000)'
	200000 -< 250000 = '[200,000; 250,000)'
	250000 -< 300000 = '[250,000; 300,000)'
	300000 -< 350000 = '[300,000; 350,000)'
	350000 -< 400000 = '[350,000; 400,000)'
	400000 - high = '[400,000+]'
	other = 'Invalid Value'
	; * use a semi-colon to end each format in the proc format statement;
	* Note on how we use the < to create open intervals.  
	* The dash - will create closed intervals, and
	hence the dash should only be used with discrete values;
value sqft_fmt 
	. = 'Missing'			
	1 - 1000 = '[1; 1,000]'
	1000 <- 1500 = '(1,000; 1,500]'
	1500 <- 2000 = '(1,500; 2,000]'
	2000 <- 2500 = '(2,000; 2,500]'
	2500 <- 3000 = '(2,500; 3,000]'
	3000 - high = '(3,000+]'
	other = 'Invalid Value'
	;	
run;


******************************************************************************;
* Now let's apply our naive SAS formats;
* Use the put() function to apply any format in SAS;

data temp;
	set mydata.ames_housing_data;

	* Subset data using IF statements;
	* There are more efficient ways to do this, but this is the most basic way to 
		implement multiple conditions in SAS;
	if (SaleCondition = 'Normal');
 	if (BldgType = '1Fam');
	if (Zoning in ('RH','RL','RP','RM'));
	if (Street='Pave');
	if (Utilities='AllPub');

	* Use the put() function to create a new variable by assigning the format to 
	an existing variable;
	price_cat = put(SalePrice,price_fmt.);
	sqft_cat = put(GrLivArea,sqft_fmt.);

run;


* Create frequency tables for each category;
proc freq data=temp;
tables price_cat sqft_cat;
run; quit;


* Is this what we want?  What is the problem here?;

******************************************************************************;
******************************************************************************;

* Now let's define some smarter formats;
************************************************************;
* Smart Data Formats;
************************************************************;
proc format;
value price_sfmt 
	. = '10: Missing'			
	1 -< 100000 = '01: [1; 100,000)'
	100000 -< 150000 = '02: [100,000; 150,000)'
	150000 -< 200000 = '03: [150,000; 200,000)'
	200000 -< 250000 = '04: [200,000; 250,000)'
	250000 -< 300000 = '05: [250,000; 300,000)'
	300000 -< 350000 = '06: [300,000; 350,000)'
	350000 -< 400000 = '07: [350,000; 400,000)'
	400000 - high = '08: [400,000+]'
	other = '09: Invalid Value'
	; * use a semi-colon to end each format in the proc format statement;
	* Note on how we use the < to create open intervals.  
	* The dash - will create closed intervals, and
	hence the dash should only be used with discrete values;
value sqft_sfmt 
	. = '08: Missing'			
	1 - 1000 = '01: [1; 1,000]'
	1000 <- 1500 = '02: (1,000; 1,500]'
	1500 <- 2000 = '03: (1,500; 2,000]'
	2000 <- 2500 = '04: (2,000; 2,500]'
	2500 <- 3000 = '05: (2,500; 3,000]'
	3000 - high = '06: (3,000+]'
	other = '07: Invalid Value'
	;	
run;


******************************************************************************;

data temp;
	set mydata.ames_housing_data;

	* Subset data using IF statements;
	* There are more efficient ways to do this, but this is the most basic way to 
		implement multiple conditions in SAS;
	if (SaleCondition = 'Normal');
 	if (BldgType = '1Fam');
	if (Zoning in ('RH','RL','RP','RM'));
	if (Street='Pave');
	if (Utilities='AllPub');

	* Use the put() function to create a new variable by assigning the format 
	to an existing variable;
	price_cat = put(SalePrice,price_sfmt.);
	sqft_cat = put(GrLivArea,sqft_sfmt.);

run;


* Create frequency tables for each category;
proc freq data=temp;
tables price_cat sqft_cat;
run; quit;




******************************************************************************;
* Test the full range of format values;
******************************************************************************;
* Sometimes we need to create some 'mock' data in order to test our SAS programs;
* Here we will overwrite some data values in order to check that our formats
* are properly defined for their full definitions;

data temp;
	set mydata.ames_housing_data;

	* Subset data using IF statements;
	* There are more efficient ways to do this, but this is the most basic way to 
		implement multiple conditions in SAS;
	if (SaleCondition = 'Normal');
 	if (BldgType = '1Fam');
	if (Zoning in ('RH','RL','RP','RM'));
	if (Street='Pave');
	if (Utilities='AllPub');

	* Generate a uniform random number;
	u = uniform(123);
	* Use the random number to select observations to set to missing and 
	negative values;
	mySalePrice = SalePrice;
	if (u < 0.02) then mySalePrice=.;
	if (u > 0.95) then mySalePrice=-1*SalePrice;

	myGrLivArea = GrLivArea;
	if (u < 0.02) then myGrLivArea=.;
	if (u > 0.95) then myGrLivArea=-1*myGrLivArea;

	* Use the put() function to create a new variable by assigning the format 
	to an existing variable;
	price_cat = put(mySalePrice,price_sfmt.);
	sqft_cat = put(myGrLivArea,sqft_sfmt.);

run;


* Create frequency tables for each category;
proc freq data=temp;
tables price_cat sqft_cat;
run; quit;


* Create a two-way frequency table;
proc freq data=temp;
tables price_cat*sqft_cat;
run; quit;


* Use tables options to suppress the output percentages;
proc freq data=temp ;
tables sqft_cat*price_cat / nocol nocum nopercent norow;
run; quit;
* Verify the difference by comparing the output with and without the options;











