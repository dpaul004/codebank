/*	Chad R. Bhatti
	01.02.2015
	eda_03.sas
*/


* Dr. Bhatti's Predict 410 data warehouse;
libname mydata	'/courses/d6fc9ae5ba27fe300/c_3505/SAS_Data/' access=readonly;

* List out the column names and data types for the data set;
proc contents data=mydata.ames_housing_data; run; quit;

* Print out the first 10 observations on the data set;
proc print data=mydata.ames_housing_data(obs=10); run; quit;


********************************************************************;
* Basic data summary output;
********************************************************************;
* Here we will route the output from a SAS procedure to a SAS
* data set so that we can use it as data and not just a summary;

proc freq data=mydata.ames_housing_data;
tables Neighborhood / out=neighborhood_counts;
run; quit;
* Note that here we are using the old school output statement OUT;
* The more modern method of capturing output data sets from SAS
* procedures is to use the SAS ODS System;


data neighborhood_counts; 
set neighborhood_counts;
neighborhood_count = count;
drop percent count;
run;


proc print data=neighborhood_counts; run; quit;


********************************************************************;
* Basic data merge;
********************************************************************;
* Here we will sort a permanent SAS data set and output the result
* into a temporary (or working) SAS data set;
proc sort data=mydata.ames_housing_data out=temp;
by Neighborhood;
run;


proc sort data=neighborhood_counts;
by Neighborhood;
run;


* This is the SAS equivalent to a LEFT JOIN;
* Both data sets must be sorted by the merge key, and the IN option
* must be used to specify the direction of the join;
data temp2;
merge temp (in=a) neighborhood_counts (in=b);
by Neighborhood;
if (a=1);
MyNeighborhood = Neighborhood;
if (neighborhood_count < 100) then MyNeighborhood='Other';
run;


proc print data=temp2(obs=10); run; quit;



********************************************************************;
* How should we look at this data?;
********************************************************************;

data temp3;
set temp2;
PriceSqft = SalePrice / GrLivArea;
run;


proc sgplot data=temp3;
hbox SalePrice / category=MyNeighborhood transparency=0.25;
run; quit;


proc sgplot data=temp3;
hbox PriceSqft / category=MyNeighborhood transparency=0.25;
run; quit;


/*
Modern EDA is more than basic data summaries and data plots. 
We will learn a little more about modern EDA in Predict 410.
The topic is covered much more extensively in the Advanced
Modeling Techniques course.

At a minimum here we want to start thinking about EDA for 
building predictive models, not for performing a data quality
check.

*/


/* FINAL NOTE:
In the RESULTS tab click on the Word icon to download SAS output in
RTF format.  This will allow you to cut and paste the output into
your report.
*/






















