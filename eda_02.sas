/*	Chad R. Bhatti
	12.29.2014
	eda_02.sas
*/


* Dr. Bhatti's Predict 410 data warehouse;
libname mydata	'/courses/d6fc9ae5ba27fe300/c_3505/SAS_Data/' access=readonly;

* List out the column names and data types for the data set;
proc contents data=mydata.ames_housing_data; run; quit;

* Print out the first 10 observations on the data set;
proc print data=mydata.ames_housing_data(obs=10); run; quit;


********************************************************************;
* Basic statistical plots;
********************************************************************;
* The workhorse plotting procedure is PROC SGPLOT;
* PROC SGPLOT is used to make all types of plots -
*	bar charts, histograms, boxplots, scatterplots;
* Read about PROC SGPLOT in The Little SAS Book;



********************************************************************;
* Bar Chart;
********************************************************************;

* Produce bar chart of LotShape by Neighborhood;
proc sgplot data=mydata.ames_housing_data;
vbar Neighborhood / group=LotShape groupdisplay=cluster;
run;quit;

* Compare to the cluster display to the default display 
	(groupdisplay=stack);
proc sgplot data=mydata.ames_housing_data;
vbar Neighborhood / group=LotShape;
run;quit;

* Switch vbar and group variables;
proc sgplot data=mydata.ames_housing_data;
vbar LotShape / group=Neighborhood groupdisplay=cluster;
run;quit;


********************************************************************;
* Histogram;
********************************************************************;

proc sgplot data=mydata.ames_housing_data;
histogram SalePrice / transparency=0.5;
density SalePrice / type=normal;
density SalePrice / type=kernel;
run; quit;


* What about a transformation of SalePrice?;
data temp;
set mydata.ames_housing_data;
logSalePrice = log(SalePrice);
run;


proc sgplot data=temp;
histogram logSalePrice / transparency=0.5;
density logSalePrice / type=normal;
density logSalePrice / type=kernel;
run; quit;



********************************************************************;
* Box Plots;
********************************************************************;

proc sgplot data=mydata.ames_housing_data;
vbox SalePrice / category=neighborhood;
run; quit;

* What do the whiskers measure in a SAS boxplot?;
* Answer is in The Little SAS Book;

proc sgplot data=mydata.ames_housing_data;
hbox SalePrice / category=neighborhood transparency=0.75; *Try 0.75 and 0.25;
run; quit;



********************************************************************;
* Scatter Plots;
********************************************************************;

proc sgplot data=mydata.ames_housing_data;
scatter x=GrLivArea y=SalePrice;
run; quit;



********************************************************************;
* Smoother Plots;
********************************************************************;

proc sgplot data=mydata.ames_housing_data;
reg x=GrLivArea y=SalePrice / clm;
loess x=GrLivArea y=SalePrice / nomarkers;
run; quit;


********************************************************************;
* Panel Plots;
********************************************************************;

proc freq data=mydata.ames_housing_data;
tables LotShape;
run; quit;


proc sgpanel data=mydata.ames_housing_data;
panelby LotShape / rows=1 columns=4;
scatter x=GrLivArea y=SalePrice;
title 'Sale Price by Living Area';
run; quit;

* You make a paneled histogram;
* Note that you can make a scatterplot matrix using PROC CORR;




/*
In these examples I have shown the basic SAS syntax for a variety of 
graphics options available in PROC SGPLOT.  Read more in your copy of
The Little SAS Book and learn how to control and fine tune the graphical 
output from PROC SGPLOT.
*/





/* FINAL NOTE:
In the RESULTS tab click on the Word icon to download SAS output in
RTF format.  This will allow you to cut and paste the output into
your report.
*/



























