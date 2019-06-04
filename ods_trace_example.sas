*/	Chad R. Bhatti
	04.19.2015
	ods_trace_example.sas
*/


*libname mydata	'/courses/d6fc9ae5ba27fe300/c_3505/SAS_Data/' access=readonly;
libname mydata 	'/scs/crb519/PREDICT_410/SAS_Data/' access=readonly;


/*
Regression Analysis By Example, 5th Edition ISBN 9780470905845 - p. 328-329
http://www1.aucegypt.edu/faculty/hadi/RABE5/#Download

Property Valuation:  The objective of scientific mass appraisal 
(or Automated Valuation Models) is to predict the sale price of a 
home based on selected physical attributes of the property. 

Y:  sale price of house in thousands of dollars
X1: taxes in thousands of dollars
X2: number of bathrooms
X3: lot size in thousands of square feet
X4: living space in thousands of square feet
X5: number of garage stalls
X6: number of rooms
X7: number of bedrooms
X8: age of home in years
X9: number of fireplaces 

*/


/*
proc contents data=mydata.building_prices; run; quit;
proc print data=mydata.building_prices(obs=5); run; quit;
*/


/*	Use ODS TRACE to have SAS list out the output data sets that are available
	to be captured by ODS OUTPUT.  You will find this list in your LOG file, not
	the RESULTS page.
*/

************************************************************************;
* PROC FREQ EXAMPLE;
************************************************************************;
ods trace on;
proc freq data=mydata.building_prices;
tables X2;
run; quit;
ods trace off;

* Look in your LOG and find the data set name OneWayFreqs;
* Here is how we capture the output;
ods output  OneWayFreqs=bathroom_freq;
proc freq data=mydata.building_prices;
tables X2;
run; quit;
ods output close;

proc print data=bathroom_freq; run; quit;




************************************************************************;
* PROC MEANS EXAMPLE;
************************************************************************;
ods trace on;
proc means data=mydata.building_prices;
var X2;
run; quit;
ods trace off;
* Look in the LOG file and see what output data sets SAS produces for PROC MEANS;


* Capture the ODS OUTPUT data set;
ods output Summary=bathroom_summary;
proc means data=mydata.building_prices;
var X2;
run; quit;
ods output close;

proc print data=bathroom_summary; run; quit;



************************************************************************;
* PROC REG EXAMPLE;
************************************************************************;
ods trace on;
proc reg data=mydata.building_prices; 
model Y = X2 X4 X6;
run; quit;
ods trace off;
* Look in the LOG file and see what output data sets SAS produces for PROC MEANS;


* Capture the ODS OUTPUT data set;
ods output ANOVA=anova_table
	FitStatistics=gof_output
	ParameterEstimates=parameter_estimates;
proc reg data=mydata.building_prices; 
model Y = X2 X4 X6;
run; quit;
ods output close;

title Captured ANOVA Table Output;
proc print data=anova_table; run; quit;

title Captured GOF Output;
proc print data=gof_output; run; quit;

title Captured Parameter Estimates;
proc print data=parameter_estimates; run; quit;




************************************************************************;
* MORE COMPLICATED PROC REG EXAMPLE;
************************************************************************;
ods trace on;
proc reg data=mydata.building_prices; 
model Y = X2 X4 X6 / selection=rsquare start=3 stop=3 aic bic cp ;
run; quit;
ods trace off;
* Look in the LOG file and see what output data sets SAS produces for PROC MEANS;


* Capture the ODS OUTPUT data set;
ods output SubsetSelSummary=selection_summary;
proc reg data=mydata.building_prices; 
model Y = X2 X4 X6 / selection=rsquare start=3 stop=3 aic bic cp ;
run; quit;
ods output close;


title Captured Variable Selection Output;
proc print data=selection_summary; run; quit;



************************************************************************;
* OUTPUTTING FITTED VALUES AND RESIDUAL WITH PROC REG;
************************************************************************;
proc reg data=mydata.building_prices; 
model Y = X2 X4 X6 ;
output out=residuals_plus_fitted_values r=model_residual pred=model_prediction;
run; quit;

proc print data=residuals_plus_fitted_values; run; quit;
























