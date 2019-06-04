/*	Chad R. Bhatti
	01.11.2015
	naive_reg_model.sas
*/


* Dr. Bhatti's Predict 410 data warehouse;
libname mydata	'/courses/d6fc9ae5ba27fe300/c_3505/SAS_Data/' access=readonly;

proc contents data=mydata.ames_housing_data; run; quit;
******************************************************************************;

* Use DataDocumentation.txt to create predictor variables for your regression model;
* Note that the names have been streamlined on the data set;
* It would be smart to have a print out of the proc contents of the data set when reading the 
* data documentation;


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
	* What are we trying to do by applying these conditions?;

	* This data set has a lot of different types of attribute information in it;
	* How can we use this attribute information in a regression model?;

	log_price = log(SalePrice);

	total_baths = max(FullBath,0) + max(BsmtFullBath,0);
	total_halfbaths = max(HalfBath,0) + max(BsmtHalfBath,0);
	total_baths_calc = total_baths + total_halfbaths;

	* Central Air Indicator;
	if (CentralAir='Y') then central_air=1; else central_air=0;
	* Fireplace Indicator;
	if (Fireplaces>0) then fireplace_ind=1; else fireplace_ind=0;
	* Garage Indicator;
	if (GarageCars>0) then garage_ind=1; else garage_ind=0;
	* Good Basement Indicator;
	if (BsmtQual in ('Ex','Gd')) or (BsmtCond in ('Ex','Gd')) then good_basement_ind=1; else good_basement_ind=0;

	* Exterior Material Quality - Family of Indicator Variables;
	if (ExterQual='Ex') then ExterQual_Ex=1; else ExterQual_Ex=0;
	if (ExterQual='Gd') then ExterQual_Gd=1; else ExterQual_Gd=0;
	if (ExterQual='TA') then ExterQual_TA=1; else ExterQual_TA=0;
	if (ExterQual='Fa') then ExterQual_Fa=1; else ExterQual_Fa=0;
	if (ExterQual='Po') then ExterQual_Po=1; else ExterQual_Po=0;

	* Brick Exterior;
	if (Exterior1 in ('BrkComm','BrkFace')) or (Exterior2 in ('BrkComm','BrkFace')) 
	then brick_exterior=1; else brick_exterior=0;

	* Tile Roof;
	if (RoofMat='ClyTile') then tile_roof=1; else tile_roof=0;

	* Lot Shape;
	if (LotShape in ('Reg','IR1')) then regular_lot=1; else regular_lot=0;

	* Lot Configuration;
	if (LotConfig='Inside') then lot_inside=1; else lot_inside=0;
	if (LotConfig='Corner') then lot_corner=1; else lot_corner=0;
	if (LotConfig='CulDSac') then lot_culdsac=1; else lot_culdsac=0;
	if (LotConfig in ('FR2','FR3')) then lot_frontage=1; else lot_frontage=0;

	* Construct a composite quality index;
	quality_index = OverallCond*OverallQual;

run;

* How many observations did we shed with our IF conditions?;

* Other continuous variables of interest include: TotRmsAbvGrd, GrLivArea, LotArea, LotFrontage;


proc reg data=temp;
model SalePrice = total_baths_calc TotRmsAbvGrd GrLivArea LotArea LotFrontage
	central_air fireplace_ind garage_ind good_basement_ind 
	ExterQual_Fa ExterQual_TA ExterQual_Gd ExterQual_Ex 
	quality_index regular_lot tile_roof brick_exterior 
	lot_inside lot_corner lot_culdsac lot_frontage
	;
run; quit;

* What happened here?  What to do?;
* One - we did not properly include the family of indicator variables for lot type;
* lot_fontgage lot_corner lot_culdsac lot_frontage - need to remove one of these indicators;

proc freq data=temp;
tables tile_roof ExterQual;
run;


* Model #1;
proc reg data=temp;
model SalePrice = total_baths_calc TotRmsAbvGrd GrLivArea LotArea 
	central_air fireplace_ind garage_ind good_basement_ind 
	ExterQual_Fa ExterQual_TA ExterQual_Gd 
	quality_index regular_lot brick_exterior 
	lot_corner lot_culdsac lot_frontage
	;
run; quit;


* How do we interpret this model?;
* How do we interpret the indicator variables?;
* Are there predictor variables in this model that do not make sense?;
* Do they signal that there might be a problem?;
* Is this a 'good' regression model?;
* What does 'good' mean?;
* Can this regression model be 'good' in some respects and 'not good' in others?;



* Note on using the family of indicator variables to represent a categorical variable;
* How the baseline category is defined affects the estimates;
* What is the baseline category for external quality below?  How about above?;

* Model #2;
proc reg data=temp;
model SalePrice = total_baths_calc TotRmsAbvGrd GrLivArea LotArea 
	central_air fireplace_ind garage_ind good_basement_ind 
	ExterQual_TA ExterQual_Gd ExterQual_Ex 
	quality_index regular_lot brick_exterior 
	lot_corner lot_culdsac lot_frontage
	;
run; quit;

* Compare Model #1 and Model #2;
* Do we like this model better?  Are all of these predictor variables 'important'?;



