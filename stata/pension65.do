clear

global root "C:/Users/bjwil/OneDrive/Desktop/Mini-Project"

* ================================================================================= *
*   Author: Brandon Williams
*	Date: 10/4/2023
*	Mini-Project: GMM Regressions
*	
* ================================================================================= *

*-- Log file
capture log close
log using "$root/stata/results.log", replace

*-- Open data
use "$root/stata/pension65_stata.dta", replace


gen water1 = (drnk_wtr_src == 13 | drnk_wtr_src == 22 | drnk_wtr_src == 41 | drnk_wtr_src == 42 | drnk_wtr_src == 43 | drnk_wtr_src == 51)
gen water2 = (drnk_wtr_src == 11 | drnk_wtr_src == 12 | drnk_wtr_src == 21 | drnk_wtr_src == 71)

* ======================================= *
* >>>>>>>>>>> Generate variables <<<<<<<<<
* ======================================= *

* -- local of interest
local instlist9 "floor1 floor2 wall1 wall2 wall3 water1 water2 rural residence edu"
local depvarlist "employed poor_mental_health medical_care"

* -- post and pre treatment observations
gen postyears = 1 if  year>=2012
gen preyears = 1 if year<=2011

* -- interaction for test
	gen i2014 = year==2014 if year>=2014 & year<=2017 
	gen i2015 = year==2015 if year>=2014 & year<=2017 
	gen i2016 = year==2016 if year>=2014 & year<=2017 
	gen i2017 = year==2017 if year>=2014 & year<=2017 

	foreach v in `instlist9'	{
		gen i2014i`v' = i2014*`v'
		gen i2015i`v' = i2015*`v'
		gen i2016i`v' = i2016*`v'
		gen i2017i`v' = i2017*`v'

		local inter2014 "`inter2014'  i2014i`v'"
		local inter2015 "`inter2015'  i2015i`v'"
		local inter2016 "`inter2016'  i2016i`v'"
		local inter2017 "`inter2017'  i2017i`v'"
	}

	gen i2014D = i2014*pension65 
	gen i2015D = i2015*pension65 
	gen i2016D = i2016*pension65 
	gen i2017D = i2017*pension65 

* -- program
run "$root/stata/p1_gmmdidm.do"

* ======================================= *
* >>>>>> Descriptive <<<<<<<<
* ======================================= *

* --------------------- Table 1: Descriptive statistics ---------------------
sum pension65 `depvarlist' `instlist9' preyears postyears 
* ---------------------------------------------------------------------------

* --------------------- Table 2: propensity scores  -------------------------
reg pension65 `instlist9'  
	sum `instlist9'  if pension65==1 & e(sample) 
	sum `instlist9'  if pension65==0 & e(sample) 

foreach v in `instlist9' {
	reg `v' pension65  
} 
* ---------------------------------------------------------------------------


* ======================================= *
* >>>>> Regressions <<<<<<<<<
* ======================================= *
* It computes the GMM to build Figures 2 and 3
egen cvar = group(cluster year)
local sample "age>=65 & year~=2012 & year~=2013 & year<=2017"

foreach depvar in `depvarlist' 	{
local cloop = `cloop' + 1

di ""
di ""
di ""
di ""
di ">>>>>>>>>>> <<<<<<<<<<<<<"
di in yellow "sample: `sample'"
di ""
di in yellow "depvar: `depvar'"
di ""
di in yellow "instruments: `instlist9'"
di ">>>>>>>>>>> <<<<<<<<<<<<<"

 capture timer clear 1
 timer on 1

 gmmdidm `depvar' pension65  if `sample', inst(`instlist9') time(year) yfirst( 2014 2015 2016 2017 ) ysecond( 2010 2011 2014 2015 2016 2017) ref(2011) max(10) new cluster(cvar) 

 timer off 1
 timer list 1

* -- matrix for graph 
matrix M`depvar'_1 = [_b[/did2010],_b[/did2010] - invnorm(0.975)*_se[/did2010] , _b[/did2010] + invnorm(0.975)*_se[/did2010] , _b[/did2010] - invnorm(0.95)*_se[/did2010] , _b[/did2010] + invnorm(0.95)*_se[/did2010]]
matrix M`depvar'_2 = [0,0,0,0,0]
matrix M`depvar'_3 = [_b[/did2014],_b[/did2014] - invnorm(0.975)*_se[/did2014] , _b[/did2014] + invnorm(0.975)*_se[/did2014] , _b[/did2014] - invnorm(0.95)*_se[/did2014] , _b[/did2014] + invnorm(0.95)*_se[/did2014]]
matrix M`depvar'_4 = [_b[/did2015],_b[/did2015] - invnorm(0.975)*_se[/did2015] , _b[/did2015] + invnorm(0.975)*_se[/did2015] , _b[/did2015] - invnorm(0.95)*_se[/did2015] , _b[/did2015] + invnorm(0.95)*_se[/did2015]]
matrix M`depvar'_5 = [_b[/did2016],_b[/did2016] - invnorm(0.975)*_se[/did2016] , _b[/did2016] + invnorm(0.975)*_se[/did2016] , _b[/did2016] - invnorm(0.95)*_se[/did2016] , _b[/did2016] + invnorm(0.95)*_se[/did2016]]
matrix M`depvar'_6 = [_b[/did2017],_b[/did2017] - invnorm(0.975)*_se[/did2017] , _b[/did2017] + invnorm(0.975)*_se[/did2017] , _b[/did2017] - invnorm(0.95)*_se[/did2017] , _b[/did2017] + invnorm(0.95)*_se[/did2017]]
matrix M`depvar' = [M`depvar'_1 \M`depvar'_2 \M`depvar'_3 \M`depvar'_4 \M`depvar'_5 \M`depvar'_6]

matrix list M`depvar'


* >> post treatment indicators <<

* -- propensity score F and correlation
di in y "---------- Propensity score ------------"
di ""
di in y "F-stat = " r(F)
di in y "corr(D,Dhat) = " r(corrDD)
di ""
di in y "----------------------------------------"
* -- overid test
di in y "---------- Overid test -----------------"
di ""
estat overid
di ""
di in y "----------------------------------------"

* -- Stationarity of propensity score post-treatment ``test''
	reg pension65 i2014 i2016 i2017 `instlist9' `inter2014' `inter2016' `inter2017'   if `sample' & year~=. & (year>=2014 & year<=2017) & `depvar'~=., cluster(cvar)
		test `inter2014' `inter2016' `inter2017'

if "`depvar'"=="employed"	{
	reg pension65 i2014 i2017       `instlist9' `inter2014'             `inter2017'   if `sample' & year~=. & (year>=2014 & year<=2017) & year~=2016 & `depvar'~=., cluster(cvar)
		test `inter2014'  `inter2017'
}

* -- Assumption A6 post-treatment ``test''

	reg `depvar' pension65 i2014 i2016 i2017 `instlist9' `inter2014' `inter2016' `inter2017'   if `sample' & year~=. & (year>=2014 & year<=2017), cluster(cvar)
		test `inter2014' `inter2016' `inter2017'

if "`depvar'"=="employed"	{
	reg `depvar' pension65 i2014       i2017 `instlist9' `inter2014'             `inter2017'   if `sample' & year~=. & (year>=2014 & year<=2017) & year~=2016, cluster(cvar)
		test `inter2014'  `inter2017'	
}

								}

*-- Log file
capture log close