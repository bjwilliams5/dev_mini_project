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
log using "$root/stata/results_rdd.log", replace

*-- Open data
use "$root/stata/pension65_stata.dta", replace

*-- outcome, running variable, and cutoff defined, create running difference
gl Y employed
gl Z medical_care
gl R age
gl c = 64.5

gen double R = $R - $c

gen water1 = (drnk_wtr_src == 13 | drnk_wtr_src == 22 | drnk_wtr_src == 41 | drnk_wtr_src == 42 | drnk_wtr_src == 43 | drnk_wtr_src == 51)
gen water2 = (drnk_wtr_src == 11 | drnk_wtr_src == 12 | drnk_wtr_src == 21 | drnk_wtr_src == 71)


replace pension65 = . if pension65 == 8 | pension65 == 9

* ======================================= *
* >>>>>>>>>>> Propensity Weighting <<<<<<<<<
* ======================================= *


	
logit pension65 floor1 floor2 wall1 wall2 wall3 water1 water2 rural residence edu if age >= 65 & pension65 <2
predict pscore

su pscore if pension65==1, detail
su pscore if pension65==0, detail

histogram pscore, by(pension65) binrescale

teffects ipw (employed) (pension65 floor1 floor2 wall1 wall2 wall3 water1 water2 rural residence edu c.altitude##c.altitude) if age >= 65


* ======================================= *
* >>>>>>>>>>> RD Plots <<<<<<<<<
* ======================================= *



rdplot $Y $R if age>=55 & age<=75, c($c) p(1) h(10 10) masspoints(off) fuzzy(pension65)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
rdrobust $Y $R if age>=55 & age<=75, c($c) p(1) h(10 10) masspoints(off)

rdplot hh_work $R if age>=55 & age<=75, c($c) p(1) h(10 10) masspoints(off)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
rdrobust hh_work $R if age>=55 & age<=75, c($c) p(1) h(10 10) masspoints(off)
	
rdplot $Z $R if age>=55 & age<=75, c($c) p(1) h(10 10) masspoints(off)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
rdrobust $Z $R if age>=55 & age<=75, c($c) p(1) h(10 10) masspoints(off)

rdplot poor_mental_health $R if age>=55 & age<=75, c($c) p(1) h(10 10) masspoints(off)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
rdrobust poor_mental_health $R if age>=55 & age<=75, c($c) p(1) h(10 10) masspoints(off)
	


rdplot $Y $R if age>=55 & age<=75 & pscore >=.37, c($c) p(1) h(10 10) masspoints(off)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
rdplot $Z $R if age>=55 & age<=75 & pscore >=.37, c($c) p(1) h(10 10) masspoints(off)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))

rdplot hh_work $R if age>=55 & age<=75 & pscore >=.3, c($c) p(1) h(10 10) masspoints(off)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
* ======================================= *
* >>>>>>>>>>> Fuzzy RD estimates <<<<<<<<<
* ======================================= *

rdrobust $Y $R if age>=55 & age<=75, c($c) p(0) h(10 10) masspoints(off) fuzzy(pension65)

rdrobust poor_mental_health $R, c($c) p(1) h(5) masspoints(off) fuzzy(pension65)

* ======================================= *
* >>>>>>>>>>> Placebo <<<<<<<<<
* ======================================= *

foreach i  of numlist 61/69 {
	rdrobust $Y $R if age>=`i'-10 & age<=`i'+10, c(`i') p(1) h(10 10) masspoints(off)
} 


* ======================================= *
* >>>>>>>>>>> Bandwidth Sensitivity <<<<<<<<<
* ======================================= *


rdplot $Y $R if age>=60 & age<=70, c($c) p(1) h(5 5) masspoints(off) fuzzy(pension65)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
rdrobust $Y $R, c($c) p(1) h(5 5) masspoints(off)


rdplot hh_work $R if age >=59 & age <=71, c($c) p(1) h(5 5) masspoints(off)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
rdrobust hh_work $R, c($c) p(1) h(5 5) masspoints(off)
	
rdplot $Z $R if age>=58 & age<=72, c($c) p(1) h(5 5) masspoints(off)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
rdrobust $Z $R, c($c) p(1) h(5) masspoints(off)

rdplot poor_mental_health $R if age>=59 & age<=71, c($c) p(1) h(5 5) masspoints(off)	///
	graph_options(graphregion(color(white)) title("") /// 
	ytitle("`lab'") legend(off))
	
