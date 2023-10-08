clear

global root "C:/Users/bjwil/OneDrive/Desktop/Mini-Project"

* ================================================================================= *
*   Author: Brandon Williams
*	Date: 10/4/2023
*	Mini-Project: Clean and prepare stata file
*	
* ================================================================================= *

*-- Log file
capture log close
log using "$root/stata/cleaning.log", replace

*-- Open data
use "$root/data/clean/pension65.dta", replace

* ======================================= *
* >>>>>>>>>>> Data Cleaning <<<<<<<<<
* ======================================= *

* -- unite missing variables
mvdecode _all, mv(99)


* -- remove those who didn't finish the interview 

drop if interview_result == . 

* -- dummy years
tab year, gen(yr)

* -- redifine poorly defined binaries / categories


label variable sex "0 for male"

replace sex = 0 if sex == 1
replace sex = 1 if sex == 2

label variable educ "education in levels"

replace educ = . if educ == 8 

label variable rural "0 for urban"

replace rural = 0 if rural == 1
replace rural = 1 if rural == 2

label variable residence "more pop to less pop"

gen employed = (work_activity < 4 | work_activity == 5)
gen hh_work = (work_activity == 4 | work_activity == 7)
gen other_work = (work_activity == 6 | work_activity > 7)

gen threat_violence = (humiliated == 1 | threaten == 1 | push == 1 | slap == 1 | violence == 1)
gen extreme_violence = (punch == 1 | kick == 1 | strangle == 1 | knife_threat == 1 | knife_attack == 1 | force_sex == 1 | severe_violence == 1)

gen medical_care = (receive_treat == 1 | measure_bp == 1 | measure_bsugar == 1 | dentist == 1 | mammogram == 1)

gen poor_mental_health = (no_sleep == 1 | no_sleep == 2 | no_sleep == 3 | depressed == 1 | depressed == 2 | depressed == 3)

* -- pension65 category
replace pension65 = 0 if pension65 == 2


* ======================================= *
* >>>>>>>>>>> Poverty Measures <<<<<<<<<
* ======================================= *

* -- roof material: rudimentary < 29, other > 39
gen roof1 = (roof < 29)			
gen roof2 = (roof < 39 & roof >29)
gen roof3 = (roof > 39)

* -- floor material: rudimentary < 29, other > 39

gen floor1 = (floor < 29)			
gen floor2 = (floor < 39 & floor >29)
gen floor3 = (floor > 39)

* -- wall material: natural < 19, rudimentary < 29, finished < 39, other > 39

gen wall1 = (wall < 19)
gen wall2 = (wall < 29 & wall > 19)
gen wall3= (wall > 29 & wall < 39)
gen wall4 = (wall >39)

gen water1 = (drnk_wtr_src == 13 | drnk_wtr_src == 22 | drnk_wtr_src == 41 | drnk_wtr_src == 42 | drnk_wtr_src == 43 | drnk_wtr_src == 51)
gen water2 = (drnk_wtr_src == 11 | drnk_wtr_src == 12 | drnk_wtr_src == 21 | drnk_wtr_src == 71)

save pension65_stata.dta, replace

*-- Log file
capture log close



