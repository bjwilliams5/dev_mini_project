

* ----------------------------------------------------------------------------------
* DID when treatmet status is observed in only one period (paper: Botosaru and Gutierrez)
*
* 
*  		Federico Gutierrez 
* 			Vanderbilt University
*			federico.h.gutierrez@vanderbilt.edu
* -----------------------------------------------------------------------------------
*  Notes: 	
*			- multimple periods are allowed
*			- the periods to include in the first stage may be specified in yfirst
*			- the periods to include in the second stage may be specified in ysecond
* ----------------------------------------------------------------------------------

capture program drop gmmdidm
program define gmmdidm, rclass

version 12

syntax varlist(max=2) [if] [in] [aweight],  [Controls(varlist)] INStruments(varlist) Time(varlist) ///
											[ONEstep]  [First(string)] YFirst(string) YSecond(string) [REFperiod(string)] [ONLYpointestimates] /// 
											[MAXiterations(integer 10000)] [CONVergence(real 10)] [TECHnique(string)] [NEWspecification] [CLUSTER(string)] [PYear(string)]
tokenize `varlist'
marksample touse
local depvar = "`1'"
macro shift 1
local Dvar "`*'"

forvalues i = 1/12 	{
	capture matrix drop did`i'
						}
preserve
qui{

* -- years in pre-treatment
	local ypre `ysecond'
	foreach a in `yfirst'	{
	local ypre = subinword("`ypre'","`a'","",1)
	}

* -- sample weight
	if "`exp'"~=""{
	local _factor = subinword("`exp'", "=", "",1)
	sum `_factor'
	}
	if "`exp'"==""{
	tempvar _factor
	gen `_factor'=1
	sum `_factor'
	}

* -- keep variables and observations of interest
	capture keep `if'	
	keep `varlist' `controls' `instruments' `time' `_factor' `cluster'
	drop if `depvar' ==.
	drop if `time'==.
	foreach v in `controls' {
		drop if `v'==.
							}
	foreach v in `instruments' {
		drop if `v'==.
							}


* -- indicator of periods where D is observed (variable `posttreat')							
	tempvar posttreat

	capture drop __itime*
	tab `time', gen(__itime)
	local numper = r(r)
		
	gen `posttreat'=0
		forvalues i = 1/`numper' 	{
			sum `Dvar' if __itime`i'==1
				if `r(N)' ~=0 			{
					replace `posttreat'=1 if __itime`i'==1
										}
									}
	sum `posttreat'
	if r(mean) == 1 {
		noi di ""
		noi di in red "Treatment status is observed in all periods. Standard DID can be implemented"
		exit
					}

* -- Reference period (DID will be computed in relation to this base year)
	tempvar reftime

	if "`refperiod'" =="" 	{
		sum `time' if `posttreat'==0
		gen `reftime' = `time'==r(max)
							}	
	if "`refperiod'" ~="" 	{
		gen `reftime' = `time'==`refperiod'
							}
							
* -- Sample first stage (variable `samplefirst')
	tempvar samplefirst
				
	if "`yfirst'" ~= ""	{
		local nyfirst = wordcount("`yfirst'")
		tokenize `yfirst'

		local condfirst "`time'==`1'"
		forvalues i = 2/`nyfirst' 	{
			local condfirst "`condfirst' | `time'==``i''"
									}
		gen `samplefirst' = 0
		foreach y in `yfirst' 		{
			replace `samplefirst'=1 if `time'==`y'
									}
						}
					
* -- Sample second stage (variable `samplesecond')
	capture drop __stime*

	if "`ysecond'" ~= ""	{
		local nysecond = wordcount("`ysecond'")
		tokenize `ysecond'

		local condsecond "`time'==`1'"
		forvalues i = 2/`nysecond' 	{
			local condsecond "`condsecond' | `time'==``i''"
									}

	foreach a in `ysecond'	{
		gen __stime`a' = `time'==`a'
	}

		*noi tab `time' if `condsecond', gen(__stime)
						}

* -- Local indicating the year in second stage of the reference period					
	foreach a in `ysecond'	{
		sum `reftime' if __stime`a'==1
			if `r(mean)' ==1 		{
				local nref = `a'
									}
							}

* --------------------------------------------------------------------------------- *
* >>>>>>>>>>>>>>> Two-step estimator <<<<<<<<<<<<<<<<<<<<<<<	*
* --------------------------------------------------------------------------------- *
							
* -- First stage
	tempvar dhat
	capture matrix drop bf
						
	if "`first'" == "ols" | "`first'" == "" {
		reg `Dvar' `instruments' `controls' [aw=`_factor'] if `condfirst'
		predict `dhat'
		corr `Dvar' `dhat'
		local corrDD = r(rho)
		matrix bf = e(b)
		local F = e(F)
		local R2 = e(r2)		
		return scalar F = `F'
		return scalar R2 = `R2'
		return scalar corrDD = `corrDD'

											}
	if "`first'" == "logit"  {
		logit `Dvar' `instruments' `controls' [pw=`_factor'] if `condfirst'
		predict `dhat'
		matrix bf = e(b)
											}
	if "`first'" == "probit"  {
		probit `Dvar' `instruments' `controls' [pw=`_factor']  if `condfirst'
		predict `dhat'
		matrix bf = e(b)
											}


* -- Second stage new method
if "`newspecification'" ~= "" 		{
capture matrix drop auxelem
tempvar pxb ndepvar
capture drop `pxb'
local pivot =  word("`yfirst'",1)


	reg `depvar' `Dvar' `instruments' `controls' [aw=`_factor'] if  __stime`pivot'==1
	matrix b`pivot' = e(b)		

	matrix auxelem = b`pivot'[1,2...]
	matrix score `pxb' = auxelem

foreach a in `pyear' {
	reg  `depvar' `Dvar' `instruments' `controls' [aw=`_factor'] if  __stime`a'==1
	matrix b`a' = e(b)		
}

	local dy "`pivot' `pyear'"
	local othy `yfirst'
	foreach a in `dy'	{
	local othy = subinword("`othy'","`a'","",1)
	}

gen `ndepvar' = `depvar' - `pxb' 

foreach a in `othy' {
	reg `ndepvar' `Dvar'               `controls' [aw=`_factor'] if  __stime`a'==1
	matrix b`a' = e(b)		
}

foreach a in `ypre' {
	reg `ndepvar' `dhat'               `controls' [aw=`_factor'] if  __stime`a'==1
	matrix b`a' = e(b)		
}


								}


								
* --------------------------------------------------------------------------------- *
* >>>>>>>>>>>>>>> GMM estimator <<<<<<<<<<<<<<<<<<<<<<<	*
* --------------------------------------------------------------------------------- *

* --- Treatment status
	replace `Dvar' = 999 if `posttreat'==0

* --- Moment conditions
	capture drop cons
	gen cons = 1

* -- first stage
if "`first'" == "ols" | "`first'" == "" {
	local expt "{zb: `instruments' `controls' cons}"
	local A0 "(`Dvar'-`expt')"
	local eqf "(`A0'*`samplefirst')"
	local phat "{zb:}"
										}

if "`first'" == "probit" 				{
	local expt "{zb: `instruments' `controls' cons}"
	local A0 "(`Dvar'*normalden(`expt')/normal({zb:}) - (1-`Dvar')*normalden({zb:})/(1-normal({zb:})))"
	local eqf "(`A0'*`samplefirst')"
	local phat "normal({zb:})"
										}

if "`first'" == "logit"					{
	local expt "{zb: `instruments' `controls' cons}"
	local F "exp(`expt')/(1+exp({zb:}))"
	local F2 "exp({zb:})/(1+exp({zb:}))"
	local A0 "(`Dvar'*(1-`F') - (1-`Dvar')*`F2')"
	local eqf "(`A0'*`samplefirst')"
	local phat "exp({zb:})/(1+exp({zb:}))"
										}

* -- second stage

if "`newspecification'" ~= "" 		{

	foreach j in `ysecond' 	{
		local pxb`j' "{pxb:}"
	}

	local pxb`pivot' "{pxb: `instruments' `controls'}"
	local pxb`rnref' "{pxb:}"	

	foreach j in `pyear' 	{
		local pxb`j' "{pxb`j': `instruments' `controls'}"
	}



local ctr`pivot' "{xb`pivot': cons}"
local eq1 "((`depvar'  -{did`pivot'}*`Dvar' -{b0}*`Dvar' - `pxb`pivot'' - `ctr`pivot'')*__stime`pivot')"	
	matrix beq1 = [b`pivot'[1,1]-b`nref'[1,1], b`nref'[1,1], b`pivot'[1,2...]]
	local inst1 "instruments(2: `Dvar' `instruments' `controls')"

forvalues i = 2/`nyfirst'	{

	local ay = word("`yfirst'",`i')
	local j = `i'+1

	local ctr`ay' "{xb`ay':`controls' cons}"
	local eq`i' "((`depvar'  -{did`ay'}*`Dvar' -{b0}*`Dvar' - `ctr`ay''- `pxb`ay'')*__stime`ay')"	
	matrix beq`i' = [b`ay'[1,1]-b`nref'[1,1], b`ay'[1,2...]]
	local inst`i' "instruments(`j': `Dvar' `instruments' `controls')"
}


local nypre = wordcount("`ypre'")
forvalues h = 1/`nypre'	{

	local i = `nyfirst' + `h' 
	local ay = word("`ypre'",`h')
	local j = `i'+1

	local ctr`ay' "{xb`ay':`controls' cons}"
	local eq`i' "((`depvar'  -{did`ay'}*`phat' -{b0}*`phat' - `ctr`ay''- `pxb`ay'' )*__stime`ay')"	
	matrix beq`i' = [b`ay'[1,1]-b`nref'[1,1], b`ay'[1,2...]]
	local inst`i' "instruments(`j': `instruments' `controls')"

	if `nref'==`ay'	{
		local eq`i' "((`depvar'  -                  {b0}*`phat' - `ctr`ay''- `pxb`ay'')*__stime`ay')"	
		matrix beq`i' = [ b`nref'[1,2...]]
		local inst`i' "instruments(`j': `instruments' `controls')"
		}
}

matrix dir
			matrix inival = [bf]
			local allinstr ""
			forvalues  j = 1/`nysecond' 	{
				matrix inival = [inival,beq`j']
				local allinstr "`allinstr' `inst`j''"
										}
			
									}
	
* -- end qui
		}

* ------------- Output --------------------------------
/*
* --- periods included in each stage 						
di " -- periods included in first stage --"						
tab `time' if `condfirst'
di ""


di " -- periods included in second stage --"						
tab `time' if `condsecond'
di ""
*/
/*
* --- report values from two-step estimator
foreach v in `ysecond' 	{
	local did`v' =  b`v'[1,1]- b`nref'[1,1] 
	di "did`v' = " `did`v''
	return scalar did`v' = `did`v''
							}
*/

* --- gmm
if `convergence' ~= 10 {			
local pconver  "conv_ptol(`convergence')"
						}
						
if "`cluster'"~=""	{
	local vce "cluster `cluster'"
}

if "`onlypointestimates'" == "" {


if "`newspecification'" ~= "" 		{
gmm `eqf' ///
	`eq1' ///
	`eq2' ///
	`eq3' ///
	`eq4' ///
	`eq5' ///
	`eq6' ///
	`eq7' ///
	`eq8' ///
    [aw=`_factor'], instruments(1: `instruments' `controls') `allinstr'  winitial(identity) from(inival)	///
					`onestep' conv_maxiter(`maxiterations') technique(`technique') `pconver'  vce(`vce')
								}
	
								}
restore

end

