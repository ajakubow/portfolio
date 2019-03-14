********************************************************************************
* File: 			mainLogitsReplicate.do
*
* Description:		replication file for iachr analysis
*
* First version:  	20170224
* This verison:   	20170301
* Last change by: 	Alex
* requires:       	macro.csv
* provides:       	NA
********************************************************************************

*** Priors ---------------------------------------------------------------------
cap log close
clear all
cd "~/Dropbox/ProjectsCurrent/guim/plea2"


*** Load Master & Verify -------------------------------------------------------
use "build/output/plea2mergedData01.dta", clear  
cap datasignature confirm
notes _dta

* Model Macros *
#delimit ;
global ivars def_acceptD;
global  case integrity vic_group minority incumb_gov cejil_crt djge_court 
		adhocjudg lamicus  prvloss fst_appear; 
global  rhs integrity vic_group minority incumb_gov cejil_crt djge_court 
		adhocjudg lamicus  prvloss fst_appear LJI lgdp11 t;	
#delimit cr


*** Setup for Multiple Imputation Analysis -------------------------------------
mi import flong, m(imp) id(case d_judgmnt) imp(dpi_tf-gdp10)

replace prvloss =  0 if prvloss == .
	
psmatch2 def_acceptD $case, kernel kerneltype(normal) out(rights3)
	gen matchwgt = _weight
	gen pscore = _pscore

		
mi convert wide, clear		

			
*** Setup for Bootstrapped Standard Erros with Imputation ----------------------
program define myboot, rclass
	mi estimate, esampvaryok: ologit rights3 $ivars $rhs [pw = matchwgt], vce($vcetype)

	return scalar b_cop = el(e(b_mi),1,1)
	return scalar b_int = el(e(b_mi),1,2)
	return scalar b_grp = el(e(b_mi),1,3)
	return scalar b_min = el(e(b_mi),1,4)
	return scalar b_gov = el(e(b_mi),1,5)
	return scalar b_cjl = el(e(b_mi),1,6)
	return scalar b_njg = el(e(b_mi),1,7)
	return scalar b_ahc = el(e(b_mi),1,8)
	return scalar b_ami = el(e(b_mi),1,9)
	return scalar b_pls = el(e(b_mi),1,10)
	return scalar b_fst = el(e(b_mi),1,11)
	return scalar b_lji = el(e(b_mi),1,12)
	return scalar b_gdp = el(e(b_mi),1,13)
	return scalar b_tim = el(e(b_mi),1,14)
end


*** Main Models ----------------------------------------------------------------
#delimit;
bootstrap 
	b_def_acceptD	= r(b_cop)
	b_integrity 	= r(b_int)
	b_vic_group 	= r(b_grp) 
	b_minority 		= r(b_min)
	b_incumb_gov 	= r(b_gov)
	b_cejil_crt 	= r(b_cjl)
	b_djge_court 	= r(b_njg)
	b_adhocjudg		= r(b_ahc)
	b_lamicus  		= r(b_ami)
	b_prvloss 		= r(b_pls)
	b_fst_appear	= r(b_fst)
	b_LJI 			= r(b_lji)
	b_lgdp11		= r(b_gdp)
	b_t 			= r(b_tim),
	reps(10) : myboot;
#delimit cr


*** Main Models  ---------------------------------------------------------------
global vcetype cluster cmisscode
foreach dvar of varlist $dvars {
	* Priors *
	qui psmatch2 $ivars $rhs, kernel kerneltype(normal) out(`dvar'3)
		gen matchwgt = _weight
		gen pscore = _pscore

	* Model 1: Ordered Logit *
	mi estimate,  bootstrap, reps($reps) saving(m1): ///
		ologit `dvar'3 $ivars $m2rhs [pw = matchwgt], vce($vcetype)
		qui estat summarize, labels			
		estadd local ufx "No"
		estadd local match "Yes"	
		estadd local imp "Yes"		
		eststo `dvar'_m1
		
	* Model 2:  2-step FE for Commission & Court *
	qui ologit `dvar'3 i.cmisscode2
		predict `dvar'1_com2 `dvar'2_com2 `dvar'3_com2
	qui ologit `dvar'3 i.prescode
		predict `dvar'1_crt2 `dvar'2_crt2 `dvar'3_crt2

	bootstrap, reps($reps) saving(m2): ///
		ologit `dvar'3 $ivars $m2rhs `dvar'1_com2 `dvar'3_com2 ///
		`dvar'1_crt2 `dvar'3_crt2 [pw = matchwgt], vce($vcetype)
		qui estat summarize, labels
		estadd local ufx "Fixed"
		estadd local match "Yes"	
		estadd local imp "Yes"		
		eststo `dvar'_m2
		
	* Model 3: 2-Level Random Effects  *
	bootstrap, reps($reps) saving(m3): ///
		meologit `dvar'3 $ivars $m2rhs [pw = matchwgt] ///
		|| cmsprscode: , vce($vcetype)
		qui estat summarize, labels
		estadd local ufx "2-Level Random"
		estadd local match "Yes"	
		estadd local imp "Yes"		
		eststo `dvar'_m3

	* Model 4: 3-Level Random Effects  *
	bootstrap, reps($reps) saving(m4): ///
		meologit `dvar'3 $ivars $m2rhs  ///
		|| cmsprscode:  || ccode: [pw = matchwgt], vce($vcetype)
		qui estat summarize, labels
		estadd local ufx "3-Level Random"
		estadd local match "Yes"	
		estadd local imp "Yes"		
		eststo `dvar'_m4
}


*** Main Results Table ---------------------------------------------------------
local counter = 1
foreach dvar of varlist $dvars {
	local subtit `: word `counter' of $dvarVec'
	global ttitle "$modtype `subtit'"
	local tblcnt = `tblcnt' + 1 // table counter
	esttab `dvar'_m1 `dvar'_m2 `dvar'_m3 `dvar'_m4 
		using "${filen}.rtf", append ///
		b(%4.3f) se(%4.3f) onecell compress nogaps eqlabels(none) nonotes ///
		title({\pagebb{\b Table `tblcnt': ${ttitle}}}) label ///
		star(* 0.05 ** 0.01 *** 0.001) ///
		scalars("ufx Unit Effects" "match Matching" ///
				"imp Imputed Data" ///
				"N Observations"   "chi2 LR chi{\super 2}" ///
			"p Prob > chi{\super 2}") ///
		sfmt(%20s %20s %20s %12.0fc %10.3f %10.3f) ///
		mtitles($mtitles) ///
		nonumbers obslast noconstant order($ivars $m2rhs) ///
		keep($ivars $m2rhs)	///
		addnotes("$setype errors in parentheses." /// 
			"{\super *} p<0.05, {\super **} p<0.01, {\super ***} p<0.001." ///
			"Fixed-effects extracted from first-stage unit-dummy regressions for Commission and Court." ///
			"Random effects approach nests countries within Commission-Court regime.")	
	local counter = `counter' + 1
}


*** Main Models MFX ------------------------------------------------------------
foreach dvar of varlist $dvars {
	forvalues dvlvl = 1/$dvlvls {
		forvalues mod = 1/4 {
		display "`dvar' `dvlvl' `mod'"
		display "*******************"
		estimates restore `dvar'_m`mod'
		qui estat summarize, labels
		estadd margins, dydx(*) predict(outcome(`dvlvl')) atmeans post
				eststo `dvar'_m`mod'mfx_y`dvlvl'
		}		
		* Clean-Up *
		drop 	`dvar'1_com* `dvar'2_com* `dvar'3_com*  ///
				`dvar'1_crt* `dvar'2_crt* `dvar'3_crt*  ///
				`dvar'1_st* `dvar'2_st* `dvar'3_st*
	}
}


*** Marginal Effects Table -----------------------------------------------------
local tblcnt = 1
local counter = 1
global mtitlesMFX 1 2 3 4

foreach dvar of varlist $dvars {
	* Aggressive Tables *
	global modtype Marginal Effects (Aggressive Verdict): 
	local subtit `: word `counter' of $dvarVec'
	global ttitle "$modtype `subtit'"
	local tblcnt = `tblcnt' + 1 // table counter
	esttab `dvar'_m1mfx_y3 `dvar'_m2mfx_y3 `dvar'_m3mfx_y3 `dvar'_m4mfx_y3 ///
		using "${filen}Appendix.rtf", append b(%4.3f) se(%4.3f) onecell compress nogaps ///
		eqlabels(none) cells("margins_b(fmt(3)star)") collabels(none) ///
		title({\pagebb{\b Table A`tblcnt'a: ${ttitle}}}) label ///
		star(* 0.05 ** 0.01 *** 0.001) ///
		mtitles($mtitlesMFX) ///
		nonumbers obslast nodepvars noconstant ///
		order($ivars $m2rhs) ///
		keep($ivars $m2rhs)	///
		addnotes( "Tables report effect of a one-unit increase on the probability of receiving an aggressive settlement outcome." ///
				"{\super *} p<0.05, {\super **} p<0.01, {\super ***} p<0.001" ///
				"Marginal effect estimation holds all other variables at their means.")
	
	* Restrained Table *
	global modtype Marginal Effects (Restrained Verdict): 
	local subtit `: word `counter' of $dvarVec'
	global ttitle "$modtype `subtit'"
	esttab `dvar'_m1mfx_y1 `dvar'_m2mfx_y1 `dvar'_m3mfx_y1 `dvar'_m4mfx_y1 ///
		using "${filen}Appendix.rtf", append b(%4.3f) se(%4.3f) onecell compress nogaps ///
		eqlabels(none) cells("margins_b(fmt(3)star)") collabels(none) ///
		title({\pagebb{\b Table A`tblcnt'b: ${ttitle}}}) label ///
		star(* 0.05 ** 0.01 *** 0.001) ///
		mtitles($mtitlesMFX) ///
		nonumbers obslast nodepvars noconstant ///
		order($ivars $m2rhs) ///
		keep($ivars $m2rhs)	///
		addnotes("Tables report effect of a one-unit increase on the probability of receiving a restrained settlement outcome." ///
				"{\super *} p<0.05, {\super **} p<0.01, {\super ***} p<0.001" ///
				"Marginal effect estimation holds all other variables at their means.")
local counter = `counter' + 1
}	
