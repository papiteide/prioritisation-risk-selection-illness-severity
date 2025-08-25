

clear

cap log close
log using "$log/results_with_bootstrap.log", replace

display "*******************************************************************************"
display "*******************************************************************************"
display "Results that require bootstrap start here"
display "*******************************************************************************"
display "*******************************************************************************"

use "$datasets/dataset_cleaned.dta", clear


replace age=. if age==-9
replace age = age/100

 
*********************************************************************************
*Setting the PHI variables to be used as factor variables by the margins command
*********************************************************************************

gen pmi_type=1 if pmi_employer==1
replace pmi_type=2 if pmi_wages==1
replace pmi_type=3 if pmi_relative==1
replace pmi_type=4 if pmi_ind==1
replace pmi_type=5 if pmi_no==1

label variable pmi_type "PHI status of the individual"
label define lab_pmi_type  1 "Employer provided" 2 "Deducted wages" 3 "Through relative" 4 "Individually bought" 5 "No PMI"
label values pmi_type lab_pmi_type



***************************************************************************************
*Setting the Education variables to be used as factor variables by the margins command
***************************************************************************************

*Specify the smallest education level as the base level
fvset base 0 edlevel
gen edu45=1 if edlevel==4|edlevel==5
replace edu45=0 if edlevel<4&edlevel>=0


****************************************************************************************
***************************************************************************************


*-------------------------------------
*Defining the samples of interest
*-------------------------------------
*keep only observations with observed income and education
*This would be England, Wales, and Scotland
gen region_sample=1 if bigregion==1|bigregion==2|bigregion==3
gen ln_incomehh=ln(incomehh)
gen sample_interest = 1 if (bigregion==1|bigregion==2|bigregion==3) & edlevel != . & ln_incomehh != . & married_cohab != .



*************************************************************************
*Creating a vble on whether they had insurance on the previous wave
**************************************************************************

sort pid wave
gen pmi_lag1=pmi[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)


**************************************************************************
*Globals for regressions
**************************************************************************



global var_pmi      "i.pmi_type"

*smoker
global var_basic     " female c.age c.age#c.age married_cohab"
global var_basic_edu      " female c.age c.age#c.age i.edlevel married_cohab"
global var_basic_eduincome       "female c.age c.age#c.age i.edlevel  married_cohab c.ln_incomehh "
global var_basic_income       "female c.age c.age#c.age married_cohab c.ln_incomehh "


global var_fixed       "region_2-region_19 wave_2-wave_12 time_elapsed time_elapsed2 time_elapsed3 "
global var_fixed1      "region_2-region_19 wave_2-wave_12 "
global var_fixed_region "region_2-region_19 "
global var_fixed_wave 	"wave_2-wave_12 "
global var_fixed_time_elapsed "time_elapsed time_elapsed2 time_elapsed3 "


************************************************************************
*Taking into account childbirth hospitalisations
************************************************************************

gen nhs1_nobirth=nhs1
replace nhs1_nobirth=. if hospch==1
*We give missing to those women for which all the hospitalisations were due to childbirth


************************************************************************************************************************
* This is the sample used for bootstrap
************************************************************************************************************************

* This is the sample used to estimate
keep if sample_interest == 1
save "$junk\sample_bootstrap.dta", replace


************************************************************************************************************************
*Outcome variables
global var_output1	" nhs1_nobirth consult_nhs "
global var_output2	" hosp consultant "
global var_output3	" nhs1_nobirth consult_nhs hosp consultant "
global var_output4_1 "nhs1_nobirth"
global var_output4_2 "consult_nhs"
global var_output4_3 "hosp"
global var_output4_4 "consultant"


************************************************************************************************************************
*-----------------------------------------------------------------------------------------------------------------------
* Here we start the bootstrap loop
*-----------------------------------------------------------------------------------------------------------------------
************************************************************************************************************************

clear
clear matrix


* Number of replications
local nrep = 1000


* Starting quietly bracket
matrix drop _all
quietly {
	set seed 666
	
	* Starting Replication loop
	forvalues rep = 0(1)`nrep' {
		use "$junk\sample_bootstrap.dta", clear
		
		noi display "This is iteration number " `rep'
		
	* This part obtains the random sample
	* If rep==0, we will have the estimated effect, so we do not resample when rep==0
	
	if `rep'>0 {
		bsample, cluster(pid)
	}
	
	************************************************************************************************************************
	*----------------------------------------------------------------------------------------------------------------------
	* Computing the severity index
	*----------------------------------------------------------------------------------------------------------------------
	************************************************************************************************************************	

	tab exc1_vpoor5

	label variable exc1_vpoor5 "self-assessed health 1 = excellent, 5 = very poor"

	gen age_delete=age
	gen age2_delete=age*age
	
	oprobit exc1_vpoor5 hlprba-hlprbm age_delete age2_delete if sample_interest==1, cluster(pid)
	predict severity_temp, index
	
	sum severity_temp, detail
	gen severity_1=(severity_temp-r(min))/(r(max)-r(min))
	label variable severity_1 "severity index [0,1] with weights computed using self-assessed health"
	drop severity_temp

	* Computing lag severity
	sort pid wave
	gen severity_1_lag1=severity_1[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)
	label variable severity_1_lag1 "lag one period severity index [0,1] with weights computed using self-assessed health"
		
	gen severity=severity_1
	gen severity2=severity*severity
	gen severity3=severity2*severity
	gen severity4=severity3*severity
	
	*lag severity
	gen severity_lag1=severity_1_lag1
	gen severity2_lag1=severity_lag1*severity_lag1
		
	************************************************************************************************************************
	* Regression using the severity index
	************************************************************************************************************************	
		
	foreach Y of global var_output1 {
	* Begining of NHS outcome variable loop
		
	*----------------------------------------------------------------------------------------------------------------------
	* Without education or household incomehh
	*----------------------------------------------------------------------------------------------------------------------
	* Probit
	probit `Y' severity c.severity#c.severity $var_basic $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	if `rep' == 0 {
		matrix obs_`Y'_b = e(N)
	}
	
	if `rep' == 0 {
		matrix observed_`Y'_b = e(b)
		matrix minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix observed_`Y'_b = (observed_`Y'_b , minimum_`Y'_b, obs_`Y'_b)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix b_`Y'_b = e(b)
		matrix minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_b = (b_`Y'_b , minimum_`Y'_b, obs_`Y'_b)
	}
	if `rep' > 1 {
		matrix b_`Y'_b_`rep' = e(b)
		matrix minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_b_`rep' = (b_`Y'_b_`rep' , minimum_`Y'_b, obs_`Y'_b)
		matrix b_`Y'_b = b_`Y'_b \ b_`Y'_b_`rep'
		
	}
	
	* Marginal effects
	margins, dydx(severity) at(severity = (0 0.01 0.05(0.05)0.5))
	if `rep' == 0 {
		matrix margins_`Y'_b = r(b)
	}
	if `rep' == 1 {
		matrix marginsb_`Y'_b = r(b)
	}
	if `rep' > 1 {
		matrix marginsb_`Y'_b_`rep' = r(b)
		matrix marginsb_`Y'_b = marginsb_`Y'_b \ marginsb_`Y'_b_`rep'
	}
		
	* LPM
	reg `Y' severity c.severity#c.severity $var_basic $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	if `rep' == 0 {
		matrix lpmobserved_`Y'_b = e(b)
		matrix lpm_minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmobserved_`Y'_b = (lpmobserved_`Y'_b , lpm_minimum_`Y'_b, obs_`Y'_b)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix lpmb_`Y'_b = e(b)
		matrix lpm_minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_b = (lpmb_`Y'_b , lpm_minimum_`Y'_b, obs_`Y'_b)
	}
	if `rep' > 1 {
		matrix lpmb_`Y'_b_`rep' = e(b)
		matrix lpm_minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_b_`rep' = (lpmb_`Y'_b_`rep' , lpm_minimum_`Y'_b, obs_`Y'_b)
		matrix lpmb_`Y'_b = lpmb_`Y'_b \ lpmb_`Y'_b_`rep'
	}
	
	* Marginal effects
	margins, dydx(severity) at(severity = (0 0.01 0.05(0.05)0.5))
	if `rep' == 0 {
		matrix lpmmar_`Y'_b = r(b)
	}
	if `rep' == 1 {
		matrix lpmmarb_`Y'_b = r(b)
	}
	if `rep' > 1 {
		matrix lpmmarb_`Y'_b_`rep' = r(b)
		matrix lpmmarb_`Y'_b = lpmmarb_`Y'_b \ lpmmarb_`Y'_b_`rep'
	}
	
	
	
	*----------------------------------------------------------------------------------------------------------------------
	* With education but no household income
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' severity c.severity#c.severity  $var_basic_edu $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)

	if `rep' == 0 {
		matrix obs_`Y'_be = e(N)
	}
	
	if `rep' == 0 {
		matrix observed_`Y'_be = e(b)
		matrix minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix observed_`Y'_be = (observed_`Y'_be , minimum_`Y'_be, obs_`Y'_be)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix b_`Y'_be = e(b)
		matrix minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_be = (b_`Y'_be , minimum_`Y'_be, obs_`Y'_be)
	}
	if `rep' > 1 {
		matrix b_`Y'_be_`rep' = e(b)
		matrix minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_be_`rep' = (b_`Y'_be_`rep' , minimum_`Y'_be, obs_`Y'_be)
		matrix b_`Y'_be = b_`Y'_be \ b_`Y'_be_`rep'
	}
		
	
	* LPM
	reg `Y' severity c.severity#c.severity  $var_basic_edu $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	if `rep' == 0 {
		matrix lpmobserved_`Y'_be = e(b)
		matrix lpm_minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmobserved_`Y'_be = (lpmobserved_`Y'_be , lpm_minimum_`Y'_be, obs_`Y'_be)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix lpmb_`Y'_be = e(b)
		matrix lpm_minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_be = (lpmb_`Y'_be , lpm_minimum_`Y'_be, obs_`Y'_be)
	}
	if `rep' > 1 {
		matrix lpmb_`Y'_be_`rep' = e(b)
		matrix lpm_minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_be_`rep' = (lpmb_`Y'_be_`rep' , lpm_minimum_`Y'_be, obs_`Y'_be)
		matrix lpmb_`Y'_be = lpmb_`Y'_be \ lpmb_`Y'_be_`rep'
	}
	
	*----------------------------------------------------------------------------------------------------------------------
	* With education and household income
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' severity c.severity#c.severity  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	
	if `rep' == 0 {
		matrix obs_`Y'_bei = e(N)
	}
	
	if `rep' == 0 {
		matrix observed_`Y'_bei = e(b)
		matrix minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix observed_`Y'_bei = (observed_`Y'_bei , minimum_`Y'_bei , obs_`Y'_bei)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix b_`Y'_bei = e(b)
		matrix minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_bei = (b_`Y'_bei , minimum_`Y'_bei , obs_`Y'_bei)
	}
	if `rep' > 1 {
		matrix b_`Y'_bei_`rep' = e(b)
		matrix minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_bei_`rep' = (b_`Y'_bei_`rep' , minimum_`Y'_bei , obs_`Y'_bei)
		matrix b_`Y'_bei = b_`Y'_bei \ b_`Y'_bei_`rep'
	}
	
	
	* Marginal effects
	margins, dydx(severity) at(severity = (0 0.01 0.05(0.05)0.5))
	if `rep' == 0 {
		matrix margins_`Y'_bei = r(b)
	}
	if `rep' == 1 {
		matrix marginsb_`Y'_bei = r(b)
	}
	if `rep' > 1 {
		matrix marginsb_`Y'_bei_`rep' = r(b)
		matrix marginsb_`Y'_bei = marginsb_`Y'_bei \ marginsb_`Y'_bei_`rep'
	}
	
	* Probit Conditional probability
	margins, at(severity = (0 0.01 0.05(0.05)0.5)) post
	if `rep' == 0 {
		matrix pprob_`Y'_bei = r(b)
	}
	if `rep' == 1 {
		matrix pprobb_`Y'_bei = r(b)
	}
	if `rep' > 1 {
		matrix pprobb_`Y'_bei_`rep' = r(b)
		matrix pprobb_`Y'_bei = pprobb_`Y'_bei \ pprobb_`Y'_bei_`rep'
	}
	
	* LPM
	reg `Y' severity c.severity#c.severity  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	if `rep' == 0 {
		matrix lpmobserved_`Y'_bei = e(b)
		matrix lpm_minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmobserved_`Y'_bei = (lpmobserved_`Y'_bei , lpm_minimum_`Y'_bei , obs_`Y'_bei)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix lpmb_`Y'_bei = e(b)
		matrix lpm_minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_bei = (lpmb_`Y'_bei , lpm_minimum_`Y'_bei , obs_`Y'_bei)
	}
	if `rep' > 1 {
		matrix lpmb_`Y'_bei_`rep' = e(b)
		matrix lpm_minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_bei_`rep' = (lpmb_`Y'_bei_`rep' , lpm_minimum_`Y'_bei , obs_`Y'_bei)
		matrix lpmb_`Y'_bei = lpmb_`Y'_bei \ lpmb_`Y'_bei_`rep'
	}
	
	* Marginal effects
	margins, dydx(severity) at(severity = (0 0.01 0.05(0.05)0.5))
	if `rep' == 0 {
		matrix lpmmar_`Y'_bei = r(b)
	}
	if `rep' == 1 {
		matrix lpmmarb_`Y'_bei = r(b)
	}
	if `rep' > 1 {
		matrix lpmmarb_`Y'_bei_`rep' = r(b)
		matrix lpmmarb_`Y'_bei = lpmmarb_`Y'_bei \ lpmmarb_`Y'_bei_`rep'
	}

	* LPM Conditional probability
	margins, at(severity = (0 0.01 0.05(0.05)0.5)) post
	if `rep' == 0 {
		matrix lprob_`Y'_bei = r(b)
	}
	if `rep' == 1 {
		matrix lprobb_`Y'_bei = r(b)
	}
	if `rep' > 1 {
		matrix lprobb_`Y'_bei_`rep' = r(b)
		matrix lprobb_`Y'_bei = lprobb_`Y'_bei \ lprobb_`Y'_bei_`rep'
	}
	

	*----------------------------------------------------------------------------------------------------------------------
	* With education and household income (third order polynomial)
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' severity c.severity#c.severity c.severity#c.severity#c.severity  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)

	if `rep' == 0 {
		matrix obs_`Y'_bei3 = e(N)
	}
	
	if `rep' == 0 {
		matrix observed_`Y'_bei3 = e(b)
		matrix minimum_`Y'_bei3 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix observed_`Y'_bei3 = (observed_`Y'_bei3 , minimum_`Y'_bei3 , obs_`Y'_bei3)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix b_`Y'_bei3 = e(b)
		matrix minimum_`Y'_bei3 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_bei3 = (b_`Y'_bei3 , minimum_`Y'_bei3 , obs_`Y'_bei3)
	}
	if `rep' > 1 {
		matrix b_`Y'_bei3_`rep' = e(b)
		matrix minimum_`Y'_bei3 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_bei3_`rep' = (b_`Y'_bei3_`rep' , minimum_`Y'_bei3 , obs_`Y'_bei3)
		matrix b_`Y'_bei3 = b_`Y'_bei3 \ b_`Y'_bei3_`rep'
	}
	
	
	*LPM
	reg `Y' severity c.severity#c.severity c.severity#c.severity#c.severity  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	if `rep' == 0 {
		matrix lpmobserved_`Y'_bei3 = e(b)
		matrix lpm_minimum_`Y'_bei3 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmobserved_`Y'_bei3 = (lpmobserved_`Y'_bei3 , lpm_minimum_`Y'_bei3 , obs_`Y'_bei3)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix lpmb_`Y'_bei3 = e(b)
		matrix lpm_minimum_`Y'_bei3 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_bei3 = (lpmb_`Y'_bei3 , lpm_minimum_`Y'_bei3 , obs_`Y'_bei3)
	}
	if `rep' > 1 {
		matrix lpmb_`Y'_bei3_`rep' = e(b)
		matrix lpm_minimum_`Y'_bei3 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_bei3_`rep' = (lpmb_`Y'_bei3_`rep' , lpm_minimum_`Y'_bei3 , obs_`Y'_bei3)
		matrix lpmb_`Y'_bei3 = lpmb_`Y'_bei3 \ lpmb_`Y'_bei3_`rep'
	}
	
	*----------------------------------------------------------------------------------------------------------------------
	* With education and household income (fourth order polynomial)
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' severity c.severity#c.severity c.severity#c.severity#c.severity c.severity#c.severity#c.severity#c.severity  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)

	if `rep' == 0 {
		matrix obs_`Y'_bei4 = e(N)
	}
	
	if `rep' == 0 {
		matrix observed_`Y'_bei4 = e(b)
		matrix minimum_`Y'_bei4 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix observed_`Y'_bei4 = (observed_`Y'_bei4 , minimum_`Y'_bei4 , obs_`Y'_bei4)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix b_`Y'_bei4 = e(b)
		matrix minimum_`Y'_bei4 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_bei4 = (b_`Y'_bei4 , minimum_`Y'_bei4 , obs_`Y'_bei4)
	}
	if `rep' > 1 {
		matrix b_`Y'_bei4_`rep' = e(b)
		matrix minimum_`Y'_bei4 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_bei4_`rep' = (b_`Y'_bei4_`rep' , minimum_`Y'_bei4 , obs_`Y'_bei4)
		matrix b_`Y'_bei4 = b_`Y'_bei4 \ b_`Y'_bei4_`rep'
	}
	
	
	* LPM
	reg `Y' severity c.severity#c.severity c.severity#c.severity#c.severity c.severity#c.severity#c.severity#c.severity  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	if `rep' == 0 {
		matrix lpmobserved_`Y'_bei4 = e(b)
		matrix lpm_minimum_`Y'_bei4 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmobserved_`Y'_bei4 = (lpmobserved_`Y'_bei4 , lpm_minimum_`Y'_bei4 , obs_`Y'_bei4)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix lpmb_`Y'_bei4 = e(b)
		matrix lpm_minimum_`Y'_bei4 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_bei4 = (lpmb_`Y'_bei4 , lpm_minimum_`Y'_bei4 , obs_`Y'_bei4)
	}
	if `rep' > 1 {
		matrix lpmb_`Y'_bei4_`rep' = e(b)
		matrix lpm_minimum_`Y'_bei4 =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_bei4_`rep' = (lpmb_`Y'_bei4_`rep' , lpm_minimum_`Y'_bei4 , obs_`Y'_bei4)
		matrix lpmb_`Y'_bei4 = lpmb_`Y'_bei4 \ lpmb_`Y'_bei4_`rep'
	}

	
	
	}
	* End of nhs outcome variable loop
	
	
	
	
	
	
	foreach Y of global var_output2 {
	* Begining of all types hospitalisaion/consultation outcome variable loop
	
	*----------------------------------------------------------------------------------------------------------------------
	* Without education or household incomehh
	*----------------------------------------------------------------------------------------------------------------------
	* Probit
	probit `Y' severity c.severity#c.severity $var_basic $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	
	if `rep' == 0 {
		matrix obs_`Y'_b = e(N)
	}
	
	if `rep' == 0 {
		matrix observed_`Y'_b = e(b)
		matrix minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix observed_`Y'_b = (observed_`Y'_b , minimum_`Y'_b, obs_`Y'_b)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix b_`Y'_b = e(b)
		matrix minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_b = (b_`Y'_b , minimum_`Y'_b, obs_`Y'_b)
	}
	if `rep' > 1 {
		matrix b_`Y'_b_`rep' = e(b)
		matrix minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_b_`rep' = (b_`Y'_b_`rep' , minimum_`Y'_b, obs_`Y'_b)
		matrix b_`Y'_b = b_`Y'_b \ b_`Y'_b_`rep'
		
	}
	
	* LPM
	reg `Y' severity c.severity#c.severity $var_basic $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	if `rep' == 0 {
		matrix lpmobserved_`Y'_b = e(b)
		matrix lpm_minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmobserved_`Y'_b = (lpmobserved_`Y'_b , lpm_minimum_`Y'_b, obs_`Y'_b)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix lpmb_`Y'_b = e(b)
		matrix lpm_minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_b = (lpmb_`Y'_b , lpm_minimum_`Y'_b, obs_`Y'_b)
	}
	if `rep' > 1 {
		matrix lpmb_`Y'_b_`rep' = e(b)
		matrix lpm_minimum_`Y'_b =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_b_`rep' = (lpmb_`Y'_b_`rep' , lpm_minimum_`Y'_b, obs_`Y'_b)
		matrix lpmb_`Y'_b = lpmb_`Y'_b \ lpmb_`Y'_b_`rep'
	}
		
	
	
	*----------------------------------------------------------------------------------------------------------------------
	* With education but no household income
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' severity c.severity#c.severity  $var_basic_edu $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)

	if `rep' == 0 {
		matrix obs_`Y'_be = e(N)
	}
	
	if `rep' == 0 {
		matrix observed_`Y'_be = e(b)
		matrix minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix observed_`Y'_be = (observed_`Y'_be , minimum_`Y'_be, obs_`Y'_be)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix b_`Y'_be = e(b)
		matrix minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_be = (b_`Y'_be , minimum_`Y'_be, obs_`Y'_be)
	}
	if `rep' > 1 {
		matrix b_`Y'_be_`rep' = e(b)
		matrix minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_be_`rep' = (b_`Y'_be_`rep' , minimum_`Y'_be, obs_`Y'_be)
		matrix b_`Y'_be = b_`Y'_be \ b_`Y'_be_`rep'
	}
		
	
	* LPM
	reg `Y' severity c.severity#c.severity  $var_basic_edu $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	if `rep' == 0 {
		matrix lpmobserved_`Y'_be = e(b)
		matrix lpm_minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmobserved_`Y'_be = (lpmobserved_`Y'_be , lpm_minimum_`Y'_be, obs_`Y'_be)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix lpmb_`Y'_be = e(b)
		matrix lpm_minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_be = (lpmb_`Y'_be , lpm_minimum_`Y'_be, obs_`Y'_be)
	}
	if `rep' > 1 {
		matrix lpmb_`Y'_be_`rep' = e(b)
		matrix lpm_minimum_`Y'_be =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_be_`rep' = (lpmb_`Y'_be_`rep' , lpm_minimum_`Y'_be, obs_`Y'_be)
		matrix lpmb_`Y'_be = lpmb_`Y'_be \ lpmb_`Y'_be_`rep'
	}
	
	*----------------------------------------------------------------------------------------------------------------------
	* With education and household income
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' severity c.severity#c.severity  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)

	
	if `rep' == 0 {
		matrix obs_`Y'_bei = e(N)
	}
	
	if `rep' == 0 {
		matrix observed_`Y'_bei = e(b)
		matrix minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix observed_`Y'_bei = (observed_`Y'_bei , minimum_`Y'_bei , obs_`Y'_bei)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix b_`Y'_bei = e(b)
		matrix minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_bei = (b_`Y'_bei , minimum_`Y'_bei , obs_`Y'_bei)
	}
	if `rep' > 1 {
		matrix b_`Y'_bei_`rep' = e(b)
		matrix minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix b_`Y'_bei_`rep' = (b_`Y'_bei_`rep' , minimum_`Y'_bei , obs_`Y'_bei)
		matrix b_`Y'_bei = b_`Y'_bei \ b_`Y'_bei_`rep'
	}
		
	* Probit Conditional probability
	margins, at(severity = (0 0.01 0.05(0.05)0.5)) post
	if `rep' == 0 {
		matrix pprob_`Y'_bei = r(b)
	}
	if `rep' == 1 {
		matrix pprobb_`Y'_bei = r(b)
	}
	if `rep' > 1 {
		matrix pprobb_`Y'_bei_`rep' = r(b)
		matrix pprobb_`Y'_bei = pprobb_`Y'_bei \ pprobb_`Y'_bei_`rep'
	}
	
	* LPM
	reg `Y' severity c.severity#c.severity  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1, cluster(pid)
	if `rep' == 0 {
		matrix lpmobserved_`Y'_bei = e(b)
		matrix lpm_minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmobserved_`Y'_bei = (lpmobserved_`Y'_bei , lpm_minimum_`Y'_bei , obs_`Y'_bei)
	}
	* Here, I accumulate the coefficient matrices...
	if `rep' == 1 {
		matrix lpmb_`Y'_bei = e(b)
		matrix lpm_minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_bei = (lpmb_`Y'_bei , lpm_minimum_`Y'_bei , obs_`Y'_bei)
	}
	if `rep' > 1 {
		matrix lpmb_`Y'_bei_`rep' = e(b)
		matrix lpm_minimum_`Y'_bei =-_b[severity]/(2*_b[c.severity#c.severity])
		matrix lpmb_`Y'_bei_`rep' = (lpmb_`Y'_bei_`rep' , lpm_minimum_`Y'_bei , obs_`Y'_bei)
		matrix lpmb_`Y'_bei = lpmb_`Y'_bei \ lpmb_`Y'_bei_`rep'
	}
	
	* LPM Conditional probability
	margins, at(severity = (0 0.01 0.05(0.05)0.5)) post
	if `rep' == 0 {
		matrix lprob_`Y'_bei = r(b)
	}
	if `rep' == 1 {
		matrix lprobb_`Y'_bei = r(b)
	}
	if `rep' > 1 {
		matrix lprobb_`Y'_bei_`rep' = r(b)
		matrix lprobb_`Y'_bei = lprobb_`Y'_bei \ lprobb_`Y'_bei_`rep'
	}
		
	}
	* End of all types of services outcome variable loop
	
	
	
	
	}
	* End of sampling loop

}
* End of quietly bracket


************************************************************************************************************************
* Now, I will rename the matrices. This will make easier to read the coefficients
************************************************************************************************************************
* hosp = hospitalisation regardless of publicly/privately funded
* b = basics
* be = basics + education
* bei = basics + education + incomehh
* bei3 = basics + education + incomehh + 3rd-order polynomial
* bei4 = basics + education + incomehh + 3rd-order polynomial + 4th-order polynomial
*matrix(ij): i = 1(nhs1_nobirth) 2(consult_nhs) 3(hosp) 4(consultant)
*			 j = 1(b) 2(be) 3(bei) 4(bei3) 5(bei4)

{
* Matrices for the probit estimates
matrix observed11 = observed_nhs1_nobirth_b
matrix observed12 = observed_nhs1_nobirth_be
matrix observed13 = observed_nhs1_nobirth_bei
matrix observed14 = observed_nhs1_nobirth_bei3
matrix observed15 = observed_nhs1_nobirth_bei4

matrix observed21 = observed_consult_nhs_b
matrix observed22 = observed_consult_nhs_be
matrix observed23 = observed_consult_nhs_bei
matrix observed24 = observed_consult_nhs_bei3
matrix observed25 = observed_consult_nhs_bei4

matrix observed31 = observed_hosp_b
matrix observed32 = observed_hosp_be
matrix observed33 = observed_hosp_bei

matrix observed41 = observed_consultant_b
matrix observed42 = observed_consultant_be
matrix observed43 = observed_consultant_bei

matrix b11 = b_nhs1_nobirth_b
matrix b12 = b_nhs1_nobirth_be
matrix b13 = b_nhs1_nobirth_bei
matrix b14 = b_nhs1_nobirth_bei3
matrix b15 = b_nhs1_nobirth_bei4

matrix b21 = b_consult_nhs_b
matrix b22 = b_consult_nhs_be
matrix b23 = b_consult_nhs_bei
matrix b24 = b_consult_nhs_bei3
matrix b25 = b_consult_nhs_bei4

matrix b31 = b_hosp_b
matrix b32 = b_hosp_be
matrix b33 = b_hosp_bei

matrix b41 = b_consultant_b
matrix b42 = b_consultant_be
matrix b43 = b_consultant_bei


* Matrices for the LPM estimates
matrix lpmobserved11 = lpmobserved_nhs1_nobirth_b
matrix lpmobserved12 = lpmobserved_nhs1_nobirth_be
matrix lpmobserved13 = lpmobserved_nhs1_nobirth_bei
matrix lpmobserved14 = lpmobserved_nhs1_nobirth_bei3
matrix lpmobserved15 = lpmobserved_nhs1_nobirth_bei4

matrix lpmobserved21 = lpmobserved_consult_nhs_b
matrix lpmobserved22 = lpmobserved_consult_nhs_be
matrix lpmobserved23 = lpmobserved_consult_nhs_bei
matrix lpmobserved24 = lpmobserved_consult_nhs_bei3
matrix lpmobserved25 = lpmobserved_consult_nhs_bei4

matrix lpmobserved31 = lpmobserved_hosp_b
matrix lpmobserved32 = lpmobserved_hosp_be
matrix lpmobserved33 = lpmobserved_hosp_bei

matrix lpmobserved41 = lpmobserved_consultant_b
matrix lpmobserved42 = lpmobserved_consultant_be
matrix lpmobserved43 = lpmobserved_consultant_bei

matrix lpmb11 = lpmb_nhs1_nobirth_b
matrix lpmb12 = lpmb_nhs1_nobirth_be
matrix lpmb13 = lpmb_nhs1_nobirth_bei
matrix lpmb14 = lpmb_nhs1_nobirth_bei3
matrix lpmb15 = lpmb_nhs1_nobirth_bei4

matrix lpmb21 = lpmb_consult_nhs_b
matrix lpmb22 = lpmb_consult_nhs_be
matrix lpmb23 = lpmb_consult_nhs_bei
matrix lpmb24 = lpmb_consult_nhs_bei3
matrix lpmb25 = lpmb_consult_nhs_bei4

matrix lpmb31 = lpmb_hosp_b
matrix lpmb32 = lpmb_hosp_be
matrix lpmb33 = lpmb_hosp_bei

matrix lpmb41 = lpmb_consultant_b
matrix lpmb42 = lpmb_consultant_be
matrix lpmb43 = lpmb_consultant_bei


* Matrices for probit marginal effects
matrix marginsobserved11 = margins_nhs1_nobirth_b
matrix marginsobserved13 = margins_nhs1_nobirth_bei
matrix marginsobserved21 = margins_consult_nhs_b
matrix marginsobserved23 = margins_consult_nhs_bei

matrix marginsb11 = marginsb_nhs1_nobirth_b
matrix marginsb13 = marginsb_nhs1_nobirth_bei
matrix marginsb21 = marginsb_consult_nhs_b
matrix marginsb23 = marginsb_consult_nhs_bei


* Matrices for lpm marginal effects
matrix lpmmar11 = lpmmar_nhs1_nobirth_b
matrix lpmmar13 = lpmmar_nhs1_nobirth_bei
matrix lpmmar21 = lpmmar_consult_nhs_b
matrix lpmmar23 = lpmmar_consult_nhs_bei

matrix lpmmarb11 = lpmmarb_nhs1_nobirth_b
matrix lpmmarb13 = lpmmarb_nhs1_nobirth_bei
matrix lpmmarb21 = lpmmarb_consult_nhs_b
matrix lpmmarb23 = lpmmarb_consult_nhs_bei


* Matrices for probit conditional probability
matrix pprob1 = pprob_nhs1_nobirth_bei
matrix pprob2 = pprob_consult_nhs_bei
matrix pprob3 = pprob_hosp_bei
matrix pprob4 = pprob_consultant_bei

matrix pprobb1 = pprobb_nhs1_nobirth_bei
matrix pprobb2 = pprobb_consult_nhs_bei
matrix pprobb3 = pprobb_hosp_bei
matrix pprobb4 = pprobb_consultant_bei


* Matrices for lpm conditional probability
matrix lprob1 = lprob_nhs1_nobirth_bei
matrix lprob2 = lprob_consult_nhs_bei
matrix lprob3 = lprob_hosp_bei
matrix lprob4 = lprob_consultant_bei

matrix lprobb1 = lprobb_nhs1_nobirth_bei
matrix lprobb2 = lprobb_consult_nhs_bei
matrix lprobb3 = lprobb_hosp_bei
matrix lprobb4 = lprobb_consultant_bei
}


************************************************************************************************************************
* Now I save the matrices as datasets, so that they can be read by bstat
************************************************************************************************************************
{
	
local bootstrap_coef "b11 b12 b13 b14 b15 b21 b22 b23 b24 b25 b31 b32 b33 b41 b42 b43 "
foreach m of local bootstrap_coef {
	cd "$junk"
	preserve
	drop _all
	svmat double `m', names(coef)
	save "`m'.dta", replace
	restore
}
local bootstrap_coef_lpm "lpmb11 lpmb12 lpmb13 lpmb14 lpmb15 lpmb21 lpmb22 lpmb23 lpmb24 lpmb25 lpmb31 lpmb32 lpmb33 lpmb41 lpmb42 lpmb43 "
foreach m of local bootstrap_coef_lpm {
	cd "$junk"
	preserve
	drop _all
	svmat double `m', names(coef)
	save "`m'.dta", replace
	restore
}
local bootstrap_margins_probit "marginsb11 marginsb13 marginsb21 marginsb23 "
foreach m of local bootstrap_margins_probit {
	cd "$junk"
	preserve
	drop _all
	svmat double `m', names(coef)
	save "`m'.dta", replace
	restore
}
local bootstrap_margins_lpm "lpmmarb11 lpmmarb13 lpmmarb21 lpmmarb23 "
foreach m of local bootstrap_margins_lpm {
	cd "$junk"
	preserve
	drop _all
	svmat double `m', names(coef)
	save "`m'.dta", replace
	restore
}
local bootstrap_prob "pprobb1 pprobb2 pprobb3 pprobb4 lprobb1 lprobb2 lprobb3 lprobb4 "
foreach m of local bootstrap_prob {
	cd "$junk"
	preserve
	drop _all
	svmat double `m', names(coef)
	save "`m'.dta", replace
	restore
}

}


************************************************************************************************************************
* Reading bootstrap files from bstat
************************************************************************************************************************

{
cd "$junk"
*m is the first digit indicating the outcome variables
*n is the second digit indicating the regression identifier


* Probit & LPM coefficients tables
foreach m in 1 2 {
	foreach n in 1 2 3 4 5 {
	if `n' == 1 local app "replace"
	if `n' != 1 local app "append"
	bstat using "b`m'`n'.dta", stat(observed`m'`n')
	outreg2 using "severity_probit_coef_`m'", excel dec(3) asterisk(coef) bracket ctitle(`n') `app'
	* results of the probit bootstrap confidence interval
	bstat using "lpmb`m'`n'.dta", stat(lpmobserved`m'`n')
	outreg2 using "severity_lpm_coef_`m'", excel dec(3) asterisk(coef) bracket ctitle(`n') `app'
	* results of the LPM bootstrap confidence interval

	* end of n loop
	}
* end of m loop
}

foreach m in 3 4 {
	foreach n in 1 2 3 {
	if `n' == 1 local app "replace"
	if `n' != 1 local app "append"
	bstat using "b`m'`n'.dta", stat(observed`m'`n')
	outreg2 using "severity_probit_coef_`m'", excel dec(3) asterisk(coef) bracket ctitle(`n') `app'
	* results of the probit bootstrap confidence interval
	bstat using "lpmb`m'`n'.dta", stat(lpmobserved`m'`n')
	outreg2 using "severity_lpm_coef_`m'", excel dec(3) asterisk(coef) bracket ctitle(`n') `app'
	* results of the LPM bootstrap confidence interval

	* end of n loop
	}
* end of m loop
}



* Probit & LPM marginal effects tables
foreach m in 1 2 {
	foreach n in 1 3 {
	if `n' == 1 local app "replace"
	if `n' != 1 local app "append"
	bstat using "marginsb`m'`n'.dta", stat(marginsobserved`m'`n')
	outreg2 using "severity_probit_margins_`m'", excel dec(3) asterisk(coef) bracket ctitle(`n') `app'
	* results of the probit marginal effects bootstrap 
	bstat using "lpmmarb`m'`n'.dta", stat(lpmmar`m'`n')
	outreg2 using "severity_lpm_margins_`m'", excel dec(3) asterisk(coef) bracket ctitle(`n') `app'
	* results of the lpm marginal effects bootstrap 

	* end of n loop
	}
* end of m loop
}



* Conditional probability
foreach m in 1 2 3 4 {
	bstat using "pprobb`m'.dta", stat(pprob`m') level(90)
	matrix ci_normal = e(ci_normal)'
	outreg2 using "severity_probit_prob_`m'", excel dec(3) asterisk(coef) stat(coef se ci_normal ) sideway replace
	bstat using "lprobb`m'.dta", stat(lprob`m') level(90)
	matrix ci_normal = e(ci_normal)'
	outreg2 using "severity_lpm_prob_`m'", excel dec(3) asterisk(coef) stat(coef se ci_normal ) sideway replace
	}

}

log close
