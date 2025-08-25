

clear 


cap log close
log using "$log/results_without_bootstrap.txt", replace text

display "*******************************************************************************"
display "*******************************************************************************"
display "Results that DO NOT require bootstrap start here"
display "*******************************************************************************"
display "*******************************************************************************"

use "$datasets/dataset_cleaned.dta", clear

replace age=. if age==-9
replace age = age/100

 
*********************************************************************************
*Setting the PHI variables
*********************************************************************************

*I am defining pmi_type=1 as insurance obtained through the employer, it does not matter whether as fringe benefit or deducted through wages
gen pmi_type=1 if pmi_employer==1
replace pmi_type=1 if pmi_wages==1
replace pmi_type=3 if pmi_relative==1
replace pmi_type=4 if pmi_ind==1
replace pmi_type=5 if pmi_no==1

label variable pmi_type "PHI status of the individual"
label define lab_pmi_type  1 "Through Employer" 3 "Through relative" 4 "Individually bought" 5 "No PMI"
label values pmi_type lab_pmi_type



***************************************************************************************
*Setting the Education variables
***************************************************************************************

*Specify the smallest education level as the base level
fvset base 0 edlevel
gen edu45=1 if edlevel==4|edlevel==5
replace edu45=0 if edlevel<4&edlevel>=0



*-------------------------------------
*Defining the samples of interest
*-------------------------------------
*keep only observations with observed income and education
*This would be England, Wales, and Scotland
gen region_sample=1 if bigregion==1|bigregion==2|bigregion==3
gen ln_incomehh=ln(incomehh)
gen sample_interest = 1 if (bigregion==1|bigregion==2|bigregion==3) & edlevel != . & ln_incomehh != . & married_cohab != .


************************************************************************
*Taking into account childbirth hospitalisations
************************************************************************

gen nhs1_nobirth=nhs1
replace nhs1_nobirth=. if hospch==1
*We give missing to those women for which all the hospitalisations were due to childbirth


*************************************************************************
*Creating lag insurance variables
**************************************************************************

sort pid wave
gen pmi_lag1=pmi[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)

sort pid wave
gen pmi_type_lag1=pmi_type[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)
label values pmi_type_lag1 lab_pmi_type

tab pmi_type_lag1

gen pmi_employer_lag1=1 if pmi_type_lag1==1
replace pmi_employer_lag1=0 if pmi_type_lag1==4|pmi_type_lag1==3

gen pmi_relative_lag1=1 if pmi_type_lag1==3
replace pmi_relative_lag1=0 if pmi_type_lag1==1|pmi_type_lag1==4

gen pmi_ind_lag1=1 if pmi_type_lag1==4
replace pmi_ind_lag1=0 if pmi_type_lag1==1|pmi_type_lag1==3


*************************************************************************
*Creating a vble on whether they were employees the previous wave
**************************************************************************

sort pid wave
gen employee_lag1=employee[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)


**************************************************************************
*Globals for regressions
**************************************************************************

global var_pmi      "i.pmi_type"



global var_basic     " female c.age c.age#c.age married_cohab"
global var_basic_edu      " female c.age c.age#c.age married_cohab i.edlevel"
global var_basic_eduincome       "female c.age c.age#c.age  married_cohab i.edlevel c.ln_incomehh "
global var_basic_income       "female c.age c.age#c.age married_cohab c.ln_incomehh "


global var_fixed       "region_2-region_19 wave_2-wave_12 time_elapsed time_elapsed2 time_elapsed3 "
global var_fixed1      "region_2-region_19 wave_2-wave_12 "
global var_fixed_region "region_2-region_19 "
global var_fixed_wave 	"wave_2-wave_12 "
global var_fixed_time_elapsed "time_elapsed time_elapsed2 time_elapsed3 "


*Outcome variables
global var_output1	" nhs1_nobirth consult_nhs "
global var_output2	" hosp consultant "
global var_output3	" nhs1_nobirth consult_nhs hosp consultant "
global var_output4_1 "nhs1_nobirth"
global var_output4_2 "consult_nhs"
global var_output4_3 "hosp"
global var_output4_4 "consultant"


*******************************************************************************
*Computing interaction of comorbidities
*At the end, I do not use them, so I can DELETE it !!!!
******************************************************************************
foreach y in b c d e f g h i j k l m {
gen comorb_a`y'=hlprba*hlprb`y'
} 

foreach y in c d e f g h i j k l m {
gen comorb_b`y'=hlprbb*hlprb`y'
} 

foreach y in d e f g h i j k l m {
gen comorb_c`y'=hlprbc*hlprb`y'
} 


foreach y in e f g h i j k l m {
gen comorb_d`y'=hlprbd*hlprb`y'
} 


foreach y in f g h i j k l m {
gen comorb_e`y'=hlprbe*hlprb`y'
} 


foreach y in g h i j k l m {
gen comorb_f`y'=hlprbf*hlprb`y'
} 

foreach y in h i j k l m {
gen comorb_g`y'=hlprbg*hlprb`y'
} 


foreach y in i j k l m {
gen comorb_h`y'=hlprbh*hlprb`y'
} 

foreach y in j k l m {
gen comorb_i`y'=hlprbi*hlprb`y'
} 

foreach y in l m {
gen comorb_k`y'=hlprbk*hlprb`y'
} 

foreach y in m {
gen comorb_l`y'=hlprbl*hlprb`y'
} 

*************************************************************************************************************************
*Creating lag of illnesses, self-assessed health, and age
*************************************************************************************************************************

foreach y in a b c d e f g h i j k l m {
sort pid wave

gen hlprb`y'_lag1=hlprb`y'[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)
} 

sort pid wave
gen exc1_vpoor5_lag1=exc1_vpoor5[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)

sort pid wave
gen health_cond_num_lag1=health_cond_num[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)


sort pid wave
gen age_lag1=age[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)


************************************************************************************************************************
*Computing comorbidity indexes
************************************************************************************************************************
tab exc1_vpoor5
label variable exc1_vpoor5 "self-assessed health 1 = excellent, 5 = very poor"

gen age_delete=age
gen age2_delete=age*age

gen age_delete_lag1=age_lag1
gen age2_delete_lag1=age_lag1*age_lag1


oprobit exc1_vpoor5 hlprba-hlprbm age_delete age2_delete if sample_interest==1, cluster(pid)
predict severity_temp, index

sum severity_temp, detail
gen severity_1=(severity_temp-r(min))/(r(max)-r(min))
label variable severity_1 "comorbidity index [0,1] with weights computed using self-assessed health"
drop severity_temp
drop age_delete age2_delete

*Computing lag severity
sort pid wave
gen severity_1_lag1=severity_1[_n-1] if pid==pid[_n-1]&wave==(wave[_n-1]+1)
label variable severity_1_lag1 "lag one period comorbidity index [0,1] with weights computed using self-assessed health"
gen severity=severity_1


***************************************************************************************
*-------------------------------------------------------------------------------------
*Relation between illness severity and hospitalisation
*-------------------------------------------------------------------------------------
*************************************************************************************

gen health_cond_num2=health_cond_num*health_cond_num
gen health_cond_num2_lag1=health_cond_num_lag1*health_cond_num_lag1

******************************************************************
*Descriptive statistics
******************************************************************
cd "$junk"

label variable nhs1_nobirth "1 if hospitalisaitons were funded by the NHS, 0 if partially or fully privately funded "
label variable consult_nhs "1 if outpatient hospital consultation was funded by the NHS, 0 if partially or fully privately funded"
label variable female "1 if Female, 0 if male"
label variable age "Age/100(years/100)"
label variable married_cohab "1 if married or cohabitating"
label variable ln_incomehh "LN(household income in 2008)"
label variable pmi_employer_lag1 "PMI obtained through the employer"
label variable pmi_relative_lag1 "PMI obtained through a relative"
label variable pmi_ind_lag1 "PMI bought by themselves"

tab edlevel, gen(educ)
label variable educ1 "1 if no qualification or still in school, 0 otherwise"
label variable educ2 "1 if Compulsory Secondary Education, Commercial Qualification, or other qualification, 0 otherwis"
label variable educ3 "1 if General Certificate of Education (O-levels) or Appenticeship, 0 otherwise"
label variable educ4 "1 if General Certificate of Education (A-levels), 0 otherwise"
label variable educ5 "1 if Degree, Nursing, Teaching or Other Higher qualification, 0 otherwise "
label variable educ6 "1 if Higher degree (MSc, PhD, etc.), 0 otherwise "
global educ "educ1 educ2 educ3 educ4 educ5 educ6"

label variable time_elapsed "Months between interview date and date of reference of health care use questions"


quietly estpost sum consult_nhs age female $educ married_cohab ln_incomehh pmi_employer_lag1 pmi_ind_lag1 pmi_relative_lag1 time_elapsed  if sample_interest==1&pmi_lag1==1&(consult_nhs!=.)

estout using "Descriptive_Statistics_consul.xlm", replace cells("count mean sd") varwidth(21) label title("Descriptive Statistics") note("Note: sample includes individuals with private health insurance in the previous wave, who had at least one hospital outpatient consultation")

quietly estpost sum nhs1_nobirth age female $educ married_cohab ln_incomehh pmi_employer_lag1 pmi_ind_lag1 pmi_relative_lag1 time_elapsed if sample_interest==1&pmi_lag1==1&(nhs1_nobirth!=.)

estout using "Descriptive_Statistics_hosp.xlm", replace cells("count mean sd") varwidth(21) label title("Descriptive Statistics") note("Note: sample includes individuals with private health insurance in the previous wave, who had at least one hospitalisation")

*********************************************************************
*Plot of comorbidities index
*********************************************************************
cd "$figures" 

hist health_cond_num if sample_interest==1&pmi_lag1==1,  xtitle("number of comorbidities (histogram)") ytitle("  ") graphregion(fcolor(white))
graph copy density_num_cond, replace

kdensity  severity_1 if sample_interest==1&pmi_lag1==1,  xtitle("comorbidity index (kernel density)") ytitle("  ") title("") note("") graphregion(fcolor(white))
graph copy density_severity, replace

*graph combine density_num_cond density_severity,  title("Figure 6. Distribution of severity proxy measures", size(12pt)) note("Sample: privately insured individuals.") graphregion(fcolor(white))

graph combine density_num_cond density_severity,  title("") graphregion(fcolor(white))

graph save Figure_6, replace
cap graph export "$EMF/Figure_6.emf", replace
graph export "$EPS/Figure_6.eps", replace
graph export "$PNG/Figure_6.png", replace
graph export "$JPG/Figure_6.jpg", replace
graph export "$PS/Figure_6.ps", replace



*********************************************************************
*ANALYSIS
*********************************************************************


****************************
*Local Polynomial plots 
****************************
cd "$figures" 

*Local polynomial plots
*https://www.stata.com/manuals13/rlpoly.pdf
*https://www.stata.com/manuals13/g-2graphtwowaylpolyci.pdf#g-2graphtwowaylpolyci

/* Local Polynomial Plots using comorbidity index 1 */
*severity_1: index [0,1] with weights estimated using self-assessed health

sum severity_1 if sample_interest==1&pmi_lag1==1, d

lpoly nhs1_nobirth  severity_1 if sample_interest==1&pmi_lag1==1&severity_1<0.45, ci level(90) noscatter degree(1) kernel(epan2) ///
noscatter yrange(0.55(0.1)0.80) ylabel(0.6[0.1]0.8) xrange(0(0.1)0.45) xlabel(0[0.1]0.4) xtitle("comorbidity index") title("Hospitalisations") /// 
 ytitle("Prob. NHS-funded hospitalisation",height(7)) legend(label(2 "Local polynomial") position(6) col(1))  note(" ")  graphregion(fcolor(white))
graph copy nhs1_nobirth_severity, replace


lpoly consult_nhs  severity_1 if sample_interest==1&pmi_lag1==1&severity_1<0.45, ci level(90) noscatter degree(1) kernel(epan2) ///
noscatter yrange(0.55(0.1)0.80) ylabel(0.6[0.1]0.8) xrange(0(0.1)0.45) xlabel(0[0.1]0.4) xtitle("comorbidity index") title("Hospital outpatient visits") ///
 ytitle("Prob. NHS-funded hospital outpatient visit",height(7)) legend(label(2 "Local polynomial") position(6) col(1)) note(" ")  graphregion(fcolor(white))
graph copy consult_nhs_severity, replace


*grc1leg nhs1_nobirth_severity consult_nhs_severity, ycommon xcommon  title("Figure 8. Relation between the use of NHS services and the comorbidity index", size(12pt)) position(6) note("Sample: privately insured individuals with at least one hospitalisation (left) or one hospital outpatient consultation (right)",size(7pt))  graphregion(fcolor(white)) 

grc1leg nhs1_nobirth_severity consult_nhs_severity, ycommon xcommon  title("")  graphregion(fcolor(white)) 

graph save Figure_8, replace
graph export "$PNG/Figure_8.png", replace
graph export "$EPS/Figure_8.eps", replace
graph export "$PS/Figure_8.ps", replace
graph export "$JPG/Figure_8.jpg", replace

************************************************************
*Plots using directly the number of comorbidities with Confidence intervals
*************************************************************

*--------
*Graph for probability of NHS hospitalisaitons and comorbidities
*--------


preserve

gen health_cond_num_table=health_cond_num
replace health_cond_num_table=5 if health_cond_num>=5&health_cond_num<97
*This is 5 or more


version 16: table health_cond_num_table if sample_interest==1&pmi_lag1==1, c(mean nhs1_nobirth count nhs1_nobirth sd nhs1_nobirth) replace



ren table1 Prob_NHS_hosp
ren table2 Num_obs
ren table3 SD_NHS_hosp
ren health_cond_num_table num_comorbidities

label var Prob_NHS_hosp "Average"
label var num_comorbidities "Number of comorbidities"

*Confidence intervals
generate upper= Prob_NHS_hosp + invttail(Num_obs,0.05)*SD_NHS_hosp/sqrt(Num_obs)
generate lower= Prob_NHS_hosp - invttail(Num_obs,0.05)*SD_NHS_hosp/sqrt(Num_obs)

*Graph with line - NHS-funded hospitalisations
graph twoway (line Prob_NHS_hosp num_comorbidities) (rcap upper lower num_comorbidities), title("Comorbidities and NHS hospitalisations") ///
ytitle("Prob. NHS-funded hospitalisation") note("Sample are privately insured individuals with at least one hospitalisation") ///
legend(label(2 "90% Confidence Intervals"))  xlabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5+")  graphregion(fcolor(white))

*Same graph as above but to prepare it for the combine comand
graph twoway (line Prob_NHS_hosp num_comorbidities) (rcap upper lower num_comorbidities), title("Hospitalisations") ///
ytitle("Prob. NHS-funded hospitalisation", height(7)) legend(label(2 "90% CI"))  xlabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5+") graphregion(fcolor(white))
graph copy hosp_num_comorbidities, replace


drop num_comorbidities

restore



*--------
*Graph for NHS hospital consultations and comorbidities with confidence intervals
*--------


*This restore/preserve is necessary because above I am replacing the dataset with the Table, so I lose variables
preserve

gen health_cond_num_table=health_cond_num
replace health_cond_num_table=5 if health_cond_num>=5&health_cond_num<97
*This is 5 or more


version 16: table health_cond_num_table if sample_interest==1&pmi_lag1==1, c(mean consult_nhs count consult_nhs sd consult_nhs) replace
ren table1 Prob_NHS_consult
ren table2 Num_obs
ren table3 SD_consult_nhs
ren health_cond_num_table num_comorbidities

label var Prob_NHS_consult "Average"
label var num_comorbidities "Number of comorbidities"

*Confidence intervals
generate upper= Prob_NHS_consult + invttail(Num_obs,0.05)*SD_consult_nhs/sqrt(Num_obs)
generate lower= Prob_NHS_consult - invttail(Num_obs,0.05)*SD_consult_nhs/sqrt(Num_obs)

*Graph with line - Hospital outpatient visit
graph twoway (line Prob_NHS_consult num_comorbidities) (rcap upper lower num_comorbidities),  title("comorbidities and NHS hospital outpatient visit") ///
ytitle("Prob. NHS-funded hospital outpatient visit") note("Sample are privately insured individuals with at least one outpatient appointment") legend(label(2 "90% Confidence Intervals")) xlabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5+") graphregion(fcolor(white))


*Same graph as above but to prepare it for the combine comand
graph twoway (line Prob_NHS_consult num_comorbidities) (rcap upper lower num_comorbidities),  title("Hospital outpatient visits") ///
ytitle("Prob. NHS-funded hospital outpatient visit", height(7)) legend(label(2 "90% CI")) xlabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5+") graphregion(fcolor(white))
graph copy cons_num_comorbidities, replace


*grc1leg hosp_num_comorbidities cons_num_comorbidities, ycommon xcommon title("Figure 7. Relation between the use of NHS services and the number of comorbidities", size(12pt)) position(6)  note("Sample: privately insured individuals with at least one hospitalisation (left) or one hospital outpatient consultation (right)",size(7pt))  graphregion(fcolor(white))


grc1leg hosp_num_comorbidities cons_num_comorbidities, ycommon xcommon title("")  graphregion(fcolor(white))

graph save Figure_7, replace
graph export "$PNG/Figure_7.png", replace
graph export "$EPS/Figure_7.eps", replace
graph export "$PS/Figure_7.ps", replace
graph export "$JPG/Figure_7.jpg", replace


cd "$figures" 
restore 



*************************************************************************************************************************
*Here we do the instrumental variables analysis (as requested by referee)
***********************************************************************************************************************
cd "$tables" 

gen exc1_vpoor5_2=exc1_vpoor5*exc1_vpoor5


********** Hospitalisations with Basic Regressors

*First Stages

regress exc1_vpoor5 health_cond_num c.health_cond_num#c.health_cond_num  $var_basic $var_fixed if sample_interest==1&pmi_lag1==1&nhs1_nobirth!=., cluster(pid)
testparm health_cond_num c.health_cond_num#c.health_cond_num 
local Fstat=r(F)

regress exc1_vpoor5_2 health_cond_num c.health_cond_num#c.health_cond_num  $var_basic $var_fixed if sample_interest==1&pmi_lag1==1&nhs1_nobirth!=., cluster(pid)
testparm health_cond_num c.health_cond_num#c.health_cond_num 
local Fstat_sq=r(F)

*Second stage

ivregress 2sls nhs1_nobirth $var_basic $var_fixed ( exc1_vpoor5 exc1_vpoor5_2 = health_cond_num c.health_cond_num#c.health_cond_num ) if sample_interest==1&pmi_lag1==1, cluster(pid)
display _b[exc1_vpoor5]+_b[exc1_vpoor5_2]
display _b[exc1_vpoor5]*2+_b[exc1_vpoor5_2]*(2^2)
display _b[exc1_vpoor5]*3+_b[exc1_vpoor5_2]*(3^2)
display _b[exc1_vpoor5]*4+_b[exc1_vpoor5_2]*(4^2)
display _b[exc1_vpoor5]*5+_b[exc1_vpoor5_2]*(5^2)

local minimum=-_b[exc1_vpoor5]/(2*_b[exc1_vpoor5_2])
display "minimum: " `minimum'

sum nhs1_nobirth if e(sample)

outreg2  using "iv_strategy", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)', F stat linear term,`Fstat', F stat quadratic term,`Fstat_sq') nolabel dec(3) bracket ctitle("Hospitalisations, Basic") keep(exc1_vpoor5 exc1_vpoor5 exc1_vpoor5 exc1_vpoor5_2 $var_basic $var_fixed_time_elapsed) replace excel

********** Hospitalisations with Basic Regressors + Income +Edu


*First Stages

regress exc1_vpoor5 health_cond_num c.health_cond_num#c.health_cond_num  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1&nhs1_nobirth!=., cluster(pid)
testparm health_cond_num c.health_cond_num#c.health_cond_num 
local Fstat=r(F)

regress exc1_vpoor5_2 health_cond_num c.health_cond_num#c.health_cond_num  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1&nhs1_nobirth!=., cluster(pid)
testparm health_cond_num c.health_cond_num#c.health_cond_num 
local Fstat_sq=r(F)

*Second stage

ivregress 2sls nhs1_nobirth $var_basic_eduincome $var_fixed ( exc1_vpoor5 exc1_vpoor5_2 = health_cond_num c.health_cond_num#c.health_cond_num ) if sample_interest==1&pmi_lag1==1, cluster(pid)
display _b[exc1_vpoor5]+_b[exc1_vpoor5_2]
display _b[exc1_vpoor5]*2+_b[exc1_vpoor5_2]*(2^2)
display _b[exc1_vpoor5]*3+_b[exc1_vpoor5_2]*(3^2)
display _b[exc1_vpoor5]*4+_b[exc1_vpoor5_2]*(4^2)
display _b[exc1_vpoor5]*5+_b[exc1_vpoor5_2]*(5^2)

local minimum=-_b[exc1_vpoor5]/(2*_b[exc1_vpoor5_2])
display "minimum: " `minimum'

sum nhs1_nobirth if e(sample)

outreg2  using "iv_strategy", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)', F stat linear term,`Fstat', F stat quadratic term,`Fstat_sq') nolabel dec(3) bracket ctitle("Hospitalisations, Basic + Edu + Income") keep(exc1_vpoor5 exc1_vpoor5 exc1_vpoor5 exc1_vpoor5_2 $var_basic_eduincome $var_fixed_time_elapsed) append excel


********** Consultations with Basic Regressors

*First Stages

regress exc1_vpoor5 health_cond_num c.health_cond_num#c.health_cond_num  $var_basic $var_fixed if sample_interest==1&pmi_lag1==1&consult_nhs!=., cluster(pid)
testparm health_cond_num c.health_cond_num#c.health_cond_num 
local Fstat=r(F)

regress exc1_vpoor5_2 health_cond_num c.health_cond_num#c.health_cond_num  $var_basic $var_fixed if sample_interest==1&pmi_lag1==1&consult_nhs!=., cluster(pid)
testparm health_cond_num c.health_cond_num#c.health_cond_num 
local Fstat_sq=r(F)

*Second stage

ivregress 2sls consult_nhs $var_basic $var_fixed ( exc1_vpoor5 exc1_vpoor5_2 = health_cond_num c.health_cond_num#c.health_cond_num ) if sample_interest==1&pmi_lag1==1, cluster(pid)
display _b[exc1_vpoor5]+_b[exc1_vpoor5_2]
display _b[exc1_vpoor5]*2+_b[exc1_vpoor5_2]*(2^2)
display _b[exc1_vpoor5]*3+_b[exc1_vpoor5_2]*(3^2)
display _b[exc1_vpoor5]*4+_b[exc1_vpoor5_2]*(4^2)
display _b[exc1_vpoor5]*5+_b[exc1_vpoor5_2]*(5^2)

local minimum=-_b[exc1_vpoor5]/(2*_b[exc1_vpoor5_2])
display "minimum: " `minimum'

sum consult_nhs if e(sample)

outreg2  using "iv_strategy", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)', F stat linear term,`Fstat', F stat quadratic term,`Fstat_sq') nolabel dec(3) bracket ctitle("Consult_nhs, Basic") keep(exc1_vpoor5 exc1_vpoor5 exc1_vpoor5 exc1_vpoor5_2 $var_basic $var_fixed_time_elapsed) append excel

********** Consultations with Basic Regressors + Education + Income


*First Stage

regress exc1_vpoor5 health_cond_num c.health_cond_num#c.health_cond_num  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1&consult_nhs!=., cluster(pid)
testparm health_cond_num c.health_cond_num#c.health_cond_num 
local Fstat=r(F)

regress exc1_vpoor5_2 health_cond_num c.health_cond_num#c.health_cond_num  $var_basic_eduincome $var_fixed if sample_interest==1&pmi_lag1==1&consult_nhs!=., cluster(pid)
testparm health_cond_num c.health_cond_num#c.health_cond_num 
local Fstat_sq=r(F)

*Second stage

ivregress 2sls consult_nhs $var_basic_eduincome $var_fixed ( exc1_vpoor5 exc1_vpoor5_2 = health_cond_num c.health_cond_num#c.health_cond_num ) if sample_interest==1&pmi_lag1==1, cluster(pid)
display _b[exc1_vpoor5]+_b[exc1_vpoor5_2]
display _b[exc1_vpoor5]*2+_b[exc1_vpoor5_2]*(2^2)
display _b[exc1_vpoor5]*3+_b[exc1_vpoor5_2]*(3^2)
display _b[exc1_vpoor5]*4+_b[exc1_vpoor5_2]*(4^2)
display _b[exc1_vpoor5]*5+_b[exc1_vpoor5_2]*(5^2)

local minimum=-_b[exc1_vpoor5]/(2*_b[exc1_vpoor5_2])
display "minimum: " `minimum'

sum consult_nhs if e(sample)

outreg2  using "iv_strategy", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)', F stat linear term,`Fstat', F stat quadratic term,`Fstat_sq') nolabel dec(3) bracket ctitle("consult_nhs, Basic + Edu + Income") keep(exc1_vpoor5 exc1_vpoor5 exc1_vpoor5 exc1_vpoor5_2 $var_basic_eduincome $var_fixed_time_elapsed) append excel


***********************************************************************************************************************
* Interaction with a binary variable pmi_employer
***********************************************************************************************************************
cd "$junk"

preserve

tab pmi_type_lag1 if sample_interest==1&pmi_lag1==1&employee_lag1==1&consult_nhs!=.	
keep if pmi_type_lag1==1|pmi_type_lag1==4
keep if employee_lag1==1

	*------------------------------
	* Number of comorbidities
	*------------------------------
	
	probit nhs1_nobirth c.health_cond_num c.health_cond_num#c.health_cond_num i.pmi_employer_lag1 c.health_cond_num#i.pmi_employer_lag1 c.health_cond_num#c.health_cond_num#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed  if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum nhs1_nobirth if e(sample)
	outreg2  using "pmi_e_cond_num_coef", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Probit Hosp. All controls") keep(c.health_cond_num c.health_cond_num#c.health_cond_num i.pmi_employer_lag1  c.health_cond_num#i.pmi_employer_lag1 c.health_cond_num#c.health_cond_num#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_time_elapsed) replace excel



	reg nhs1_nobirth c.health_cond_num c.health_cond_num#c.health_cond_num i.pmi_employer_lag1  c.health_cond_num#i.pmi_employer_lag1 c.health_cond_num#c.health_cond_num#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed  if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum nhs1_nobirth if e(sample)
	outreg2  using "pmi_e_cond_num_coef", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("LPM Hosp. All controls") keep(health_cond_num c.health_cond_num#c.health_cond_num i.pmi_employer_lag1  c.health_cond_num#i.pmi_employer_lag1 c.health_cond_num#c.health_cond_num#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_time_elapsed) append excel

	
	probit consult_nhs c.health_cond_num c.health_cond_num#c.health_cond_num i.pmi_employer_lag1 c.health_cond_num#i.pmi_employer_lag1 c.health_cond_num#c.health_cond_num#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed  if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum consult_nhs if e(sample)
	outreg2  using "pmi_e_cond_num_coef", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Probit Consult. All controls") keep(c.health_cond_num c.health_cond_num#c.health_cond_num i.pmi_employer_lag1  c.health_cond_num#i.pmi_employer_lag1 c.health_cond_num#c.health_cond_num#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_time_elapsed) append excel

	
	reg consult_nhs c.health_cond_num c.health_cond_num#c.health_cond_num i.pmi_employer_lag1  c.health_cond_num#i.pmi_employer_lag1 c.health_cond_num#c.health_cond_num#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed  if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum consult_nhs if e(sample)
	outreg2  using "pmi_e_cond_num_coef", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("LPM Consult. All controls") keep(health_cond_num c.health_cond_num#c.health_cond_num i.pmi_employer_lag1  c.health_cond_num#i.pmi_employer_lag1 c.health_cond_num#c.health_cond_num#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_time_elapsed) append excel	

	
	*------------------------------
	* Severity index
	*------------------------------
	
	probit nhs1_nobirth c.severity c.severity#c.severity i.pmi_employer_lag1 c.severity#i.pmi_employer_lag1 c.severity#c.severity#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed  if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum nhs1_nobirth if e(sample)
	outreg2  using "pmi_e_severity_coef", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Probit Hosp. All controls") keep(c.severity c.severity#c.severity i.pmi_employer_lag1  c.severity#i.pmi_employer_lag1 c.severity#c.severity#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_time_elapsed) replace excel


	reg nhs1_nobirth c.severity c.severity#c.severity i.pmi_employer_lag1  c.severity#i.pmi_employer_lag1 c.severity#c.severity#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed  if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum nhs1_nobirth if e(sample)
	outreg2  using "pmi_e_severity_coef", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("LPM Hosp. All controls") keep(severity c.severity#c.severity i.pmi_employer_lag1  c.severity#i.pmi_employer_lag1 c.severity#c.severity#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_time_elapsed) append excel

	
	probit consult_nhs c.severity c.severity#c.severity i.pmi_employer_lag1 c.severity#i.pmi_employer_lag1 c.severity#c.severity#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed  if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum consult_nhs if e(sample)
	outreg2  using "pmi_e_severity_coef", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Probit Consult. All controls") keep(c.severity c.severity#c.severity i.pmi_employer_lag1  c.severity#i.pmi_employer_lag1 c.severity#c.severity#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_time_elapsed) append excel

	
	reg consult_nhs c.severity c.severity#c.severity i.pmi_employer_lag1  c.severity#i.pmi_employer_lag1 c.severity#c.severity#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed  if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum consult_nhs if e(sample)
	outreg2  using "pmi_e_severity_coef", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("LPM Consult. All controls") keep(severity c.severity#c.severity i.pmi_employer_lag1  c.severity#i.pmi_employer_lag1 c.severity#c.severity#i.pmi_employer_lag1 $var_basic_eduincome $var_fixed_time_elapsed) append excel		

	
	
restore


************************************************************************************************************************
*Outcome variables
global var_output1	" nhs1_nobirth consult_nhs "
global var_output2	" hosp consultant "
global var_output3	" nhs1_nobirth consult_nhs hosp consultant "
global var_output4_1 "nhs1_nobirth"
global var_output4_2 "consult_nhs"
global var_output4_3 "hosp"
global var_output4_4 "consultant"



***********************************************
*Regressions using Number of comorbidities
************************************************

cd "$junk"




foreach Y of global var_output1 {
		

cd "$junk"
	
	*----------------------------------------------------------------------------------------------------------------------
	* Without education or household incomehh
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_probit_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic") title("Relation between No. of comorbidities and `Y'. Probit coefficients") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic $var_fixed_time_elapsed) replace excel

	margins, dydx(health_cond_num) at(health_cond_num = (0(1)6)) post
	outreg2 using "cond_num_probit_margin_`Y'", dec(3) bracket ctitle("Basic") title("Marginal effects (number of comorbidities, outcome variable: `Y')") replace excel
	
	reg `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_lpm_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic") title("Relation between No. of comorbidities and `Y'. LPM coefficients") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic $var_fixed_time_elapsed) replace excel
	
	margins, dydx(health_cond_num) at(health_cond_num = (0(1)6)) post
	outreg2 using "cond_num_lpm_margin_`Y'", dec(3) bracket ctitle("Basic") title("Marginal effects (number of comorbidities, outcome variable: `Y')") replace excel

	
	*----------------------------------------------------------------------------------------------------------------------
	* With education but no household incomehh
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic_edu $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_probit_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic_edu $var_fixed_time_elapsed) append excel
	
	reg `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic_edu $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_lpm_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic_edu $var_fixed_time_elapsed) append excel
		
	
	*----------------------------------------------------------------------------------------------------------------------
	* With education and household incomehh
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_probit_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education + Income") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_time_elapsed) append excel

	margins, dydx(health_cond_num) at(health_cond_num = (0(1)6)) post
	outreg2 using "cond_num_probit_margin_`Y'", dec(3) bracket ctitle("Basic + Education + Income") append excel
	
	probit `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	margins, at(health_cond_num = (0(1)6)) level(90) post

	global var_output1	" nhs1_nobirth consult_nhs "
	


	reg `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_lpm_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education + Income") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_time_elapsed) append excel
	
	margins, dydx(health_cond_num) at(health_cond_num = (0(1)6)) post
	outreg2 using "cond_num_lpm_margin_`Y'", dec(3) bracket ctitle("Basic + Education + Income") append excel
	
	

	
	*----------------------------------------------------------------------------------------------------------------------
	* With education and household incomehh, 3rd-polynomial
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' health_cond_num c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_probit_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education + Income (3rd-polynomial)") keep(health_cond_num c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_time_elapsed) append excel
	
	reg `Y' health_cond_num c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_lpm_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education + Income (3rd-polynomial)") keep(health_cond_num c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_time_elapsed) append excel
		
		
	*----------------------------------------------------------------------------------------------------------------------
	* With education and household incomehh, 4th-polynomial
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' health_cond_num c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_probit_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education + Income (4th-polynomial)") keep(health_cond_num c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_time_elapsed) append excel
	
	reg `Y' health_cond_num c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	local minimum=-_b[health_cond_num]/(2*_b[c.health_cond_num#c.health_cond_num])
	display "minimum: " `minimum'
	sum `Y' if e(sample)
	outreg2  using "cond_num_lpm_coef_`Y'", addstat(Minimum U shape, `minimum', Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education + Income (4th-polynomial)") keep(health_cond_num c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num c.health_cond_num#c.health_cond_num#c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_time_elapsed) append excel
		
	
	}

	
foreach Y of global var_output2 {


	*----------------------------------------------------------------------------------------------------------------------
	* Without education or household incomehh
	*----------------------------------------------------------------------------------------------------------------------
	cd "$junk"
	probit `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum `Y' if e(sample)
	outreg2  using "cond_num_probit_coef_`Y'", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic") title("Relation between No. of comorbidities and `Y'. Probit coefficients") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic $var_fixed_time_elapsed) replace excel
	
	reg `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum `Y' if e(sample)
	outreg2  using "cond_num_lpm_coef_`Y'", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic") title("Relation between No. of comorbidities and `Y'. LPM coefficients") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic $var_fixed_time_elapsed) replace excel

	
	*----------------------------------------------------------------------------------------------------------------------
	* With education but no household incomehh
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic_edu $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum `Y' if e(sample)
	outreg2  using "cond_num_probit_coef_`Y'", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic_edu $var_fixed_time_elapsed) append excel
	
	reg `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic_edu $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum `Y' if e(sample)
	outreg2  using "cond_num_lpm_coef_`Y'", addstat( Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic_edu $var_fixed_time_elapsed) append excel
	
	
	*----------------------------------------------------------------------------------------------------------------------
	* With education and household incomehh
	*----------------------------------------------------------------------------------------------------------------------
	probit `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)

	sum `Y' if e(sample)
	outreg2  using "cond_num_probit_coef_`Y'", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education + Income") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_time_elapsed) append excel
	

	
	reg `Y' health_cond_num c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_region $var_fixed_wave $var_fixed_time_elapsed if sample_interest==1&pmi_lag1==1, cluster(pid)
	sum `Y' if e(sample)
	outreg2  using "cond_num_lpm_coef_`Y'", addstat(Average dep. vble, `r(mean)') nolabel dec(3) bracket ctitle("Basic + Education + Income") keep(health_cond_num c.health_cond_num#c.health_cond_num $var_basic_eduincome $var_fixed_time_elapsed) append excel	
	
	}

********************************************************************************
log close






