
clear matrix
cap log close
clear
set mat 600
set memory 400m
set more off
log using "$log/clean_dataset.log", text replace
set linesize 80

use "$datasets/dataset_not_cleaned.dta", clear



***********************************************************************
*Because of the reshape there are lots of empty observations
*We eliminate them by
drop if age==.



*************************************************************************
*Computing time elapsed between Date of interview and reference date for health care use questions
*Notice that the questions on health care utilization have a reference data of 1st Sept 

gen time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,1995) if wave==6
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,1996) if wave==7
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,1997) if wave==8
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,1998) if wave==9

replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,1999) if wave==10
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,2000) if wave==11
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,2001) if wave==12
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,2002) if wave==13

replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,2003) if wave==14
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,2004) if wave==15
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,2005) if wave==16
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,2006) if wave==17
replace time_elapsed=mdy(month_i,day_i,year_i)-mdy(9,1,2007) if wave==18

replace time_elapsed=time_elapsed/30.25
label variable time_elapsed "Months between interview date and date of reference of health care use questions"
gen time_elapsed2=time_elapsed*time_elapsed
gen time_elapsed3=time_elapsed2*time_elapsed
***********************************************************************

tab month, gen(month_)
gen jan_march=1 if month_1==1|month_2==1|month_3==1
replace jan_march=0 if month_1==0&month_2==0&month_3==0

gen apr_may=1 if month_4==1|month_5==1
replace apr_may=0 if month_4==0&month_5==0

*JUne and July does not exist in the data
gen ago_sept=1 if month_6==1|month_7==1
replace ago_sept=0 if month_6==0&month_7==0

gen oct_dec=1 if month_8==1|month_9==1|month_10==1
replace oct_dec=0 if month_8==0&month_9==0&month_10==0

*******************************************************
*Dropping variables with many inapplicable
drop race plbornc sctype school aidhh aidhrs

*******************************************************
*Building Income Variables

label variable remth "Monthly Indiv Labour Income measured in Dec 2008 £ (divided by 10000)"
gen ln_earnings=ln(remth)
label variable ln_earnings "LN of remth"

*******************************************************
*Building Job status

replace jbhrs=. if jbhrs<0

*Job status
cap drop employee
gen employee=1 if jbstat==2
replace employee=0 if jbstat==1|(jbstat>=3&jbstat!=.)
********************************************************
*Building basic demographics

*Coding female
gen female=0 if sex==1
replace female=1 if sex==2

********************************
*Health

gen exc1_vpoor5=hlstat
replace exc1_vpoor5=. if hlstat<1|hlstat>5

gen excellent=1 if hlstat==1
replace excellent=0 if hlstat==2|hlstat==3|hlstat==4|hlstat==5

gen good=1 if hlstat==2
replace good=0 if hlstat==1|hlstat==3|hlstat==4|hlstat==5

gen fair=1 if hlstat==3
replace fair=0 if hlstat==1|hlstat==2|hlstat==4|hlstat==5

gen poor_v_poor=1 if hlstat==4|hlstat==5
replace poor_v_poor=0 if hlstat==1|hlstat==2|hlstat==3

gen excellent_good=1 if hlstat==1|hlstat==2
replace excellent_good=0 if hlstat==3|hlstat==4|hlstat==5

gen fair_v_bad=1 if hlstat==3|hlstat==4|hlstat==5
replace fair_v_bad=0 if hlstat==1|hlstat==2


gen ghq1=hlghq1
replace ghq1=. if hlghq1<1

gen ghq2=hlghq2
replace ghq2=. if hlghq2<0


gen health_imp=lfimpa
replace health_imp=. if lfimpa<1|lfimpa>10
label variable health_imp "How important is health for you: 1 no at all, 10 very important"


gen health_veryimp=1 if lfimpa==10
replace health_veryimp=0 if lfimpa>0&lfimpa<10
label variable health_veryimp "1 if health is very important for the resp, 0 otherwise"

tab lfimpa
tab lfimpa if lfimpa>0&lfimpa<11
tab health_imp
tab health_veryimp


gen health_worsen=hlsf10c
replace health_worsen=. if hlsf10c<1|hlsf10c>5
label variable health_worsen "I expect health to worsens: 1 def. true, 5 def. false"



*******************************************************
*Health Care Use 
****************************************************


*Coding whether saw consultant:
tab hlsvl 
ren hlsvl consultant
replace consultant=. if consultant<0
label variable consultant "1 if saw consultant since september last year, 0 otherwise"

*Coding hospitalizations
replace hosp=. if hosp<1
replace hosp=2-hosp
label variable hosp "1 if suffered hospitalization not related to childbirth, 0 othw"



*Hospitalization in the NHS or Privately

gen nhs1=1 if hospnhs==1
replace nhs1=0 if hospnhs==2|hospnhs==3
label variable nhs1 "1 if HOSP fully NHS, 0 if HOSP private or mixed funded. Conditional on Hospitalization"


*Consultant in the NHS or Privately

gen consult_nhs=1 if hlsvln==1
replace consult_nhs=0 if hlsvln==2|hlsvln==3
label variable consult_nhs "1 if Consultant fully NHS funded, 0 if private funded or both. Conditional seeing consultant"


*-----------------------------------------
*Private health insurance variables
*-----------------------------------------

*Private insurance variables

gen pmi=1 if hlcvr>0&hlcvr<3
replace pmi=0 if hlcvr==3
label variable pmi "1 if individual has private medical insurance, 0 othw"


*private insurance in own name is hlcvr==1

*private medical insurance paid directly by person
gen pmi_ind=1 if hlcvrh==1
replace pmi_ind=0 if hlcvrh==2|hlcvrh==3
replace pmi_ind=0 if hlcvr==2
replace pmi_ind=0 if pmi==0

*private medical insurance deducted from wages
gen pmi_wages=1 if hlcvrh==2
replace pmi_wages=0 if hlcvrh==1|hlcvrh==3
replace pmi_wages=0 if hlcvr==2
replace pmi_wages=0 if pmi==0

*private medical insurance paid by employer
gen pmi_employer=1 if hlcvrh==3
replace pmi_employer=0 if hlcvrh==1|hlcvrh==2
replace pmi_employer=0 if hlcvr==2 
replace pmi_employer=0 if pmi==0

*private medical insurance from relative
gen pmi_relative=1 if hlcvr==2 
replace pmi_relative=0 if hlcvr==1|hlcvr==3


*no private medical insurance
gen pmi_no=1 if pmi==0
replace pmi_no=0 if pmi==1


label variable pmi_ind "1 if indv has pmi and it is paid directly, 0 othw (inc. no insurance)"
label variable pmi_wages "1 if indv has pmi and it is deducted from wages, 0 othw (inc. no insurance)"
label variable pmi_employer "1 if indv has pmi paid by employer, 0 othw (inc. no insurance)"
label variable pmi_relative "1 if indv has pmi through relative, 0 othw (inc. no insurance)"
label variable pmi_no       "1 if indv has no pmi, 0 if he has pmi"




*-----------------------------------------
*Other variables
*-----------------------------------------

gen hidwave=0
replace hidwave=600000000 if wave==6
replace hidwave=700000000 if wave==7
replace hidwave=800000000 if wave==8
replace hidwave=900000000 if wave==9
replace hidwave=1000000000 if wave==10
replace hidwave=1100000000 if wave==11
replace hidwave=1200000000 if wave==12
replace hidwave=1300000000 if wave==13
replace hidwave=1400000000 if wave==14
replace hidwave=1500000000 if wave==15
replace hidwave=1600000000 if wave==16
replace hidwave=1700000000 if wave==17
replace hidwave=1800000000 if wave==18

replace hidwave=hidwave+hid


*creating dummies for waves
tab wave, gen(wave_)

*creating dummies for regions
replace region=. if region<1
tab region, gen(region_)




*----------------------------
*Education -one definition
*----------------------------
gen edu1=1 if qfachi>4&qfachi!=.
replace edu1=0 if qfachi<5&qfachi>0
label variable edu1 "O-levels or less"

gen edu2=1 if qfachi==3|qfachi==4
replace edu2=0 if (qfachi<3&qfachi>0)|(qfachi>4&qfachi!=.)
label variable edu2 "A-level or teaching"

gen edu3=1 if qfachi<3&qfachi>0
replace edu3=0 if qfachi>2&qfachi!=.
drop qfachi
gen edu=edu1+2*edu2+3*edu3

order edu edu1 edu2 edu3

*-----------------------------
*Education -another definition
*-----------------------------
gen 	  edlevel=5 if qfedhi == 1
replace edlevel=4 if qfedhi == 2|qfedhi == 3|qfedhi == 4|qfedhi == 5
replace edlevel=3 if qfedhi == 6
replace edlevel=2 if qfedhi == 7|qfedhi == 10
replace edlevel=1 if qfedhi == 8|qfedhi == 9|qfedhi == 11
replace edlevel=0 if qfedhi == 12|qfedhi == 13

tab edlevel, gen(edlevel_)



*---------------------------------------
*Industry
*---------------------------------------
*We do the following because different classification of industries are used for different waves
des jbsic jbsic92

replace jbsic=. if jbsic<0
replace jbsic92=. if jbsic92<0

*Industry using SIC80
gen jbsic_f=int(jbsic/1000)
tab jbsic_f, gen(ind80_)
replace ind80_2=1 if ind80_1==1
drop ind80_1
*We put together Agriculture, Forestry, Fishing with Energy & Water Supplies
*Otherwise, some observations drop in hosp_private1 & hosp_private2

*Industry using SIC92
*http://www.statistics.gov.uk/methods_quality/sic/contents.asp
gen jbsic92_f=int(jbsic92/100)


gen ind92_a=(jbsic92_f==1|jbsic92_f==2)
replace ind92_a=. if jbsic92_f==.

gen ind92_b=(jbsic92_f==5)
replace ind92_b=. if jbsic92_f==.

gen ind92_c=(jbsic92_f>=10&jbsic92_f<=14)
replace ind92_c=. if jbsic92_f==.

gen ind92_d=(jbsic92_f>=15&jbsic92_f<=37)
replace ind92_d=. if jbsic92_f==.

gen ind92_e=(jbsic92_f==40|jbsic92_f==41)
replace ind92_e=. if jbsic92_f==.

gen ind92_f=(jbsic92_f==45)
replace ind92_f=. if jbsic92_f==.

gen ind92_g=(jbsic92_f>=50&jbsic92_f<=52)
replace ind92_g=. if jbsic92_f==.

gen ind92_h=(jbsic92_f==55)
replace ind92_h=. if jbsic92_f==.

gen ind92_i=(jbsic92_f>=60&jbsic92_f<=64)
replace ind92_i=. if jbsic92_f==.

gen ind92_j=(jbsic92_f>=65&jbsic92_f<=67)
replace ind92_j=. if jbsic92_f==.

gen ind92_k=(jbsic92_f>=70&jbsic92_f<=74)
replace ind92_k=. if jbsic92_f==.

gen ind92_l=jbsic92_f==75
replace ind92_l=. if jbsic92_f==.

gen ind92_m=(jbsic92_f==80)
replace ind92_m=. if jbsic92_f==.

gen ind92_n=(jbsic92_f==85)
replace ind92_n=. if jbsic92_f==.

gen ind92_o=(jbsic92_f>=90&jbsic92_f<=93)
replace ind92_o=. if jbsic92_f==.

gen ind92_p=(jbsic92_f==95)
replace ind92_p=. if jbsic92_f==.

gen ind92_q=(jbsic92_f==99)
replace ind92_q=. if jbsic92_f==.


gen ind92_abcde=ind92_a+ind92_b+ind92_c+ind92_d+ind92_e
gen ind92_gh=ind92_g+ind92_h
gen ind92_lq=ind92_l+ind92_q
gen ind92_mn=ind92_m+ind92_n
gen ind92_op=ind92_o+ind92_p

drop ind92_a ind92_b ind92_c ind92_d ind92_g ind92_h ind92_e ind92_l ind92_m ind92_n ind92_q ind92_p ind92_o

order ind92_abcde ind92_f ind92_gh ind92_i ind92_j ind92_k ind92_lq ind92_mn ind92_op 
sum jbsic92 ind92_*

gen jbsic92_grouped=ind92_abcde+2*ind92_f+3*ind92_gh+4*ind92_i+5*ind92_j+6*ind92_k+7*ind92_lq+8*ind92_mn+9*ind92_op


gen borra80=ind80_1
gen borra92=ind92_f

*We are filling with zeros for those years in which the variable was not defined because a different classification
*was being used

foreach y of varlist ind80_2-ind80_10 {
replace `y'=0 if `y'==.&borra92!=.&(year>=2002&year<3000)
}

foreach y of varlist ind92_ab-ind92_op {
replace `y'=0 if `y'==.&borra80!=.&(year<2002)
}
drop borra80 borra92 

*Only 1050 observations
drop if ind92_abcde==.& ind80_2!=.



*-----------------------------------------------------------------------
*Health Problems
*You can call them by hlprb?
*------------------------------------------------------------------------
replace hlprba=. if hlprba<0
label variable hlprba "1 if problem with arms, legs, hands..."

replace hlprbb=. if hlprbb<0
label variable hlprbb "1 if problem with sight "

replace hlprbc=. if hlprbc<0
label variable hlprbc "1 if problem with hearing "

replace hlprbd=. if hlprbd<0
label variable hlprbd "1 if problem with skin conditions"

replace hlprbe=. if hlprbe<0
label variable hlprbe "1 if problem with chest, breathing"

replace hlprbf=. if hlprbf<0
label variable hlprbf "1 if problem with heart/blood pressure"

replace hlprbg=. if hlprbg<0
label variable hlprbg "1 if problem with stomach, digestion"

replace hlprbh=. if hlprbh<0
label variable hlprbh "1 if problem with diabetes"

replace hlprbi=. if hlprbi<0
label variable hlprbi "1 if problem with anxiety, depression"

replace hlprbj=. if hlprbj<0
label variable hlprbj "1 if problem with alcohol or drug"

replace hlprbk=. if hlprbk<0
label variable hlprbk "1 if problem with epilepsy"

replace hlprbl=. if hlprbl<0
label variable hlprbl "1 if problem with migraine"

*The "Other" category must include cancer and stroke that are only available in the last waves
replace hlprbm=. if hlprbm<0
replace hlprbm=1 if hlprbn==1
replace hlprbm=1 if hlprbo==1
label variable hlprbm "1 if any other health problem"
drop hlprbo hlprbn
order hlprb?

gen health_cond_num=hlprba+hlprbb+hlprbc+hlprbd+hlprbe+hlprbf+hlprbg+hlprbh+hlprbi+hlprbj+hlprbk+hlprbl+hlprbm
label variable health_cond_num "number of health problems"



*-------------------------------------------------------------------
*Other variables
*-----------------------------------------------------------------

*1 is married, 2 is living as a couple, 3 widowed, 4 divorced, 5 separated, 6 never married, 0 under 19, -1 not answered
gen married=mlstat==1
replace married=. if mlstat<0

gen married_cohab=(mlstat==1|mlstat==2)
replace married_cohab=. if mlstat<0


*----------------------------------------------------------------------------------
*Regions
*---------------------------------------------------------------------------------

gen england=1 if region<17
replace england=0 if region>16&region!=.

gen wales=1 if region==17
replace wales=0 if region!=17&region!=.

gen scotland=1 if region==18
replace scotland=0 if region!=18&region!=.

gen north_ir=1 if region==19
replace north_ir=0 if region!=19&region!=.

gen britain=1 if england==1|wales==1|scotland==1
replace britain=0 if north_ir==1

gen bigregion=1 if region<17
replace bigregion=2 if region==17
replace bigregion=3 if region==18
replace bigregion=4 if region==19
label variable bigregion "1 England, 2 Wales, 3 Scot, 4 North Ire"
tab bigregion pmi, r


*-----------------------------------------------------
*Deleting obs
drop if pmi==.
drop if edu==.
drop if region==.
*----------------------------------------------------

*-------------------------------------------------
*Creating current interview date
*-------------------------------------------------
gen int_date=mdy(month_i,day_i,year_i)
label variable int_date "interview date"
*-------------------------------------------------
*Creating previous interview date
*-------------------------------------------------
sort pid wave
bys pid: gen int_date_tm1=int_date[_n-1]
label variable int_date_tm1 "interview date of previous wave"

*------------------------------------------------
*Creating year of previous interview wave
*------------------------------------------------
sort pid wave
bys pid: gen year_tm1=year_i[_n-1]
label variable year_tm1 "interview year of previous wave"



*****************************************************************************
*Saving clean dataset
*****************************************************************************
save "$datasets/dataset_cleaned.dta", replace

log close

