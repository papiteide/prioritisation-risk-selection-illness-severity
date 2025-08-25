


cap log close
clear
set mat 400
set memory 400m
set more off

log using "$log/create_dataset.log", replace
set linesize 80


************************************************************
*The objective of the file is to build a dataset in stata xt format
*With the waves 6-18 of the BHPS
*************************************************************


***********************************************************
*First, it merges the dataset for waves 6 to 18 of the BHPH
*Then it reshape it from wide to long format so that it can be used by xt
*Then it merge it with time invariante information
*Then it save is in a final dataset
***********************************************************

*------------------------------
*First We Merge the waves 6-18 of the BHPS
*-------------------------------

*For each wave, we start with the individual respondent file
*We merge it with household variables.
*Notice that you cannot merge household across waves...
*So, this is the best place to build thouse household variables from individual ones...
*Then we save a file called borrar_waveX

*we will not use xindall because the interesting vbles are incorporated in xindresp
*Moreover, xindall has registers for kids, so it would be a bit messy 



global waveid "f g h i j k l m n o p q r"


*******************************************
*Ordering the files appropiately
******************************************

use "$rawBHPS/xwavedat.dta", clear
sort pid
save "$junk/xwavedat.dta", replace

foreach y of global waveid {
use "$rawBHPS/`y'hhsamp.dta"
sort `y'hid
save "$junk/`y'hhsamp", replace

use "$rawBHPS/`y'hhresp.dta"
sort `y'hid
save "$junk/`y'hhresp.dta", replace

use "$rawBHPS/`y'indresp.dta"
sort pid
save "$junk/`y'indresp.dta", replace

}

************************************************
*Merging files to build wave specific datasets
**********************************************

foreach y of global waveid {
*loading individual respondent file
cd "$junk"

use `y'indresp, clear

*merging with household file
sort `y'hid

display "merging with hhsamp"
merge `y'hid using `y'hhsamp
tab _merge
display "sum pid if _merge==1"
sum pid if _merge==1
display "sum pid if _merge==2"
sum pid if _merge==2
keep if _merge==3
drop _merge
sort `y'hid

display "merging with hhresp"
merge `y'hid using `y'hhresp
tab _merge
display "sum pid if _merge==1"
sum pid if _merge==1
display "sum pid if _merge==2"
sum pid if _merge==2
keep if _merge==3
drop _merge


*******************************************************************
*Here, I could should create the household variables
*Remember to preeced the name of the vble the `y'
*******************************************************************


gen t=1 if `y'hlcvrh==1
replace t=0 if t==.
bys `y'hid: egen `y'directlypmih=max(t)   
label variable `y'directlypmih "1 if at least a member of the household has pmi paid directly, 0 othw"
drop t

gen tt=1 if `y'hlcvrh==2
replace tt=0 if tt==.
bys `y'hid: egen `y'wagepmih=max(tt)   
label variable `y'wagepmih "1 if at least a member of the household has pmi deducted from the wages, 0 othw"
drop tt

gen ttt=1 if `y'hlcvrh==3
replace ttt=0 if ttt==.
bys `y'hid: egen `y'employerpmih=max(ttt)   
label variable `y'employerpmih "1 if at least of member of the household has pmi paid by the employer, 0 othw"
drop ttt

sort pid
save borrar_wave_`y', replace
}



*In this global, we have to put all the variables that we want are in the final dataset
*We must ommit the prefix related to the wave

#delimit ;

global vbles "age hid pno directlypmih wagepmih employerpmih mlstat mlchng jbstat plnew plnowy4 aidhh aidhrs 
                      nch02 nch1215 nch1618 nch34 nch511 nchild nkids qfachi qfedhi school scnow sctype j2has
                      jbpen jbpenm plbornc race hlcvr hlcvrh hlcvrl hscana region yr2uk4 hhmove nxdts xdts hlsv hlck
                      hosp hospch hospd hospnhs hl2gp hlsvf hlsvff hlsvfn hlsvg hlsvgf hlsvgn hlsvl hlsvlf hlsvln hlsve hlsvef hlsven hlsvi hlsvif hlsvin   
                     hldsbl hlprba hlprbb hlprbc hlprbd hlprbe hlprbf hlprbg hlprbh hlprbi hlprbj hlprbk hlprbl hlprbm hlprbn hlprbo 
                    ncigs smoker ghqa 
                    ghqb ghqc ghqd ghqe ghqf ghqg ghqh ghqi ghqj ghqk ghql hlghq1 hlghq2  heatch hsctax hsownd rentll 
                    tenure hsprbg hsprbh hsprbi hsprbj hsprbk hsprbl hsprbm  hsprbn hsprbo hsprbp hsprbq hsroom hstype 
                    fihhmn fihhmni fimn fimnl fiyr fiyrl fiyrnl fieqfca fieqfcb  fihhml fihhmnl fihhyi fihhyl fihhynl fihhyr 
                    jbhrs jbot jshrs spjbhr jbotpd doim doid doiy4 jbiscon_cc jbmngr jbsect jbsize jbsic
                   hlcka hlckb hlckc hlckd hlcke hlckf hlckg hlckh hlcki
                  hlckan hlckbn hlckcn hlckdn hlcken hlckfn hlckgn hlckhn hlckin
                  cjsten jsbgd jsbgm jsbgy4 jbbgd jbbgm jbbgy4 cjsbgd cjsbgm cjsbgy4";
#delimit cr



global waveid3 "r q p o n m l k j i h g f"

*Now we keep the variables that we need in each dataset

foreach y of global waveid3 {
cd "$junk"
use borrar_wave_`y'

*Otherwise, they interfere with jbsic92
cap drop rjbsicp rjbsicr
cap drop qjbsicp qjbsicr
cap drop pjbsicp pjbsicr



*With the prefix of `y' we must indicate here all the variables that will go in the final dataset
#delimit ;
keep  pid `y'age `y'hid `y'pno `y'directlypmih `y'wagepmih `y'employerpmih `y'mlstat `y'mlchng `y'jbstat `y'plnew `y'plnowy4 
                `y'aidhh `y'aidhrs `y'nch02 `y'nch1215 `y'nch1618 `y'nch34 `y'nch511 `y'nchild `y'nkids `y'qfachi `y'qfedhi
                `y'school `y'scnow `y'sctype `y'j2has `y'jbpen `y'jbpenm `y'plbornc `y'race `y'hlcvr `y'hlcvrh `y'hlcvrl 
                `y'hscana `y'region `y'yr2uk4 `y'hhmove `y'nxdts `y'xdts `y'hlck `y'hlsv `y'hosp `y'hospch `y'hospd 
                `y'hospnhs `y'hl2gp `y'hlsvg `y'hlsvgf `y'hlsvgn `y'hlsvl `y'hlsvln `y'hlsvlf `y'hlsvf `y'hlsvff `y'hlsvfn `y'hlsve `y'hlsvef `y'hlsven `y'hlsvi `y'hlsvif `y'hlsvin
                `y'hldsbl `y'hlprba `y'hlprbb `y'hlprbc `y'hlprbd `y'hlprbe `y'hlprbf `y'hlprbg `y'hlprbh `y'hlprbi `y'hlprbj `y'hlprbk `y'hlprbl `y'hlprbm 
                `y'ncigs `y'ghqa `y'ghqb `y'ghqc `y'ghqd `y'ghqe `y'ghqf `y'ghqg `y'ghqh `y'ghqi `y'ghqj `y'ghqk `y'ghql 
                 `y'hlghq1 `y'hlghq2  `y'heatch `y'hsctax `y'hsownd `y'rentll `y'tenure `y'hsprbg `y'hsprbh `y'hsprbi 
                 `y'hsprbj `y'hsprbk `y'hsprbl `y'hsprbm  `y'hsprbn `y'hsprbo `y'hsprbp `y'hsprbq `y'hsroom `y'hstype 
                 `y'fihhmn `y'fihhmni `y'fimn `y'fimnl `y'fiyr `y'fiyrl `y'fiyrnl `y'fieqfca `y'fieqfcb  `y'fihhml `y'fihhmnl `y'fihhyi `y'fihhyl 
                 `y'fihhynl `y'fihhyr `y'jbhrs `y'jbot `y'jshrs `y'spjbhr `y'jbotpd
                 `y'doim `y'doid `y'doiy4 `y'jbiscon_cc `y'jbmngr `y'jbsect `y'jbsize `y'jbsic
                 `y'hlcka  `y'hlckb  `y'hlckc  `y'hlckd  `y'hlcke  `y'hlckf  `y'hlckg  `y'hlckh  `y'hlcki
                 `y'hlckan `y'hlckbn `y'hlckcn `y'hlckdn `y'hlcken `y'hlckfn `y'hlckgn `y'hlckhn `y'hlckin
                  `y'cjsten `y'jsbgd `y'jsbgm `y'jsbgy4 `y'jbbgd `y'jbbgm `y'jbbgy4 `y'cjsbgd `y'cjsbgm `y'cjsbgy4 


;

#delimit cr

sort pid
save borrar_wave2_`y', replace
}

*------------------------------------------------------------------------------
*This is for the contents insurance vbles because they are not in all the waves
*-----------------------------------------------------------------------------

use borrar_wave2_r, clear
sort rhid
merge rhid using rhhresp, keep(rhscntl rhscanl)
tab _merge
drop _merge
save borrar_wave2_r, replace


use borrar_wave2_q, clear
sort qhid
merge qhid using qhhresp, keep(qhscntl qhscanl)
tab _merge
drop _merge
save borrar_wave2_q, replace

use borrar_wave2_p, clear
sort phid
merge phid using phhresp, keep(phscntl phscanl)
tab _merge
drop _merge
save borrar_wave2_p, replace

use borrar_wave2_o, clear
sort ohid
merge ohid using ohhresp, keep(ohscntl ohscanl)
tab _merge
drop _merge
save borrar_wave2_o, replace

use borrar_wave2_n, clear
sort nhid
merge nhid using nhhresp, keep(nhscntl nhscanl)
tab _merge
drop _merge
save borrar_wave2_n, replace


*---------------------------------------------------------------------
*Now, we rename vbles as pseudo numeric
*We also load those vbles that are just in some datasets
*-----------------------------------------------------------------------

*Wave 18
use "$junk/borrar_wave2_r", clear
drop if pid==.
sort pid
merge pid using "$junk/rindresp", keep(rhlprbo rhlprbn rsmoker rjbterm1 rjbsic92 rxewght rxewtuk1 rxewtuk2 rhl2hop rhlstat rjbiscon_cc rjbempr rjbcspl rjhstpy)

ren rhscntl  hscntl_18
ren rhscanl  hscanl_18
*qhscntl comes from above and are only included in waves 17, 16, 15, and 14

drop if _merge==2
drop _merge
foreach t of global vbles {
ren r`t' `t'_18
}
ren rjbterm1 jbterm1_18
ren jbsic_18 jbsic92_18

*ren jbiscon_cc jbiscon_cc_18 
ren rjbempr jbempr_18
ren rjbcspl jbcspl_18
ren rjhstpy jhstpy_18

ren rxewght   weights1_18
ren rxewtuk1  weights3_18
ren rhl2hop   hl2hop_18 
ren rxewtuk2  weights4_18
ren rhlstat   hlstat_18

sort pid
save "$junk/borrar_wave2_r", replace


*Wave 17
use "$junk/borrar_wave2_q", clear
drop if pid==.
sort pid
merge pid using "$junk/qindresp", keep(qhlprbo qhlprbn qsmoker qjbterm1 qjbsic92 qxewght qxewtuk1 qxewtuk2 qhl2hop qhlstat qjbiscon_cc qjbempr qjbcspl qjhstpy)

ren qhscntl  hscntl_17
ren qhscanl  hscanl_17
*qhscntl comes from above and are only included in waves 17, 16, 15, and 14

drop if _merge==2
drop _merge
foreach t of global vbles {
ren q`t' `t'_17
}
ren qjbterm1 jbterm1_17
ren jbsic_17 jbsic92_17

*ren qjbiscon_cc jbiscon_cc_17 
ren qjbempr jbempr_17
ren qjbcspl jbcspl_17
ren qjhstpy jhstpy_17 


ren qxewght   weights1_17
ren qxewtuk1  weights3_17
ren qhl2hop   hl2hop_17 
ren qxewtuk2  weights4_17
ren qhlstat   hlstat_17

sort pid
save "$junk/borrar_wave2_q", replace



*Wave 16
use "$junk/borrar_wave2_p", clear
drop if pid==.
sort pid
merge pid using "$junk/pindresp", keep(phlprbo phlprbn psmoker pjbterm1 pjbsic92 pxewght pxewtuk1 pxewtuk2 phlhtc phlhtf phlhti phlhtm phlwtk phlwtp phlwts phlwtm phlwte phl2hop phlstat pjbiscon_cc pjbempr pjbcspl pjhstpy)

ren phscntl  hscntl_16
ren phscanl  hscanl_16
*phscntl comes from above and are only included in waves 16, 15, and 14

ren phlstat  hlstat_16
drop if _merge==2
drop _merge
foreach t of global vbles {
ren p`t' `t'_16
}
ren pjbterm1 jbterm1_16
ren jbsic_16 jbsic92_16

*ren pjbiscon_cc jbiscon_cc_16 
ren pjbempr jbempr_16
ren pjbcspl jbcspl_16
ren pjhstpy jhstpy_16  

ren pxewght   weights1_16
ren pxewtuk1  weights3_16
ren phl2hop   hl2hop_16
ren pxewtuk2  weights4_16

*Weight and height that are only in this wave
replace phlwtp=. if phlwtm==1&phlwtp<0 
replace phlwts=. if phlwtm==1&phlwts<0 
replace phlwtk=. if phlwtm==2&phlwtk<0 

gen pounds=phlwtp if phlwtm==1
replace pounds=14*phlwts+pounds if phlwtm==1

gen weight_16=phlwtk if phlwtm==2
replace weight_16=pounds/2.2 if phlwtm==1


replace phlhtc=. if phlhtc<0
replace phlhtf=. if phlhtf<0
replace phlhti=. if phlhti<0

gen height_16=phlhtc if phlhtm==2
replace height_16=phlhtf*30.48 if phlhtm==1
replace height_16=height_16+phlhti*2.54 if phlhtm==1

ren phlwte weight_accuracy_16

drop pounds phlhtc phlhtf phlhti phlhtm phlwtk phlwtp phlwts phlwtm 



sort pid
save "$junk/borrar_wave2_p", replace




*Wave 15
use "$junk/borrar_wave2_o", clear
drop if pid==.
sort pid
merge pid using "$junk/oindresp", keep( ohlprbo ohlprbn osmoker ojbterm1 ojbsic92 oxewght oxewtuk1 oxewtuk2 ohl2hop ohlstat)


*We can comment them because they already come renamed in oindresp
*optrt5n1 optrt5n2 are personality traits and are only availabe in wave 15
*ren optrt5n1 ptrt5n1_15
*ren optrt5n2 ptrt5n2_15

ren ohscntl  hscntl_15
ren ohscanl  hscanl_15
*ohscntl comes from above and are only included in wave 15 and 14

ren ohlstat  hlstat_15

drop if _merge==2
drop _merge
foreach t of global vbles {
ren o`t' `t'_15
}

ren ojbterm1 jbterm1_15
ren jbsic_15 jbsic92_15
ren ohl2hop  hl2hop_15

ren oxewght   weights1_15
ren oxewtuk1  weights3_15
ren oxewtuk2  weights4_15

sort pid
save "$junk/borrar_wave2_o", replace


*Renaming vbles of wave n: 14
use "$junk/borrar_wave2_n", clear
drop if pid==.
sort pid
#delimit ;
merge pid using "$junk/nindresp", 
keep(nhlprbo nhlprbn nsmoker njbterm1 njbsic92 nhlhtc nhlhtf nhlhti nhlhtm nhlwtk nhlwtp nhlwts nhlwtm nhlwte nxewght nxewtuk1 nxewtuk2 nhl2hop nhlstat nhlsf10c);
#delimit cr

drop if _merge==2
drop _merge
foreach t of global vbles {
ren n`t' `t'_14
}

ren nhscntl  hscntl_14
ren nhscanl  hscanl_14
*nhscntl comes from above and are only included in wave 15 and 14

ren nhlstat  hlstat_14

ren nxewght   weights1_14
ren nxewtuk1  weights3_14
ren nxewtuk2  weights4_14

*Weight and height that are only in this wave
replace nhlwtp=. if nhlwtm==1&nhlwtp<0 
replace nhlwts=. if nhlwtm==1&nhlwts<0 
replace nhlwtk=. if nhlwtm==2&nhlwtk<0 

gen pounds=nhlwtp if nhlwtm==1
replace pounds=14*nhlwts+pounds if nhlwtm==1

gen weight_14=nhlwtk if nhlwtm==2
replace weight_14=pounds/2.2 if nhlwtm==1


replace nhlhtc=. if nhlhtc<0
replace nhlhtf=. if nhlhtf<0
replace nhlhti=. if nhlhti<0

gen height_14=nhlhtc if nhlhtm==2
replace height_14=nhlhtf*30.48 if nhlhtm==1
replace height_14=height_14+nhlhti*2.54 if nhlhtm==1

ren nhlwte weight_accuracy_14

drop pounds nhlhtc nhlhtf nhlhti nhlhtm nhlwtk nhlwtp nhlwts nhlwtm 

*These vbles are new in this wave, they have been adapted to previous waves
ren njbterm1 jbterm1_14

ren nhl2hop  hl2hop_14
ren jbsic_14 jbsic92_14
ren nhlsf10c hlsf10c_14

sort pid
save "$junk/borrar_wave2_n", replace



*Renaming vbles of wave m: 13
use "$junk/borrar_wave2_m", clear
drop if pid==.
sort pid
merge pid using "$junk/mindresp", keep(mhlprbo mhlprbn msmoker mjbterm1 mjbsic92 mxewght mxewtuk1 mxewtuk2 mhl2hop mhlstat mlfimpa)


drop if _merge==2
drop _merge
foreach t of global vbles {
ren m`t' `t'_13
}

*These vbles are new in this wave, they have been adapted to previous waves
ren mjbterm1 jbterm1_13

ren jbsic_13 jbsic92_13
ren mhlstat  hlstat_13
ren mxewght   weights1_13
ren mxewtuk1  weights3_13
ren mhl2hop   hl2hop_13
ren mxewtuk2  weights4_13
ren mlfimpa lfimpa_13

sort pid
save "$junk/borrar_wave2_m", replace



*Renaming vbles of wave l: 12
use "$junk/borrar_wave2_l", clear
drop if pid==.
sort pid
merge pid using "$junk/lindresp", keep(lhlprbo lhlprbn lsmoker ljbterm1 ljbsic92 lxewght lxewtuk1 lxewtuk2 lhl2hop lhlstat)

drop if _merge==2
drop _merge
foreach t of global vbles {
ren l`t' `t'_12
}


*These vbles are new in this wave, they have been adapted to previous waves
ren ljbterm1 jbterm1_12

ren jbsic_12 jbsic92_12
*ren ljbsic92 jbsic92_12
cap drop jbsic_12
ren lhlstat  hlstat_12
ren lxewght   weights1_12
ren lxewtuk1  weights3_12
ren lhl2hop    hl2hop_12
ren lxewtuk2  weights4_12

sort pid
save "$junk/borrar_wave2_l", replace


*Renaming vbles of wave k: 11
use "$junk/borrar_wave2_k", clear
drop if pid==.
sort pid
merge pid using "$junk/kindresp", keep(khlprbo khlprbn ksmoker kjbterm1 kxewght kxewghte kxewtuk1 kxewtuk2 khl2hop khlstat)
drop if _merge==2
drop _merge
foreach t of global vbles {
ren k`t' `t'_11
}
ren kjbterm1 jbterm1_11

ren kxewght   weights1_11
ren kxewghte  weights2_11 
ren kxewtuk1  weights3_11
ren khl2hop   hl2hop_11
ren kxewtuk2  weights4_11
ren khlstat  hlstat_11

sort pid
save "$junk/borrar_wave2_k", replace

*Renaming vbles of wave j: 10

use "$junk/borrar_wave2_j", clear
drop if pid==.
gen jhlprbo=. 
gen jhlprbn=.
sort pid
merge pid using "$junk/jindresp", keep(jsmoker jjbterm1 jxewght jxewghte jxewtsw1 jxewtsw2 jhl2hop jhlstat)
drop if _merge==2
drop _merge
foreach t of global vbles {
ren j`t' `t'_10
}
ren jjbterm1 jbterm1_10

ren jxewght   weights1_10
ren jxewghte  weights2_10 
ren jxewtsw1  weights3_10
ren jhl2hop    hl2hop_10
ren jxewtsw2  weights4_10
ren jhlstat hlstat_10
sort pid
save "$junk/borrar_wave2_j", replace


*Renaming vbles of wave i: 9
*hlstat is not available in this wave
use "$junk/borrar_wave2_i", clear
drop if pid==.
gen ihlprbo=. 
gen ihlprbn=.
merge pid using "$junk/iindresp", keep(ismnow ismever ijbterm1 ixewght ixewghte ixewtsw1 ixewtsw2 ihl2hop ihlsf10c)
drop if _merge==2
drop _merge
ren ismnow ismoker
replace ismoker=2 if ismever==2
drop ismever
foreach t of global vbles {
ren i`t' `t'_9
}
ren ijbterm1 jbterm1_9
ren ixewght   weights1_9
ren ixewghte  weights2_9 
ren ixewtsw1  weights3_9
ren ihl2hop    hl2hop_9
ren ixewtsw2  weights4_9
ren ihlsf10c hlsf10c_9

sort pid
save "$junk/borrar_wave2_i", replace


*Renaming vbles of wave h: 8

use "$junk/borrar_wave2_h", clear
drop if pid==.
gen hhlprbo=. 
gen hhlprbn=.
sort pid
merge pid using "$junk/hindresp", keep(hsmoker hjbterm hxewght hxewghte hhl2hop hhlstat hlfimpa)
drop if _merge==2
drop _merge


foreach t of global vbles {
ren h`t' `t'_8

}
ren hjbterm jbterm1_8

ren hxewght   weights1_8
ren hxewghte  weights2_8 
ren hhl2hop hl2hop_8
ren hhlstat hlstat_8
ren hlfimpa lfimpa_8

sort pid
save "$junk/borrar_wave2_h", replace



*Renaming vbles of wave g: 7

use "$junk/borrar_wave2_g", clear
drop if pid==.
gen ghlprbo=. 
gen ghlprbn=.
sort pid
merge pid using "$junk/gindresp", keep(gsmoker gjbterm gxewght gxewghte ghl2hop ghlstat)
drop if _merge==2
drop _merge


foreach t of global vbles {
ren g`t' `t'_7
}
ren gjbterm   jbterm1_7
ren gxewght   weights1_7
ren gxewghte  weights2_7 
ren ghl2hop   hl2hop_7
ren ghlstat hlstat_7

sort pid
save "$junk/borrar_wave2_g", replace

*Renaming vbles of wave f: 6

use "$junk/borrar_wave2_f", clear
drop if pid==.
gen fhlprbo=. 
gen fhlprbn=.
sort pid
merge pid using "$junk/findresp", keep(fsmoker fjbterm fxewght fhlstat)
drop if _merge==2
drop _merge


foreach t of global vbles {
ren f`t' `t'_6
}
ren fjbterm jbterm1_6
ren fxewght   weights1_6
ren fhlstat hlstat_6


sort pid
save "$junk/borrar_wave2_f", replace




global waveid2 "q p o n m l k j i h g f "

*----------------------------------------------------
*Now we build a file in wide format merging all the waves 
*---------------------------------------------------
cd "$junk"
use borrar_wave2_r
sort pid

foreach y of global waveid2 {
merge pid using borrar_wave2_`y'
sort pid


tab _merge
drop _merge
}

sort pid

save "$junk/waves6-18_wide", replace

use "$junk/waves6-18_wide", clear

*------------------------------------------------------------
*Now we Reshape it to long format. Use lists of vbles with the @ at the beginning of the words y alfanumerico !!!

*Here we must put all the time variant vbles with a sufix of @
#delimit ;
reshape long hid@ pno@ age@ directlypmih@ wagepmih@ employerpmih@ mlstat@ mlchng@ jbstat@ plnew@ plnowy4@ aidhh@ aidhrs@ 
                      nch02@ nch1215@ nch1618@ nch34@ nch511@ nchild@ nkids@ qfachi@ qfedhi@ school@ scnow@ sctype@ j2has@ 
                      jbpen@ jbpenm@ plbornc@ race@ hlcvr@ hlcvrh@ hlcvrl@ hscana@ region@ yr2uk4@ hhmove@ nxdts@ xdts@  
                      hlsv@ hosp@ hlsvf@ hlsvff@ hlsvfn@ hlsvg@ hlsvgn@ hlsvgf@ hlsvl@ hlsvlf@ hlsvln@ hlsve@ hlsven@ hlsvef@ hlsvi@ hlsvif@ hlsvin@  
                      hl2hop@ hospch@ hospd@ hospnhs@ hl2gp@ hldsbl@ hlprba@ 
                      hlprbb@ hlprbc@ hlprbd@ hlprbe@ hlprbf@ hlprbg@ hlprbh@ hlprbi@ hlprbj@ hlprbk@ hlprbl@ hlprbo@ hlprbn@ 
                      hlprbm@ smoker@ ncigs@ ghqa@ ghqb@ ghqc@ ghqd@ ghqe@ ghqf@ ghqg@ ghqh@ ghqi@ ghqj@ ghqk@ ghql@ hlghq1@
                      hlghq2@  heatch@ hsctax@ hsownd@ rentll@ tenure@ hsprbg@ hsprbh@ hsprbi@ hsprbj@ hsprbk@ hsprbl@ hsprbm@  hsprbn@ 
                      hsprbo@ hsprbp@ hsprbq@ hsroom@ hstype@ 
                      fihhmn@ fihhmni@ fimn@ fimnl@ fiyr@ fiyrl@ fiyrnl@ fieqfca@ fieqfcb@  fihhml@ fihhmnl@ fihhyi@ fihhyl@ fihhynl@ fihhyr@ 
                      jbhrs@ jbot@ jshrs@ spjbhr@ jbotpd@ doim@ doid@ doiy4@
                      jbterm1@ jbiscon_cc@ jbmngr@ jbsect@ jbsize@ jbsic@ jbsic92@
                      hlckan@ hlckbn@ hlckcn@ hlckdn@ hlcken@ hlckfn@ hlckgn@ hlckhn@ hlckin@ hlck@
                      hlcka@  hlckb@  hlckc@  hlckd@  hlcke@  hlckf@  hlckg@  hlckh@  hlcki@ hlstat@ lfimpa@ hlsf10c@
                      ptrt5n1@ ptrt5n2@ hscntl@ hscanl@ weight_accuracy@ height@ weight@  weights1 weights2 weights3
                      cjsten@ jsbgd@ jsbgm@ jsbgy4@ jbempr@ jbcspl@ jbbgd@ jbbgm@ jbbgy4@ cjsbgd@ cjsbgm@ cjsbgy4@ jhstpy@              
                     , i(pid) j(wave _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18) string;

#delimit cr
ren wave temp
gen wave=6 if temp=="_6"
replace wave=7 if temp=="_7"
replace wave=8 if temp=="_8"
replace wave=9 if temp=="_9"
replace wave=10 if temp=="_10"
replace wave=11 if temp=="_11"
replace wave=12 if temp=="_12"
replace wave=13 if temp=="_13"
replace wave=14 if temp=="_14"
replace wave=15 if temp=="_15"
replace wave=16 if temp=="_16"
replace wave=17 if temp=="_17"
replace wave=18 if temp=="_18"

drop temp

reshape clear

ren doim month_i
ren doid day_i
ren doiy4 year_i

label variable month_i "Month of interview"
label variable day_i "Day of interview"
label variable year_i "Year of interview"


*---------------------------------------------------------------
*Merge it with xwavedat: time invarying covariates
*----------------------------------------------------------------

sort pid
merge pid using "$junk/xwavedat.dta"
tab _merge
keep if _merge==3
drop _merge


*Create age using hospitalization reference date




*THIS IS NEW!!! THE DATA USED TO HAVE THE MONTH OF BIRTH !!!!!!
gen day=1
*gen datebirth=mdy(dobm,day,doby)
gen datebirth=mdy(6,day,doby)

gen agehosp=(mdy(9,1,2000)-datebirth)/(365.25) if wave==11

replace agehosp=(mdy(9,1,2007)-datebirth)/(365.25) if wave==18
replace agehosp=(mdy(9,1,2006)-datebirth)/(365.25) if wave==17
replace agehosp=(mdy(9,1,2005)-datebirth)/(365.25) if wave==16
replace agehosp=(mdy(9,1,2004)-datebirth)/(365.25) if wave==15
replace agehosp=(mdy(9,1,2003)-datebirth)/(365.25) if wave==14
replace agehosp=(mdy(9,1,2002)-datebirth)/(365.25) if wave==13
replace agehosp=(mdy(9,1,2001)-datebirth)/(365.25) if wave==12
replace agehosp=(mdy(9,1,1999)-datebirth)/(365.25) if wave==10
replace agehosp=(mdy(9,1,1998)-datebirth)/(365.25) if wave==9
replace agehosp=(mdy(9,1,1997)-datebirth)/(365.25) if wave==8
replace agehosp=(mdy(9,1,1996)-datebirth)/(365.25) if wave==7
replace agehosp=(mdy(9,1,1995)-datebirth)/(365.25) if wave==6
label variable agehosp "age in years at hospitalization reference date"


iis pid
tis wave


#delimit ;
label def jbstat 	1 "Self-employed"
			2 "Employed"
			3 "Unemployed"
			4 "Retired"
			5 "Maternity leave"
			6 "Family care"
			7 "FT studt, school"
			8 "LT sick, disabld"
			9 "Gvt trng scheme"
			10 "Other"
			-9 "Missing or wild", modify;
label values jbstat jbstat;
#delimit cr

* -------------------
* MERGE WAGE DEFLATOR 
* -------------------
gen month=month_i
gen year=year_i

tab month
sum fimnl fiyrl fihhmn if month==.|month>12|month<0,  d

sum fimnl fiyrl fihhmn if month==., d

*The following sentence does not have any real effect because
*income is missing when month is missing
replace month=6 if month==.|month>12|month<0

so year month
merge year month using "$datasets/retail_price_index_dec2008.dta"
tab _merge
       					
					
drop if _merge==2
drop _merge
drop month year


* -------------------------------
* DEFLATE EARNINGS TO BASE PRICES 
* -------------------------------
replace fimnl=. if fimnl<0
replace fiyrl=. if fiyrl<0
replace fihhmn=. if fihhmn<0
replace fihhyr=. if fihhyr<0



gen incomehh= fihhyr*(212.9/rpi)  /* total (labor+nonlabor) household income in Dec 2008 £*/        
replace incomehh=incomehh/100000
label variable incomehh "Real Annual household Income measured in Dec 2008 £ divided by 100000"
notes incomehh: "labor+nonlabor"

gen remth	=fimnl*212.9/rpi 		/* monthly in Dec 2008 £ */
replace remth=remth/10000
label var remth 	"Monthly Indiv Labour Income measured in Dec 2008 £, divided by 10000"

gen reyr 	=fiyrl*212.9/rpi 		/* annual in Dec 2008 £: refers to pay period Sept -> Sept */
replace reyr=reyr/100000
label var reyr	"Annual Labour Income measured in Dec 2008 £, divided by 100000"


gen rehhmth	=fihhmn*212.9/rpi 	/* monthly in Dec 2008 £ */
replace rehhmth=rehhmth/10000
label var rehhmth	"Monthly Household Labour Income measured in Dec 2008 £,divided by 10000"



*-------------------------------------
*Preparing Other job characteristics
*-------------------------------------


gen job_permanent=jbterm1
replace job_permanent=0 if jbterm1>1&jbterm1!=.
replace job_permanent=. if jbterm1<0
label variable job_permanent "1 if job is permanent, 0 otherwise"

gen job_privatefirm=jbsect
replace job_privatefirm=0 if jbsect>1&jbsect!=.
replace job_privatefirm=. if job_privatefirm<0
label variable job_privatefirm "1 if indiv works in private firm, 0 othw"

label variable jbsize "Total number of employees at workplace"

save "$datasets/dataset_not_cleaned.dta", replace

log close




