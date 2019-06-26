*create log file

set more off
clear



*------------------------------------------------
* 1-bangladesh-dm-ee-anthro-ee.do
*
* Audrie Lin (audrielin@berkeley.edu)
*
* Description:
*
* Merge anthro baseline, midline, endline
* Calculate anthro z-scores
* Calculate time between anthro measurements covariate
* Calculate growth velocity
*
* version 1 (2018-11-09)
* version 2 (2019-06-16)
*
*-----------------------------------------------
*-----------------------------------------------
* 
* input files: 
* 
* Anthro Form data files:
* ~/Desktop/stata/WBB-EED-analysis/data/untouched/Baseline/Baseline_Anthro_CLEANED_24Sept14_NamesStripped.dta
* ~/Desktop/stata/WBB-EED-analysis/data/untouched/Midline/Anthro_Midline_Cleaned_MatchedwEnrollment_26Jan16.dta
* ~/Desktop/stata/WBB-EED-analysis/data/untouched/Endline/EE_Endline_Anthro_CLEANED_data_26June2016.dta
* /Users/audrie/Desktop/stata/WBB-EED-analysis/data/final/washb-bangladesh-anthro-diar-ee-med-blind-tr.dta (created from bangladesh-dm-ee-med.do)
*  
*
* output files:
* $outdir/bangladesh-dm-ee-anthro-ee.dta (merged anthro data, t1, t2, t3)
* 
*------------------------------------------------
*------------------------------------------------
global indir   "~/Desktop/stata/WBB-EED-analysis/data/untouched/"
global tempdir "~/Desktop/stata/WBB-EED-analysis/data/temp"
global outdir  "~/Desktop/stata/WBB-EED-analysis/data/final"
global output  "~/Desktop/stata/WBB-EED-analysis/analysis/tables/rawoutput"
global whodir "~/Desktop/stata/who-igrowup/"


global pgm "1-bangladesh-dm-ee-anthro-ee.do"

cd $tempdir


*---------------------------------------------
* Merge anthro with med history form
*---------------------------------------------

use $outdir/washb-bangladesh-anthro-diar-ee-med-blind-tr.dta, clear

keep childid sex dob

label var sex "Sex"
*label define sex 0 "female" 1 "male"
*sex already defined
label values sex sex

label var dob "Date of birth"

format dob %d

save $tempdir/washb-bangladesh-anthro-diar-ee-med-blind-tr-anthro.dta, replace

clear


*--------------------------------------------
* Rename variables from Anthro data Forms @ T1
*--------------------------------------------

* Load input file
use "~/Desktop/stata/WBB-EED-analysis/data/untouched/Baseline/Baseline_Anthro_CLEANED_24Sept14_NamesStripped.dta", clear

rename childno childno_at1
rename dataid dataid_at1
rename faid faid_at1
rename samplecoldate samplecoldate_t1

destring clusterid, replace

*generate new variable to include twins w/ same dataid
egen childid = concat(dataid_at1 childno_at1)

sort childid 
quietly by childid:  gen dup = cond(_N==1,0,_n)
list childid samplecoldate_t1 if dup>=1


* convert date of sample collection" variables from string to numeric variables for checking age values
gen samplecoldate_fulldate_t1 = date(samplecoldate_t1, "DMY") 
	format samplecoldate_fulldate_t1 %d


*rename and distinguish variables with anthro identifier
rename samplecoldate_fulldate_t1 samplecoldate_at1

merge 1:1 childid using $tempdir/washb-bangladesh-anthro-diar-ee-med-blind-tr-anthro.dta

keep if _merge==3

drop _merge

*--------------------------------------------
* Age, using measurement date and birth date @ T1
*--------------------------------------------


gen anthrodate = samplecoldate_at1
	format anthrodate %d
	label var anthrodate "Date of anthro measurement t1"


gen aged = anthrodate-dob
	label var aged "Age in days (anthro meas t1)"
gen double agem = aged/30.4167
	label var agem "Age in months (anthro meas t1)"
gen double agey = aged/365.25
	label var agey "Age in years (anthro meas t1)"
codebook agey

* Month of measurement
gen month = month(anthrodate)
	label var month "Month of measurement"
	

rename anthrodate anthrodate_at1



*--------------------------------------------
* ensure that all the anthro measurements 
* are rounded to the correct level of precision
* no more than 2 decimal places
*--------------------------------------------

for any q7 q8 q9 q13 q14 q15 q16 q17 q18 q19 q20 q21 : destring X, replace
for any q7 q8 q9 q13 q14 q15 q16 q17 q18 q19 q20 q21 : replace X = round(X,0.01)

*--------------------------------------------
* Calculate median length measurements
*--------------------------------------------
label var q13 "Child length meas 1"
label var q14 "Child length meas 2"
label var q15 "Child length meas 3"

gen len1 = q13
gen len2 = q14
gen len3 = q15
replace len1 = . if len1>999 | len1<=0
replace len2 = . if len2>999 | len2<=0
replace len3 = . if len3>999 | len3<=0


egen float lenhei = rowmedian(len1 len2 len3)
	replace lenhei = . if lenhei>999
	replace lenhei = round(lenhei,0.001)
	label var lenhei "Child length (median), cm"
	notes lenhei: Median of replicate measures

drop len1-len3



rename q12 q12_t1


gen str measure="L" if q12_t1==1
replace measure="H" if q12_t1==2
replace measure=" " if measure == ""
label var measure "Height measured lying -L- or standing -H-"


*--------------------------------------------
* Calculate median weight measurements
*--------------------------------------------

label var q7 "Child weight meas 1"
label var q8 "Child weight meas 2"
label var q9 "Child weight meas 3"
for any q7 q8 q9 : replace X = . if (X>99 | X<=0)

* create median of mom + child and mom alone
* we have to do it this way because this reflects
* the method used in the field to determine if a 
* 3rd measurement was required


* create median of the child (if measured alone)
egen float wch = rowmedian(q7 q8 q9)

gen float weight = wch
	replace weight = round(weight,0.001)
	label var weight "Child weight (median), kg"
	notes weight: Median of replicate measures

drop  wch


*--------------------------------------------
* Calculate median head circumference measurements
*--------------------------------------------
label var q16 "Head circumference meas 1"
label var q17 "Head circumference meas 2"
label var q18 "Head circumference meas 3"

gen hc1 = q16
gen hc2 = q17
gen hc3 = q18
replace hc1 = . if hc1>99 | hc1<=0
replace hc2 = . if hc2>99 | hc2<=0
replace hc3 = . if hc3>99 | hc3<=0

egen float headc = rowmedian(hc1 hc2 hc3)
	replace headc = . if headc>99
	replace headc = round(headc,0.001)
	label var headc "Child head circumference (median), cm"
	notes headc: Median of replicate measures
	


*--------------------------------------------
* Calculate median mid upper arm circumference measurements
*--------------------------------------------


label var q19 "Mid upper arm circumference meas 1"
label var q20 "Mid upper arm circumference meas 2"
label var q21 "Mid upper arm circumference meas 3"

gen mc1 = q19
gen mc2 = q20
gen mc3 = q21
replace mc1 = . if mc1>99 | mc1<=0
replace mc2 = . if mc2>99 | mc2<=0
replace mc3 = . if mc3>99 | mc3<=0

egen float armc = rowmedian(mc1 mc2 mc3)
	replace armc = . if armc>99
	replace armc = round(armc,0.001)
	label var armc "Mid upper arm circumference (median), cm"
	notes armc: Median of replicate measures
	

*save temp file anthro-ee-t1
save $tempdir/bangladesh-ee-anthro-ee-t1.dta, replace


*--------------------------------------------
* Calculate laz, waz, whz, using the zscore06
* add-on package. do not specify oedema
*--------------------------------------------

*zscore06, a(agem) s(sex) h(lenhei) w(weight) female(0) male(1) measure(q12_t1) recum(1) stand(2)

*save a temporary dataset to merge with the WHO igrowup output
*save "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/washb-working-anthro.dta", replace




*--------------------------------------------
* Calculate laz, waz, whz, and wcz using the 
* WHO -igrowup- macro. 
*
* this is necessary because the zscore06 
* package does not calculate z-scores for 
* head circumference.  we are using both
* approaches because the zscore06 package is
* ostensibly more accurate (see the package's documentation)
* though as demonstrated below they provide identical results
* (just a good internal validity check)
*
* The WHO -igrowup- macro requires 15 parameters.
* Refer to the documentation for details on this
* finicky macro.
*
* We don't have all of the measurements that
* it processes, so need to create empty
* variables that allow it to run
*---------------------------------------------



/// Indicate to the Stata compiler where the igrowup_standard.ado file is stored ***
adopath + "$whodir"


*** check key variables

gen str reflib="$whodir"
lab var reflib "Directory of reference tables"

gen str datalib="$tempdir"
lab var datalib "Directory for datafiles"

gen str datalab="TEMPanthro"
lab var datalab "Working file"


* create a temporary sex variable for WHO coding
gen whosex = sex
	replace whosex = 2 if sex==0

	drop sex
rename whosex sex

describe sex
	summarize sex

describe headc
summarize headc

describe armc
summarize armc


describe weight
summarize weight

describe lenhei
summarize lenhei

gen age=aged
	describe age
	summarize age


gen str ageunit="days"




describe measure
summarize measure



*** create missing variables so that macro will run
for any triskin subskin oedema: gen X = .

*** set sampling wgtghts to negative to make the prevalence
*** calculations blow up -- impossible to run that piece of 
*** code w/o Stata SE b/c requires 10,000 variables
gen sw = -10
desc sw
summ sw


*---------------------------------------------
* Save a temporary dataset and run -igrowup-
* (note: igrowup adds variables to the data in
* memory and saves a copy dataset with the 
* suffix "_z_st.dta"). Dataset name must correspond
* to the "datalab" variable (defined in last chunk)
*---------------------------------------------

*save $tempdir/TEMPanthro, replace
save "~/TEMPanthro", replace

#delimit;
igrowup_standard reflib datalib datalab sex age ageunit weight lenhei measure headc armc triskin subskin oedema sw;
#delimit cr


*navigate to where this file saved

cd "~/Desktop/stata/WBB-EED-analysis/data/"

*rename output files using MAC shell commands
! mv "temp\TEMPanthro_z_st.dta" "TEMPanthro_z_st.dta"
! mv "temp\TEMPanthro_z_st.xls" "TEMPanthro_z_st.xls"



*---------------------------------------------
* Retrieve WHO calculated output
* "_f" variables identify variables outside of
* reasonable bounds
*
* merge back to the main anthro dataset
*---------------------------------------------
use "~/Desktop/stata/WBB-EED-analysis/data/TEMPanthro_z_st.dta", clear

rename aged ageday_t1

rename q7 weight1_t1
rename q8 weight2_t1
rename q9 weight3_t1

rename q13 lenhei1_t1
rename q14 lenhei2_t1
rename q15 lenhei3_t1

rename q16 headc1_t1
rename q17 headc2_t1
rename q18 headc3_t1

rename q19 muac1_t1
rename q20 muac2_t1
rename q21 muac3_t1

*rename medians
rename lenhei lenhei_med_t1
rename weight weight_med_t1
rename headc headc_med_t1
rename armc armc_med_t1


keep childid sex dob anthrodate_at1 ageday_t1 lenhei_med_t1 weight_med_t1 headc_med_t1 armc_med_t1 weight1_t1 weight2_t1 weight3_t1 lenhei1_t1 lenhei2_t1 lenhei3_t1 headc1_t1 headc2_t1 headc3_t1 muac1_t1 muac2_t1 muac3_t1 _zwei _zlen _zbmi _zwfl _zhc _fwei _flen _fbmi _fwfl _fhc _zac _fac
sort childid 
save  "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro_t1.dta", replace



*use "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/washb-working-anthro.dta", clear
*sort childid 
*merge 1:1 childid using "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro.dta"
*assert _merge==3
*drop _merge


* compare measurements from the 2 packages to ensure they are identical
* (correlation = 1)
*corr haz06 _zlen 
*corr waz06 _zwei
*corr whz06 _zwfl
*corr bmiz06 _zbmi


* delete tempfiles
*erase "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/washb-working-anthro.dta"
erase TEMPanthro_z_st.dta
erase TEMPanthro_z_st.xls

*erase "~/dropbox/washb-bangladesh-data/1-primary-outcome-datasets/TEMPanthro_z_st.xls"




*--------------------------------------------
* Set extreme values to missing and flag them
* based on the WHO 2006 standards
*--------------------------------------------
rename _zlen laz
rename _zwei waz
rename _zwfl whz
rename _zbmi bmiz
rename _zhc hcz
rename _zac acz

gen laz_x = (laz < -6 | laz >6)
	replace laz_x = . if laz==.
	label var laz_x "abs(LAZ)>6, set to missing"
	
gen waz_x = (waz < -6 | waz >5)
	replace waz_x = . if waz==.
	label var waz_x "WAZ < -6 or WAZ > 5, set to missing"

gen whz_x = (whz < -5 | whz >5)
	replace whz_x = . if whz==.
	label var whz_x "abs(WHZ)>5, set to missing"
	
gen bmiz_x = (bmiz < -5 | bmiz >5)
	replace bmiz_x = . if bmiz==.
	label var bmiz_x "abs(BMIZ)>5, set to missing"

gen hcz_x = _fhc
	replace hcz_x = . if hcz==.
	label var hcz_x "abs(HCZ)>5, set to missing"
	
gen acz_x = _fac
	replace acz_x = . if acz==.
	label var acz_x "abs(ACZ)>5, set to missing"
	
* list extreme values before setting them to missing
list childid laz waz whz if laz_x==1
list childid laz waz whz if waz_x==1
list childid laz waz whz if whz_x==1
list childid laz waz whz if bmiz_x==1
list childid hcz if hcz_x==1
list childid acz if acz_x==1

replace laz = . if laz_x==1
replace waz = . if waz_x==1
replace whz = . if whz_x==1
replace bmiz = . if bmiz_x==1
replace hcz = . if hcz_x==1
replace acz = . if acz_x==1




*--------------------------------------------
* Identify children who are stunted, 
* underweight, or wasted based on their Z-scores
*--------------------------------------------

gen byte lazminus2 = laz < -2
	replace lazminus2 =. if laz==. | laz_x==1
	label var lazminus2 "Stunted (LAZ<-2)"
gen byte lazminus3 = laz < -3
	replace lazminus3 =. if laz==. | laz_x==1
	label var lazminus3 "Severely Stunted (LAZ<-3)"

gen byte wazminus2 = waz < -2
	replace wazminus2 =. if waz==. | waz_x==1
	label var wazminus2 "Underweight (WAZ<-2)"
gen byte wazminus3 = waz < -3
	replace wazminus3 =. if waz==. | waz_x==1
	label var wazminus3 "Severely Underweight (WAZ<-3)"

gen byte whzminus2 = whz < -2
	replace whzminus2 =. if whz==. | whz_x==1
	label var whzminus2 "Wasted (WHZ<-2)"
gen byte whzminus3 = whz < -3
	replace whzminus3 =. if whz==. | whz_x==1
	label var whzminus3 "Severely Wasted (WHZ<-3)"
	
	
	
*--------------------------------------------
* Save an analysis dataset
*--------------------------------------------

label var childid "Child ID"



* restrict to variables used in the analysis
keep childid sex dob anthrodate_at1 ageday_t1 lenhei_med_t1 weight_med_t1 headc_med_t1 armc_med_t1 weight1_t1 weight2_t1 weight3_t1 lenhei1_t1 lenhei2_t1 lenhei3_t1 headc1_t1 headc2_t1 headc3_t1 muac1_t1 muac2_t1 muac3_t1 laz* waz* whz* bmiz* hcz* acz*
order childid sex dob laz* waz* whz* bmiz* hcz* acz* ageday_t1 weight1_t1 weight2_t1 weight3_t1 lenhei1_t1 lenhei2_t1 lenhei3_t1 headc1_t1 headc2_t1 headc3_t1 muac1_t1 muac2_t1 muac3_t1

save  "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro_t1.dta", replace


	
clear


*--------------------------------------------
* Rename variables from Anthro data Forms @ T2
*--------------------------------------------


use "~/Desktop/stata/WBB-EED-analysis/data/untouched/Midline/Anthro_Midline_Cleaned_MatchedwEnrollment_26Jan16.dta", clear

drop q6


rename childno childno_at2
rename dataid dataid_at2
rename faid faid_at2
rename samplecoldate samplecoldate_t2

destring clusterid, replace

*generate new variable to include twins w/ same dataid
egen childid = concat(dataid_at2 childno_at2)

sort childid 
quietly by childid:  gen dup = cond(_N==1,0,_n)
list childid samplecoldate_t2 if dup>=1


* convert date of sample collection" variables from string to numeric variables for checking age values
gen samplecoldate_fulldate_t2 = date(samplecoldate_t2, "DMY") 
	format samplecoldate_fulldate_t2 %d

*rename and distinguish variables with anthro identifier
rename samplecoldate_fulldate_t2 samplecoldate_at2

merge 1:1 childid using $tempdir/washb-bangladesh-anthro-diar-ee-med-blind-tr-anthro.dta

keep if _merge==3

drop _merge


*--------------------------------------------
* Age, using measurement date and birth date @ T2
*--------------------------------------------



gen anthrodate = samplecoldate_at2
	format anthrodate %d
	label var anthrodate "Date of anthro measurement t2"


gen aged = anthrodate-dob
	label var aged "Age in days (anthro meas t2)"
gen double agem = aged/30.4167
	label var agem "Age in months (anthro meas t2)"
gen double agey = aged/365.25
	label var agey "Age in years (anthro meas t2)"
codebook agey

* Month of measurement
gen month = month(anthrodate)
	label var month "Month of measurement"
	

rename anthrodate anthrodate_at2

*--------------------------------------------
* ensure that all the anthro measurements 
* are rounded to the correct level of precision
* no more than 2 decimal places
*--------------------------------------------

for any q7 q8 q9 q13 q14 q15 q16 q17 q18 q19 q20 q21 : destring X, replace
for any q7 q8 q9 q13 q14 q15 q16 q17 q18 q19 q20 q21 : replace X = round(X,0.01)

*--------------------------------------------
* Calculate median length measurements
*--------------------------------------------
label var q13 "Child length meas 1"
label var q14 "Child length meas 2"
label var q15 "Child length meas 3"

gen len1 = q13
gen len2 = q14
gen len3 = q15
replace len1 = . if len1>999 | len1<=0
replace len2 = . if len2>999 | len2<=0
replace len3 = . if len3>999 | len3<=0


egen float lenhei = rowmedian(len1 len2 len3)
	replace lenhei = . if lenhei>999
	replace lenhei = round(lenhei,0.001)
	label var lenhei "Child length (median), cm"
	notes lenhei: Median of replicate measures

drop len1-len3

rename q12 q12_t2

gen str measure="L" if q12_t2==1
replace measure="H" if q12_t2==2
replace measure=" " if measure == ""
label var measure "Height measured lying -L- or standing -H-"



*--------------------------------------------
* Calculate median weight measurements
*--------------------------------------------

label var q7 "Child weight meas 1"
label var q8 "Child weight meas 2"
label var q9 "Child weight meas 3"
for any q7 q8 q9 : replace X = . if (X>99 | X<=0)

* create median of mom + child and mom alone
* we have to do it this way because this reflects
* the method used in the field to determine if a 
* 3rd measurement was required


* create median of the child (if measured alone)
egen float wch = rowmedian(q7 q8 q9)

gen float weight = wch
	replace weight = round(weight,0.001)
	label var weight "Child weight (median), kg"
	notes weight: Median of replicate measures

drop  wch



*--------------------------------------------
* Calculate median head circumference measurements
*--------------------------------------------
label var q16 "Head circumference meas 1"
label var q17 "Head circumference meas 2"
label var q18 "Head circumference meas 3"

gen hc1 = q16
gen hc2 = q17
gen hc3 = q18
replace hc1 = . if hc1>99 | hc1<=0
replace hc2 = . if hc2>99 | hc2<=0
replace hc3 = . if hc3>99 | hc3<=0

egen float headc = rowmedian(hc1 hc2 hc3)
	replace headc = . if headc>99
	replace headc = round(headc,0.001)
	label var headc "Child head circumference (median), cm"
	notes headc: Median of replicate measures





*--------------------------------------------
* Calculate median mid upper arm circumference measurements
*--------------------------------------------


label var q19 "Mid upper arm circumference meas 1"
label var q20 "Mid upper arm circumference meas 2"
label var q21 "Mid upper arm circumference meas 3"

gen mc1 = q19
gen mc2 = q20
gen mc3 = q21
replace mc1 = . if mc1>99 | mc1<=0
replace mc2 = . if mc2>99 | mc2<=0
replace mc3 = . if mc3>99 | mc3<=0

egen float armc = rowmedian(mc1 mc2 mc3)
	replace armc = . if armc>99
	replace armc = round(armc,0.001)
	label var armc "Mid upper arm circumference (median), cm"
	notes armc: Median of replicate measures
	





*save temp file anthro-ee-t2
save $tempdir/bangladesh-ee-anthro-ee-t2.dta, replace


*--------------------------------------------
* Calculate laz, waz, whz, using the zscore06
* add-on package. do not specify oedema
*--------------------------------------------

*zscore06, a(agem) s(sex) h(lenhei) w(weight) female(0) male(1) measure(q12_t1) recum(1) stand(2)

*save a temporary dataset to merge with the WHO igrowup output
*save "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/washb-working-anthro.dta", replace




*--------------------------------------------
* Calculate laz, waz, whz, and wcz using the 
* WHO -igrowup- macro. 
*
* this is necessary because the zscore06 
* package does not calculate z-scores for 
* head circumference.  we are using both
* approaches because the zscore06 package is
* ostensibly more accurate (see the package's documentation)
* though as demonstrated below they provide identical results
* (just a good internal validity check)
*
* The WHO -igrowup- macro requires 15 parameters.
* Refer to the documentation for details on this
* finicky macro.
*
* We don't have all of the measurements that
* it processes, so need to create empty
* variables that allow it to run
*---------------------------------------------



/// Indicate to the Stata compiler where the igrowup_standard.ado file is stored ***
adopath + "$whodir"


*** check key variables

gen str reflib="$whodir"
lab var reflib "Directory of reference tables"

gen str datalib="$tempdir"
lab var datalib "Directory for datafiles"

gen str datalab="TEMPanthro"
lab var datalab "Working file"


* create a temporary sex variable for WHO coding
gen whosex = sex
	replace whosex = 2 if sex==0

	drop sex
rename whosex sex

describe sex
	summarize sex

describe headc
summarize headc

describe armc
summarize armc


describe weight
summarize weight

describe lenhei
summarize lenhei

gen age=aged
	describe age
	summarize age


gen str ageunit="days"




describe measure
summarize measure



*** create missing variables so that macro will run
for any triskin subskin oedema: gen X = .

*** set sampling wgtghts to negative to make the prevalence
*** calculations blow up -- impossible to run that piece of 
*** code w/o Stata SE b/c requires 10,000 variables
gen sw = -10
desc sw
summ sw


*---------------------------------------------
* Save a temporary dataset and run -igrowup-
* (note: igrowup adds variables to the data in
* memory and saves a copy dataset with the 
* suffix "_z_st.dta"). Dataset name must correspond
* to the "datalab" variable (defined in last chunk)
*---------------------------------------------


*save $tempdir/TEMPanthro, replace
save "~/TEMPanthro", replace

#delimit;
igrowup_standard reflib datalib datalab sex age ageunit weight lenhei measure headc armc triskin subskin oedema sw;
#delimit cr


*navigate to where this file saved

cd "~/Desktop/stata/WBB-EED-analysis/data/"

*rename output files using MAC shell commands
! mv "temp\TEMPanthro_z_st.dta" "TEMPanthro_z_st.dta"
! mv "temp\TEMPanthro_z_st.xls" "TEMPanthro_z_st.xls"



*---------------------------------------------
* Retrieve WHO calculated output
* "_f" variables identify variables outside of
* reasonable bounds
*
* merge back to the main anthro dataset
*---------------------------------------------
use "~/Desktop/stata/WBB-EED-analysis/data/TEMPanthro_z_st.dta", clear

rename aged ageday_t2

rename q7 weight1_t2
rename q8 weight2_t2
rename q9 weight3_t2

rename q13 lenhei1_t2
rename q14 lenhei2_t2
rename q15 lenhei3_t2

rename q16 headc1_t2
rename q17 headc2_t2
rename q18 headc3_t2

rename q19 muac1_t2
rename q20 muac2_t2
rename q21 muac3_t2

*rename medians
rename lenhei lenhei_med_t2
rename weight weight_med_t2
rename headc headc_med_t2
rename armc armc_med_t2

keep childid sex dob anthrodate_at2 ageday_t2 lenhei_med_t2 weight_med_t2 headc_med_t2 armc_med_t2 weight1_t2 weight2_t2 weight3_t2 lenhei1_t2 lenhei2_t2 lenhei3_t2 headc1_t2 headc2_t2 headc3_t2 muac1_t2 muac2_t2 muac3_t2 _zwei _zlen _zbmi _zwfl _zhc _fwei _flen _fbmi _fwfl _fhc _zac _fac
sort childid 
save  "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro_t2.dta", replace



*use "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/washb-working-anthro.dta", clear
*sort childid 
*merge 1:1 childid using "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro.dta"
*assert _merge==3
*drop _merge


* compare measurements from the 2 packages to ensure they are identical
* (correlation = 1)
*corr haz06 _zlen 
*corr waz06 _zwei
*corr whz06 _zwfl
*corr bmiz06 _zbmi


* delete tempfiles
*erase "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/washb-working-anthro.dta"
erase TEMPanthro_z_st.dta
erase TEMPanthro_z_st.xls

*erase "~/dropbox/washb-bangladesh-data/1-primary-outcome-datasets/TEMPanthro_z_st.xls"



*--------------------------------------------
* Set extreme values to missing and flag them
* based on the WHO 2006 standards
*--------------------------------------------
rename _zlen laz
rename _zwei waz
rename _zwfl whz
rename _zbmi bmiz
rename _zhc hcz
rename _zac acz

gen laz_x = (laz < -6 | laz >6)
	replace laz_x = . if laz==.
	label var laz_x "abs(LAZ)>6, set to missing"
	
gen waz_x = (waz < -6 | waz >5)
	replace waz_x = . if waz==.
	label var waz_x "WAZ < -6 or WAZ > 5, set to missing"

gen whz_x = (whz < -5 | whz >5)
	replace whz_x = . if whz==.
	label var whz_x "abs(WHZ)>5, set to missing"
	
gen bmiz_x = (bmiz < -5 | bmiz >5)
	replace bmiz_x = . if bmiz==.
	label var bmiz_x "abs(BMIZ)>5, set to missing"

gen hcz_x = _fhc
	replace hcz_x = . if hcz==.
	label var hcz_x "abs(HCZ)>5, set to missing"
	
gen acz_x = _fac
	replace acz_x = . if acz==.
	label var acz_x "abs(ACZ)>5, set to missing"
	
* list extreme values before setting them to missing
list childid laz waz whz if laz_x==1
list childid laz waz whz if waz_x==1
list childid laz waz whz if whz_x==1
list childid laz waz whz if bmiz_x==1
list childid hcz if hcz_x==1
list childid acz if acz_x==1

replace laz = . if laz_x==1
replace waz = . if waz_x==1
replace whz = . if whz_x==1
replace bmiz = . if bmiz_x==1
replace hcz = . if hcz_x==1
replace acz = . if acz_x==1




*--------------------------------------------
* Identify children who are stunted, 
* underweight, or wasted based on their Z-scores
*--------------------------------------------

gen byte lazminus2 = laz < -2
	replace lazminus2 =. if laz==. | laz_x==1
	label var lazminus2 "Stunted (LAZ<-2)"
gen byte lazminus3 = laz < -3
	replace lazminus3 =. if laz==. | laz_x==1
	label var lazminus3 "Severely Stunted (LAZ<-3)"

gen byte wazminus2 = waz < -2
	replace wazminus2 =. if waz==. | waz_x==1
	label var wazminus2 "Underweight (WAZ<-2)"
gen byte wazminus3 = waz < -3
	replace wazminus3 =. if waz==. | waz_x==1
	label var wazminus3 "Severely Underweight (WAZ<-3)"

gen byte whzminus2 = whz < -2
	replace whzminus2 =. if whz==. | whz_x==1
	label var whzminus2 "Wasted (WHZ<-2)"
gen byte whzminus3 = whz < -3
	replace whzminus3 =. if whz==. | whz_x==1
	label var whzminus3 "Severely Wasted (WHZ<-3)"
	
	
	
*--------------------------------------------
* Save an analysis dataset
*--------------------------------------------

label var childid "Child ID"


* restrict to variables used in the analysis
keep childid sex dob anthrodate_at2 ageday_t2 lenhei_med_t2 weight_med_t2 headc_med_t2 armc_med_t2 weight1_t2 weight2_t2 weight3_t2 lenhei1_t2 lenhei2_t2 lenhei3_t2 headc1_t2 headc2_t2 headc3_t2 muac1_t2 muac2_t2 muac3_t2 laz* waz* whz* bmiz* hcz* acz*
order childid sex dob laz* waz* whz* bmiz* hcz* acz* ageday_t2 weight1_t2 weight2_t2 weight3_t2 lenhei1_t2 lenhei2_t2 lenhei3_t2 headc1_t2 headc2_t2 headc3_t2 muac1_t2 muac2_t2 muac3_t2

save  "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro_t2.dta", replace


clear

*--------------------------------------------
* Rename variables from Anthro data Forms @ T3
*--------------------------------------------

use "~/Desktop/stata/WBB-EED-analysis/data/untouched/Endline/EE_Endline_Anthro_CLEANED_data_26June2016.dta", clear

drop q6


rename childNo childno_at3
rename dataid dataid_at3
rename FaId faid_at3
rename SampleColDate samplecoldate_t3

destring clusterid, replace


*generate new variable to include twins w/ same dataid
egen childid = concat(dataid_at3 childno_at3)

sort childid 
quietly by childid:  gen dup = cond(_N==1,0,_n)
list childid samplecoldate_t3 if dup>=1


* convert date of sample collection" variables from string to numeric variables for checking age values
gen samplecoldate_fulldate_t3 = date(samplecoldate_t3, "DMY") 
	format samplecoldate_fulldate_t3 %d

*rename and distinguish variables with anthro identifier
rename samplecoldate_fulldate_t3 samplecoldate_at3

merge 1:1 childid using $tempdir/washb-bangladesh-anthro-diar-ee-med-blind-tr-anthro.dta

keep if _merge==3

drop _merge

*--------------------------------------------
* Age, using measurement date and birth date @ T3
*--------------------------------------------



gen anthrodate = samplecoldate_at3
	format anthrodate %d
	label var anthrodate "Date of anthro measurement t3"


gen aged = anthrodate-dob
	label var aged "Age in days (anthro meas t3)"
gen double agem = aged/30.4167
	label var agem "Age in months (anthro meas t3)"
gen double agey = aged/365.25
	label var agey "Age in years (anthro meas t3)"
codebook agey

* Month of measurement
gen month = month(anthrodate)
	label var month "Month of measurement"
	
rename anthrodate anthrodate_at3

*--------------------------------------------
* ensure that all the anthro measurements 
* are rounded to the correct level of precision
* no more than 2 decimal places
*--------------------------------------------

for any q7 q8 q9 q13 q14 q15 q16 q17 q18 q19 q20 q21 : destring X, replace
for any q7 q8 q9 q13 q14 q15 q16 q17 q18 q19 q20 q21 : replace X = round(X,0.01)

*--------------------------------------------
* Calculate median length measurements
*--------------------------------------------
label var q13 "Child length meas 1"
label var q14 "Child length meas 2"
label var q15 "Child length meas 3"

gen len1 = q13
gen len2 = q14
gen len3 = q15
replace len1 = . if len1>999 | len1<=0
replace len2 = . if len2>999 | len2<=0
replace len3 = . if len3>999 | len3<=0


egen float lenhei = rowmedian(len1 len2 len3)
	replace lenhei = . if lenhei>999
	replace lenhei = round(lenhei,0.001)
	label var lenhei "Child length (median), cm"
	notes lenhei: Median of replicate measures

drop len1-len3

rename q12 q12_t3


gen str measure="L" if q12_t3==1
replace measure="H" if q12_t3==2
replace measure=" " if measure == ""
label var measure "Height measured lying -L- or standing -H-"




*--------------------------------------------
* Calculate median weight measurements
*--------------------------------------------

label var q7 "Child weight meas 1"
label var q8 "Child weight meas 2"
label var q9 "Child weight meas 3"
for any q7 q8 q9 : replace X = . if (X>99 | X<=0)

* create median of mom + child and mom alone
* we have to do it this way because this reflects
* the method used in the field to determine if a 
* 3rd measurement was required


* create median of the child (if measured alone)
egen float wch = rowmedian(q7 q8 q9)

gen float weight = wch
	replace weight = round(weight,0.001)
	label var weight "Child weight (median), kg"
	notes weight: Median of replicate measures

drop  wch



*--------------------------------------------
* Calculate median head circumference measurements
*--------------------------------------------
label var q16 "Head circumference meas 1"
label var q17 "Head circumference meas 2"
label var q18 "Head circumference meas 3"

gen hc1 = q16
gen hc2 = q17
gen hc3 = q18
replace hc1 = . if hc1>99 | hc1<=0
replace hc2 = . if hc2>99 | hc2<=0
replace hc3 = . if hc3>99 | hc3<=0

egen float headc = rowmedian(hc1 hc2 hc3)
	replace headc = . if headc>99
	replace headc = round(headc,0.001)
	label var headc "Child head circumference (median), cm"
	notes headc: Median of replicate measures
	


*--------------------------------------------
* Calculate median mid upper arm circumference measurements
*--------------------------------------------


label var q19 "Mid upper arm circumference meas 1"
label var q20 "Mid upper arm circumference meas 2"
label var q21 "Mid upper arm circumference meas 3"

gen mc1 = q19
gen mc2 = q20
gen mc3 = q21
replace mc1 = . if mc1>99 | mc1<=0
replace mc2 = . if mc2>99 | mc2<=0
replace mc3 = . if mc3>99 | mc3<=0

egen float armc = rowmedian(mc1 mc2 mc3)
	replace armc = . if armc>99
	replace armc = round(armc,0.001)
	label var armc "Mid upper arm circumference (median), cm"
	notes armc: Median of replicate measures
	




*save temp file anthro-ee-t3
save $tempdir/bangladesh-ee-anthro-ee-t3.dta, replace


*--------------------------------------------
* Calculate laz, waz, whz, using the zscore06
* add-on package. do not specify oedema
*--------------------------------------------

*zscore06, a(agem) s(sex) h(lenhei) w(weight) female(0) male(1) measure(q12_t1) recum(1) stand(2)

*save a temporary dataset to merge with the WHO igrowup output
*save "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/washb-working-anthro.dta", replace




*--------------------------------------------
* Calculate laz, waz, whz, and wcz using the 
* WHO -igrowup- macro. 
*
* this is necessary because the zscore06 
* package does not calculate z-scores for 
* head circumference.  we are using both
* approaches because the zscore06 package is
* ostensibly more accurate (see the package's documentation)
* though as demonstrated below they provide identical results
* (just a good internal validity check)
*
* The WHO -igrowup- macro requires 15 parameters.
* Refer to the documentation for details on this
* finicky macro.
*
* We don't have all of the measurements that
* it processes, so need to create empty
* variables that allow it to run
*---------------------------------------------



/// Indicate to the Stata compiler where the igrowup_standard.ado file is stored ***
adopath + "$whodir"


*** check key variables

gen str reflib="$whodir"
lab var reflib "Directory of reference tables"

gen str datalib="$tempdir"
lab var datalib "Directory for datafiles"

gen str datalab="TEMPanthro"
lab var datalab "Working file"


* create a temporary sex variable for WHO coding
gen whosex = sex
	replace whosex = 2 if sex==0

	drop sex
rename whosex sex

describe sex
	summarize sex

describe headc
summarize headc

describe armc
summarize armc


describe weight
summarize weight

describe lenhei
summarize lenhei

gen age=aged
	describe age
	summarize age


gen str ageunit="days"




describe measure
summarize measure



*** create missing variables so that macro will run
for any triskin subskin oedema: gen X = .

*** set sampling wgtghts to negative to make the prevalence
*** calculations blow up -- impossible to run that piece of 
*** code w/o Stata SE b/c requires 10,000 variables
gen sw = -10
desc sw
summ sw


*---------------------------------------------
* Save a temporary dataset and run -igrowup-
* (note: igrowup adds variables to the data in
* memory and saves a copy dataset with the 
* suffix "_z_st.dta"). Dataset name must correspond
* to the "datalab" variable (defined in last chunk)
*---------------------------------------------


*save $tempdir/TEMPanthro, replace
save "~/TEMPanthro", replace

#delimit;
igrowup_standard reflib datalib datalab sex age ageunit weight lenhei measure headc armc triskin subskin oedema sw;
#delimit cr


*navigate to where this file saved

cd "~/Desktop/stata/WBB-EED-analysis/data/"

*rename output files using MAC shell commands
! mv "temp\TEMPanthro_z_st.dta" "TEMPanthro_z_st.dta"
! mv "temp\TEMPanthro_z_st.xls" "TEMPanthro_z_st.xls"



*---------------------------------------------
* Retrieve WHO calculated output
* "_f" variables identify variables outside of
* reasonable bounds
*
* merge back to the main anthro dataset
*---------------------------------------------
use "~/Desktop/stata/WBB-EED-analysis/data/TEMPanthro_z_st.dta", clear

rename aged ageday_t3

rename q7 weight1_t3
rename q8 weight2_t3
rename q9 weight3_t3

rename q13 lenhei1_t3
rename q14 lenhei2_t3
rename q15 lenhei3_t3

rename q16 headc1_t3
rename q17 headc2_t3
rename q18 headc3_t3

rename q19 muac1_t3
rename q20 muac2_t3
rename q21 muac3_t3

*rename medians
rename lenhei lenhei_med_t3
rename weight weight_med_t3
rename headc headc_med_t3
rename armc armc_med_t3

keep childid sex dob anthrodate_at3 ageday_t3 lenhei_med_t3 weight_med_t3 headc_med_t3 armc_med_t3 weight1_t3 weight2_t3 weight3_t3 lenhei1_t3 lenhei2_t3 lenhei3_t3 headc1_t3 headc2_t3 headc3_t3 muac1_t3 muac2_t3 muac3_t3 _zwei _zlen _zbmi _zwfl _zhc _fwei _flen _fbmi _fwfl _fhc _zac _fac
sort childid 
save  "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro_t3.dta", replace



*use "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/washb-working-anthro.dta", clear
*sort childid 
*merge 1:1 childid using "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro.dta"
*assert _merge==3
*drop _merge


* compare measurements from the 2 packages to ensure they are identical
* (correlation = 1)
*corr haz06 _zlen 
*corr waz06 _zwei
*corr whz06 _zwfl
*corr bmiz06 _zbmi


* delete tempfiles
*erase "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/washb-working-anthro.dta"
erase TEMPanthro_z_st.dta
erase TEMPanthro_z_st.xls

*erase "~/dropbox/washb-bangladesh-data/1-primary-outcome-datasets/TEMPanthro_z_st.xls"



*--------------------------------------------
* Set extreme values to missing and flag them
* based on the WHO 2006 standards
*--------------------------------------------
rename _zlen laz
rename _zwei waz
rename _zwfl whz
rename _zbmi bmiz
rename _zhc hcz
rename _zac acz

gen laz_x = (laz < -6 | laz >6)
	replace laz_x = . if laz==.
	label var laz_x "abs(LAZ)>6, set to missing"
	
gen waz_x = (waz < -6 | waz >5)
	replace waz_x = . if waz==.
	label var waz_x "WAZ < -6 or WAZ > 5, set to missing"

gen whz_x = (whz < -5 | whz >5)
	replace whz_x = . if whz==.
	label var whz_x "abs(WHZ)>5, set to missing"
	
gen bmiz_x = (bmiz < -5 | bmiz >5)
	replace bmiz_x = . if bmiz==.
	label var bmiz_x "abs(BMIZ)>5, set to missing"

gen hcz_x = _fhc
	replace hcz_x = . if hcz==.
	label var hcz_x "abs(HCZ)>5, set to missing"
	
gen acz_x = _fac
	replace acz_x = . if acz==.
	label var acz_x "abs(ACZ)>5, set to missing"
	
* list extreme values before setting them to missing
list childid laz waz whz if laz_x==1
list childid laz waz whz if waz_x==1
list childid laz waz whz if whz_x==1
list childid laz waz whz if bmiz_x==1
list childid hcz if hcz_x==1
list childid acz if acz_x==1

replace laz = . if laz_x==1
replace waz = . if waz_x==1
replace whz = . if whz_x==1
replace bmiz = . if bmiz_x==1
replace hcz = . if hcz_x==1
replace acz = . if acz_x==1




*--------------------------------------------
* Identify children who are stunted, 
* underweight, or wasted based on their Z-scores
*--------------------------------------------

gen byte lazminus2 = laz < -2
	replace lazminus2 =. if laz==. | laz_x==1
	label var lazminus2 "Stunted (LAZ<-2)"
gen byte lazminus3 = laz < -3
	replace lazminus3 =. if laz==. | laz_x==1
	label var lazminus3 "Severely Stunted (LAZ<-3)"

gen byte wazminus2 = waz < -2
	replace wazminus2 =. if waz==. | waz_x==1
	label var wazminus2 "Underweight (WAZ<-2)"
gen byte wazminus3 = waz < -3
	replace wazminus3 =. if waz==. | waz_x==1
	label var wazminus3 "Severely Underweight (WAZ<-3)"

gen byte whzminus2 = whz < -2
	replace whzminus2 =. if whz==. | whz_x==1
	label var whzminus2 "Wasted (WHZ<-2)"
gen byte whzminus3 = whz < -3
	replace whzminus3 =. if whz==. | whz_x==1
	label var whzminus3 "Severely Wasted (WHZ<-3)"
	
	
	
*--------------------------------------------
* Save an analysis dataset
*--------------------------------------------

label var childid "Child ID"


* restrict to variables used in the analysis
keep childid sex dob anthrodate_at3 ageday_t3 lenhei_med_t3 weight_med_t3 headc_med_t3 armc_med_t3 weight1_t3 weight2_t3 weight3_t3 lenhei1_t3 lenhei2_t3 lenhei3_t3 headc1_t3 headc2_t3 headc3_t3 muac1_t3 muac2_t3 muac3_t3 laz* waz* whz* bmiz* hcz* acz*
order childid sex dob ageday_t3 weight1_t3 weight2_t3 weight3_t3 lenhei1_t3 lenhei2_t3 lenhei3_t3 headc1_t3 headc2_t3 headc3_t3 muac1_t3 muac2_t3 muac3_t3 laz* waz* whz* bmiz* hcz* acz*

save  "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro_t3.dta", replace



clear







*---------------------------------------------
* Merge anthro baseline midline and endline
*---------------------------------------------


* Load input file
use "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro_t1.dta", clear

rename waz waz_t1
rename laz laz_t1
rename bmiz bmiz_t1
rename whz whz_t1
rename hcz hcz_t1
rename acz acz_t1

rename laz_x laz_x_t1
rename lazminus2 lazminus2_t1
rename lazminus3 lazminus3_t1

rename waz_x waz_x_t1
rename wazminus2 wazminus2_t1
rename wazminus3 wazminus3_t1

rename whz_x whz_x_t1
rename whzminus2 whzminus2_t1
rename whzminus3 whzminus3_t1

rename bmiz_x bmiz_x_t1
rename hcz_x hcz_x_t1
rename acz_x acz_x_t1


merge 1:1 childid using "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro_t2.dta" 

drop _merge


rename waz waz_t2
rename laz laz_t2
rename bmiz bmiz_t2
rename whz whz_t2
rename hcz hcz_t2
rename acz acz_t2

rename laz_x laz_x_t2
rename lazminus2 lazminus2_t2
rename lazminus3 lazminus3_t2

rename waz_x waz_x_t2
rename wazminus2 wazminus2_t2
rename wazminus3 wazminus3_t2

rename whz_x whz_x_t2
rename whzminus2 whzminus2_t2
rename whzminus3 whzminus3_t2

rename bmiz_x bmiz_x_t2
rename hcz_x hcz_x_t2
rename acz_x acz_x_t2


merge 1:1 childid using "~/Desktop/stata/WBB-EED-analysis/data/temp/anthro-datasets/TEMPanthro_t3.dta"

drop _merge

rename waz waz_t3
rename laz laz_t3
rename bmiz bmiz_t3
rename whz whz_t3
rename hcz hcz_t3
rename acz acz_t3

rename laz_x laz_x_t3
rename lazminus2 lazminus2_t3
rename lazminus3 lazminus3_t3

rename waz_x waz_x_t3
rename wazminus2 wazminus2_t3
rename wazminus3 wazminus3_t3

rename whz_x whz_x_t3
rename whzminus2 whzminus2_t3
rename whzminus3 whzminus3_t3

rename bmiz_x bmiz_x_t3
rename hcz_x hcz_x_t3
rename acz_x acz_x_t3


*create covariate of time in days between growth measurements
gen anthro_days_btwn_t2_t3 = anthrodate_at3 - anthrodate_at2
	label var anthro_days_btwn_t2_t3 "days between 2 measurements of anthro Year 1 and Year 2"

gen anthro_months_btwn_t2_t3 = anthro_days_btwn_t2_t3 / 30.4375
	label var anthro_months_btwn_t2_t3 "months between 2 measurements of anthro Year 1 and Year 2"

*calculate growth velocity outcomes between Y1 and Y2
*child weight velocity (in kg/month)
*length velocity (in cm/month)
*head circumference velocity (in cm/month)

gen len_velocity_t2_t3 = (lenhei_med_t3-lenhei_med_t2) / anthro_months_btwn_t2_t3
	label var len_velocity_t2_t3 "length velocity in cm/month between Year 1 and Year 2"
gen wei_velocity_t2_t3 = (weight_med_t3-weight_med_t2) / anthro_months_btwn_t2_t3
	label var wei_velocity_t2_t3 "weight velocity in kg/month between Year 1 and Year 2"
gen hc_velocity_t2_t3 = (headc_med_t3-headc_med_t2) / anthro_months_btwn_t2_t3 
	label var hc_velocity_t2_t3 "head circumference velocity in cm/month between Year 1 and Year 2"

*change in LAZ, HAZ, WHZ, HeadC Z between Y2 and Y1
gen delta_laz_t2_t3 = laz_t3-laz_t2
	label var delta_laz_t2_t3 "change in laz (length-for-age Z score) between Year 1 and Year 2"
gen delta_waz_t2_t3 = waz_t3-waz_t2
	label var delta_waz_t2_t3 "change in waz (weight-for-age Z score) between Year 1 and Year 2"
gen delta_whz_t2_t3 = whz_t3-whz_t2
	label var delta_whz_t2_t3 "change in whz (weight-for-length Z score) between Year 1 and Year 2"
gen delta_hcz_t2_t3 = hcz_t3-hcz_t2
	label var delta_hcz_t2_t3 "change in hcz (head circumference for age Z score) between Year 1 and Year 2"	
	



*1788 objects
*save final anthro baseline midline endline dataset
save $outdir/bangladesh-dm-ee-anthro-ee.dta, replace



*export csv
outsheet using "~/Dropbox/WBB-EE-analysis/Data/Cleaned/Audrie/bangladesh-dm-ee-anthro-ee.csv", comma replace

clear

*--------------------------------------------------------
* Merge treatment anthro
*--------------------------------------------------------

* Load input file
*use $outdir/bangladesh-dm-ee-anthro-ee.dta, clear

*create numeric cluster id for merging with treatment.dta
*create substring of childid by dropping childno

*gen dataid = substr(childid, 1, 5)
*gen clusterid = substr(childid, 1, 3)
*destring clusterid, replace

*merge m:1 clusterid using $indir/washb-bangladesh-tr.dta

*keep if _merge==3
*drop _merge

*save $outdir/bangladesh-dm-ee-anthro-ee-tr.dta, replace

*clear

* Load input file
*use $outdir/bangladesh-dm-ee-anthro-ee-tr.dta, clear

*sort tr

*by tr: summarize waz_t1 laz_t1 bmiz_t1 whz_t1 hcz_t1 acz_t1
*by tr: summarize waz_t2 laz_t2 bmiz_t2 whz_t2 hcz_t2 acz_t2
*by tr: summarize waz_t3 laz_t3 bmiz_t3 whz_t3 hcz_t3 acz_t3
