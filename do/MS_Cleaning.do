/**********************************************
PROGRAM NAME: 	MS_inport_pilot
AUTHOR: 		Justin Abraham & Nick Otis
PROJECT:		Moral Stock
PURPOSE: 		Import and clean data
INPUT: 			MS_Pilot.csv
OUTPUT: 		MS_Pilot.dta
**********************************************/

*0. Clearing and setting directories
clear all
set maxvar 10000
set matsize 10000
set more off
pause on

if c(os) == "MacOSX" loc root "/Users/`c(username)'"
else if c(os) == "Windows" loc root "C:\Users\\`c(username)'"
else if c(os) == "Unix" loc root "/usr/`c(username)'"

cd `root'

*Pull data
***Could change the resulting directory**
copy https://github.com/NickOtis/MoralStock/blob/master/TEST.xlsx ., replace


***Remove below, since this will be a "public use" .do file
glo root_dir "`c(pwd)'"
glo ado_dir "$root_dir/MS_Scripts/ado/personal"
glo data_dir "$root_dir/MS_Data"
glo do_dir "$root_dir/MS_Scripts/do"
glo output_dir "$root_dir/MS_Deliverables"
glo fig_dir "$output_dir/MS_Figures"
glo tab_dir "$output_dir/MS_Tables"

sysdir set PERSONAL "$ado_dir"
cap cd "$data_dir"

/* Import and relabel variables */

import delim "$data_dir/MS_RawData/MS_Pilot.csv", varnames(1) clear

foreach var of varlist * {

    levelsof `var' in 1
    la var `var' `r(levels)'

}

drop in 1

/////////////////////
// Clean variables //
/////////////////////

/* Tracking */

destring v5 mturkcode, replace

ren v1 responseid
ren v5 completed

gen starttime = clock(v3, "YMDhms")
gen endtime = clock(v4, "YMDhms")
format starttime endtime %tc

gen duration = minutes(endtime - starttime) if hours(endtime - starttime) < 24
la var duration "Session duration (min.)"

/* Demographics */

destring q10 q12 q14 q16 q18 q20 q22 q24 q40, replace

ren q10 demo_language
la def la_lang 1 "English" 2 "Spanish" 3 "Chinese" 4 "French" 5 "Arabic"
la val demo_language la_lang

ren q12 demo_female
recode demo_female (1 = 1) (2 = 0) (nonm = .)

gen demo_age =  2016 - (1920 + q14)

ren q16 demo_edu
la def la_edu 1 "Primary School" 2 "High School or equivalent" 3 "Vocational/Technical School (2 year)" 4 "Some College" 5 "College Graduate (4 year)" 6 "Master's Degree (MS)" 7 "Doctoral Degree (PhD)" 8 "Professional Degree (MD, JD, etc.)"
la val demo_edu la_edu
gen demo_college = demo_edu > 3

ren q18 demo_loc
tab demo_loc, gen(demo)
ren demo1 demo_urban
ren demo2 demo_suburban
ren demo3 demo_rural

ren q20 demo_income
la def la_income 1 "Under $10,000" 2 "$10,000 - $19,999" 3 "$20,000 - $29,999" 4 "$30,000 - $39,999" 5 "$40,000 - $49,999" 6 "$50,000 - $74,999" 7 "$75,000 - $99,999" 8 "$100,000 - $150,000" 9 "Over $150,000"
la val demo_income la_income

ren q22 demo_empstatus
la def la_emp 1 "Employed, full time" 2 "Employed, part time" 3 "Retired" 4 "Unemployed"
la val demo_empstatus la_emp

ren q24 demo_race
la def la_race 1 "Hispanic or Latino" 2 "Black or African American" 3 "American Indian or Alaska Native" 4 "White" 5 "Asian" 6 "Native Hawaiian or Pacific Islander"
la val demo_race la_race

gen demo_afr = demo_race == 2 if ~mi(demo_race)

recode q40 (1 = 1) (2 = 0) (nonm = .), gen(demo_racist)

/* IAT */

scoreiat part_3 part_5, score("iat_dscore") green

gen iat_treat = 1 if q42 == "1"
replace iat_treat = 0 if q44 == "1"

/* Affect and self-esteem */

destring v25 q45 q46 q47 q41, replace

foreach var of varlist v25 q45 q46 q47 q41 {

    replace `var' = 7 - `var'

}

ren v25 na_afraid
ren q45 na_upset
ren q46 na_nervous
ren q47 na_distressed
ren q41 selfesteem

egen na_total = rowtotal(na_*), m

/* Charitable giving */

destring q48, replace
ren q48 donation

/* Trolley problem */

destring q35 q37, replace

gen tro_switch = 1 if ~mi(q35)
replace tro_switch = 0 if ~mi(q37)

gen tro_response = q35 == 1 if tro_switch == 1
replace tro_response = q37 == 1 if tro_switch == 0

/* Policy support */

destring v34 v35 v36 q26 q28 q30 q34 q36 q38 v44, replace

foreach var of varlist v34 v35 v36 q26 q28 q30 q34 q36 q38 v44 {

    replace `var' = `var' - 1

}

ren v34 dist_1
ren v35 dist_2
ren v36 dist_3
ren q26 dist_4
ren q28 dist_5
ren q30 dist_6
egen dist_total = rowtotal(dist_*), m

ren q34 aa_1
ren q36 aa_2
ren q38 aa_3
ren v44 aa_4
egen aa_total = rowtotal(aa_*), m

/* Save clean data */

keep responseid mturkcode completed starttime endtime duration demo_* iat_* na_* selfesteem donation tro_* aa_* dist_*
order responseid mturkcode completed starttime endtime duration demo_* iat_* na_* selfesteem donation tro_* aa_* dist_*
x
compress
save "$data_dir/MS_CleanData/MS_Pilot.dta", replace
