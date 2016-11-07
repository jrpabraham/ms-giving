/**********************************************
PROGRAM NAME: 	MS_inport_pilot
AUTHOR: 		Justin Abraham & Nick Otis
PROJECT:		Moral Stock
PURPOSE: 		Import and clean data
INPUT: 			MS_Pilot.csv
OUTPUT: 		MS_Pilot.dta
**********************************************/

*0. Clearing, setting directories, importing data
	clear all
	set maxvar 10000
	set matsize 10000
	set more off
	pause on

	ssc install revrs

	*Pull data
	copy https://raw.githubusercontent.com/jrpabraham/ms-giving/master/MS_Data.csv ., replace

	*Import and label data
	import delimited "MS_Data.csv", varnames(1) clear

	foreach var of varlist * {

		levelsof `var' in 1
		la var `var' `r(levels)'

	}

	drop in 1

*1. Cleaning variables

	*Tracking

	ren v1 responseid
	ren v5 completed

	gen starttime = clock(v3, "YMDhms")
	gen endtime = clock(v4, "YMDhms")
	format starttime endtime %tc

	gen duration = minutes(endtime - starttime) if hours(endtime - starttime) < 24
	la var duration "Session duration (min.)"

	*Demographics

	destring q1 q1_text q2 q3 q4 q5 q6 q7 q8 q9, replace

	ren q1 Lan
	la def lan 1 "English" 2 "Spanish" 3 "Chinese" 4 "French" 5 "Arabic"
	la val Lan lan

	ren q1_text LanOtr

	gen LanEng = Lan == 1

	ren q2 Gen
	recode Gen (1 = 1) (2 = 0) (nonm = .)
	la def gen 0 "Male" 1 "Female"
	label values Gen gen

	gen Age =  2016 - (1920 + q3)

	gen Age30 = Age > 30 if ~mi(Age)

	ren q4 Edu
	la def edu 1 "Primary School" 2 "High School or equivalent" 3 "Vocational/Technical School (2 year)" 4 "Some College" 5 "College Graduate (4 year)" 6 "Master's Degree (MS)" 7 "Doctoral Degree (PhD)" 8 "Professional Degree (MD, JD, etc.)"
	la val Edu edu

	gen Col = Edu > 3 if ~mi(Edu)

	ren q5 Loc
	la def loc 1 "Urban" 2 "Suburban" 3 "Rural"

	gen LocUrb = Loc == 1 if ~mi(Loc)
	gen LocSub = Loc == 2 if ~mi(Loc)

	ren q6 Inc
	la def inc 1 "Under $10,000" 2 "$10,000 - $19,999" 3 "$20,000 - $29,999" 4 "$30,000 - $39,999" 5 "$40,000 - $49,999" 6 "$50,000 - $74,999" 7 "$75,000 - $99,999" 8 "$100,000 - $150,000" 9 "Over $150,000"
	la val Inc inc

	gen IncHigh = Inc > 5 if ~mi(Inc)

	ren q7 Emp
	la def emp 1 "Employed, full time" 2 "Employed, part time" 3 "Retired" 4 "Unemployed"
	la val Emp emp

	gen UnEmp = Emp == 4 if ~mi(Emp)

	ren q8 Rac
	la def rac 1 "Hispanic or Latino" 2 "Black or African American" 3 "American Indian or Alaska Native" 4 "White" 5 "Asian" 6 "Native Hawaiian or Pacific Islander"
	la val Rac rac

	gen Afr = Rac == 2 if ~mi(Rac)

	ren q9 Pol
	recode Pol (1=1) (2=0)
	la def pol 0 "More of a conservative" 1 "More of a liberal"

	*Treatment: Exposure to the question "Are you racist"?
	destring q10, replace
	gen TreExpRac = 1 if q10 !=.
	recode TreExpRac .=0
	label define treexprac 1 "Received questions about racism" 0 "Didn't receive question about racism"
	lab var TreExpRac treexprac

	*Are you Racist
	rename q10 ExpRac
	recode ExpRac (1 = 1) (2 = 0) (nonm = .)
	la def exprac 0 "no" 1 "yes"
	la val ExpRac exprac

	*Treatment: Exposure to IAT implying racism
	destring q13, replace
	rename q13 TreIat
	recode TreIat (2=1)(.=0)
	label define treiat 1 "Received IAT results implying racism" 0 "Didn't receive IAT results implying racism"
	lab var TreIat treiat

	*Affect and self-esteem

	destring q15 q16 q17 q47 q19, replace

	la def affect 1 "Not at all" 2 "A little" 3 "Moderately" 4 "Quite a bit" 5 "Extremely"

	ren q15 NegAfi
	la val NegAfi affect

	ren q16 NegUps
	la val NegUps affect

	ren q17 NegNer
	la val NegNer affect

	ren q47 NegDis
	la val NegDis affect

	egen NegSum = rowtotal(Neg*), m

	ren q19 SelEst

	label define likert 1 "Strongly disagree" 2 "Disagree" 3"Somewhat disagree" 4 "Neither agree nor disagree" 5 "Somewhat agree" 6 "Agree" 7 "Strongly agree"
	la val SelEst likert

	*Charitable donation
	destring q20, replace
	ren q20 ChaDon

	*Trolley problem
	destring q21 q22, replace

	ren q21 TroSwi
	ren q22 TroFat

	la def tro 1 "Yes, divert the trolley" 0 "Do nothing"
	la val TroSwi two
	la val TroFat two

	gen TroRev = TroSwi == 1 & TroFat == 0

	*Policy support

	destring q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34, replace

	*Renaming
	ren q24 RedRes
	ren q25 RedBen
	ren q26 RedObl
	ren q27 RedWel
	ren q28 RedExp
	ren q29 RedDep

	la val Red* likert

	*Reverse code variables to make directions the same across variables
	foreach var in RedRes RedBen RedObl RedWel {
		revrs `var', replace
	}

	*Generate summary variable
	egen RedSum = rowtotal(Red*), m

	ren q31 AffFai
	ren q32 AffEdu
	ren q33 AffHir
	ren q34 AffCom
	la val Aff* likert

	*Reverse code variables
	foreach var in AffEdu AffCom {
		revrs `var', replace
	}

	*Generate summary variable
	egen AffSum = rowtotal(Aff*), m

	*Cleaning variable labels
	label variable Lan       "What is your primary language (i.e., the one you speak most of the time)?"
	label variable LanEng	 "English is primary language"
	label variable LanOtr    "If language is other: specify"
	label variable Gen       "What is your gender?"
	label variable Age30	 "Over 30 years old"
	label variable Edu       "Please indicate the highest level of education you have completed."
	label variable Loc       "Which of the following best describes the area you live in?"
	label variable LocUrb 	 "Lives in urban locality"
	label variable LocSub	 "Lives in suburban locality"
	label variable Inc       "Please indicate your current household income in U.S. dollars"
	label variable IncHigh   "Annual income over USD 50,000"
	label variable Emp       "What best describes your employment status?"
	label variable UnEmp     "Unemployed"
	label variable Rac       "With which racial group do you most identify with?"
	label variable Pol       "Would you describe yourself as politically leaning more liberal or more con"
	label variable ExpRac    "Are you a racist?"
	label variable NegAfi    "To what extent do you feel afraid?"
	label variable NegUps    "To what extent do you feel upset?"
	label variable NegNer    "To what extent do you feel nervous?"
	label variable NegDis    "To what extent do you feel distressed?"
	label variable NegSum	 "Negative affect score"
	label variable SelEst    "On the whole, I am satisfied with myself?"
	label variable ChaDon    "Cents donated to charity"
	label variable TroSwi    "Switch trolley problem"
	label variable TroFat    "Fat man trolley problem"
	label variable TroRev	 "Omission bias"
	label variable RedRes    "As a country's wealth increases, more of its resources should be channeled"
	label variable RedBen    "Giving to others usually benefits the givers as well"
	label variable RedObl    "Those with more resources have more obligations toward their fellow human"
	label variable RedWel    "It is beneficial for all to spend money on the public sector in education"
	label variable RedExp    "Those who are well off cannot be expected to take care of everyone else"
	label variable RedDep    "Charitable organizations just create dependency among the recipient."
	label variable AffFai    "Affirmative action for African Americans is unfair to White Americans"
	label variable AffEdu    "Affirmative action in educations gives an opportunity to qualified African"
	label variable AffHir    "Affirmative action for African Americans may force employers to hire..."
	label variable AffCom    "Affirmative action in the workplace for African Americans helps make..."
	label variable starttime "Study start time"
	label variable endtime   "Study end time"
	label variable duration  "Session duration (min.)"
	label variable Age       "Age in years"
	label variable Col       "Did respondent attend college"
	label variable Afr       "Are you African American?"
	label variable TreExpRac "Respondent is asked explicit racism question"
	label variable RedSum    "Summary of redistributive policy variables"
	label variable AffSum    "Summary of affirmative action variables"
	label variable TreIat    "Exposure to IAT treatment"

	*Removing extraneous variables and ordering
	keep responseid duration Lan LanOtr LanEng Gen Edu Col Age* Loc* Inc* Emp UnEmp Rac Afr Pol TreExpRac ExpRac TreIat NegAfi NegUps NegNer NegDis NegSum SelEst ChaDon TroSwi TroFat TroRev RedRes RedBen RedObl RedWel RedExp RedDep RedSum AffFai AffEdu AffHir AffCom AffSum
	order responseid duration Lan LanOtr LanEng Gen Edu Col Age* Loc* Inc* Emp UnEmp Rac Afr Pol TreExpRac ExpRac TreIat NegAfi NegUps NegNer NegDis NegSum SelEst ChaDon TroSwi TroFat TroRev RedRes RedBen RedObl RedWel RedExp RedDep RedSum AffFai AffEdu AffHir AffCom AffSum


*2. Test for selection

	loc outcomes "ChaDon RedSum AffSum TroRev NegSum SelEst"
	loc controls "LanEng Gen Age30 Col LocUrb LocSub IncHigh UnEmp Afr Pol TreExpRac"
	loc hetvars "Col IncHigh UnEmp Afr Pol TreExpRac"

	reg TreIat `controls', vce(cl responseid)
	reg TreExpRac `controls', vce(cl responseid)

*3. Treatment effect

	foreach control of varlist `controls' {						// Create control vector

		sum `control'
		gen `control'Xtreat = (`control' - `r(mean)') * TreIat
		loc controls "`controls' `control'Xtreat"

	}


	foreach outcome of varlist `outcomes' {

		reg `outcome' TreIat, vce(cl responseid)				// Main treatment effect
		reg `outcome' TreIat `controls', vce(cl responseid)		// Covariate adjustment

		foreach het of varlist `hetvars' {

			reg `outcome' i.TreIat##i.`het', vce(cl responseid)	// Heterogeneity

		}

	}

*4. Randomization inference

	set seed 95594731

	foreach outcome of varlist `outcomes' {

		permute TreIat beta = _b[TreIat], reps(10000): reg `outcome' TreIat, vce(cl responseid)

	}
