** Desc: Scoring program for Qualtrics single target IAT
** Author: Justin Abraham
** Input: String variables of IAT latencies
** Output: Cohens D of mean latencies
** Options:

capture program drop scoreiat
program define scoreiat, eclass
syntax varlist(min=2 max=2) [if] [in], score(str) [GREENwald] [LOWES]

tempvar touse
mark `touse' `if' `in'

tempvar nid if `touse'
gen `nid' = _n

preserve

keep `nid' `varlist'

foreach var in `varlist' {

	qui sum `nid'

	forval i = `r(min)'/`r(max)' {

        qui levelsof `var' in `i'

        if length("`r(levels)'") {

			loc iatstr = subinstr(subinstr(`r(levels)', ",", " ", .), "END", "", .)
			loc iatstr: list sort iatstr

            foreach item in `iatstr' {

				loc qid = string(real(substr("`item'", 1, 2)), "%9.0f")
				loc qlat = real(substr("`item'", 4, .))
				loc qcorrect = substr("`item'", 3, 1)

				cap: gen `var'_lat`qid' = `qlat' if `nid' == `i'
				if _rc != 0 qui replace `var'_lat`qid' = `qlat' if `nid' == `i'

				cap: gen `var'_correct`qid' = "`qcorrect'" if `nid' == `i'
				if _rc != 0 qui replace `var'_correct`qid' = "`qcorrect'" if `nid' == `i'

            }

        }

	}

}

loc root1 = word("`varlist'", 1)
loc root2 = word("`varlist'", 2)

if "`greenwald'" != "" {

	di "Calculating Greenwald et al. (2003) d-scores"

	loc denom = 0

	foreach var of varlist `root1'_lat* `root2'_lat* {

		qui replace `var' = . if `var' > 10000
		loc ++denom

	}

	egen countfast = anycount(`root1'_lat* `root2'_lat*), v(0/299)
	drop if countfast / `denom' > 0.10

}

else if "`lowes'" != "" {

	di "Calculating Lowes et al. (2015) d-scores"

	foreach var of varlist `root1'_lat* `root2'_lat* {

		qui replace `var' = 3000 if `var' > 3000

	}

}

else {

	di "Calculating unadjusted d-scores"

}

egen mean_`root1' = rowmean(`root1'_lat*)
egen mean_`root2' = rowmean(`root2'_lat*)
egen sd_both = rowmean(`root1'_lat* `root2'_lat*)
gen `score' = (mean_`root1' - mean_`root2') / sd_both
la var `score' "D-score of `varlist'"

drop `root1'_* `root2'_*

tempfile iat_dat
save `iat_dat', replace

restore
qui merge 1:1 `nid' using `iat_dat', nogen

end
