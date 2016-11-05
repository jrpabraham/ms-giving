** Desc: Custom program for the aspirations project that creates a key to merge phases of the data by correcting spouse survey issue.
** Input: List of item names constituting a scale/inventory.
** Output: String of selected item names that pass the criteria
** Options: criteria() specifies the number of violations needed to remove an item from the scale, correlation() specifies the min corrected item-total correlation, loading() specifies the minimum low primary loading on a factor, xloading() specifies the maximum cross-loading. 
** Note: Items must already be cleaned and reverse-coded if necessary.

program define _gscaleref, rclass
	gettoken type 0 : 0
	gettoken h    0 : 0 
	gettoken eqs  0 : 0

syntax varlist(min=1) [if] [in] [, min(real 0)] [, max(real 0)] [, CRIteria(real 0)] [, alpha] [, CORRelation(real 0)] [, LOADing(real 0)] [, XLOADing(real 0)] [, EXTreme(real 0)] [, fl(string)] [, PROPortion(real 0)] [, MISSing]

tempvar touse
mark `touse' `if' `in'

qui alpha `varlist' if `touse'
loc alpha = `r(alpha)'

loc svars ""

foreach item of varlist `varlist' {

	loc score = 0

	/* Test Cronbach's alpha */

	if "`alpha'" != "" {

		loc shorter: list varlist - item

		qui alpha `shorter' if `touse'

		if `r(alpha)' > `alpha' {

			di "`item' failed internal consistency"
			loc ++score

		}

	}

	/* Test item correlation */

	if "`correlation'" != "" {

		loc shorter: list varlist - item

		cap: drop `sans'
		tempvar sans
		egen `sans' = rowtotal(`shorter') if `touse'
	 
		qui pwcorr `item' `sans' if `touse'
	
		if `r(rho)' < `correlation' {

			di "`item' failed correlation"
			loc ++score

		}

	}

	/* Test factor loadings */

	if "`loading'" != "" | "`xloading'" != "" {

		cap noi: mat def X = `fl'["`item'", .]

		if _rc {

			di as err "Unable to test factor loading"
			di as err "Unable to test cross-loading"

		}

		else {

			if X[1, 1] < `loading' {

				di "`item' factor loading too low"
				loc ++score

			}

			mat def X = X'
			svmat X, names(matcol)
			qui replace X`item' = . in 1
			qui sum X`item'

			if `r(N)' > 0 {

				if `r(max)' > `xloading' {

					di "`item' failed cross-loading"
					loc ++score

				}

			} 

			else {

				di as err "Unable to test cross-loading"				

			}

			drop X`item'

		}

	}

	/* Test extreme responses */

	if "`min'" != "" & "`max'" != "" & "`proportion'" != "" {

		tempvar flag
		gen `flag' = `item' == `min' | `item' == `max'

		qui sum `flag' if `touse'

		if `r(mean)' >= `proportion' {
			loc ++score
		}

	}

	/* Fill selected items */

	if `score' < `criteria' {
		loc svars "`svars' `item'"
	}

}

loc vcount: list sizeof svars

if `vcount' == 0 {

	di as err "Scale refinement procedure omitted all components"
	gen `typlist' `h' = .

}

else if `vcount' > 0 {

	di "Using`svars' in scale"
	tempvar notfull
	egen `typlist' `notfull' = rowtotal(`svars') if `touse', m

	gen `h' = 0 if `touse'
	qui replace `h' = `notfull' if `touse'

	if "`missing'" != "" {

		tempvar mcount
		egen `mcount' = rowmiss(`varlist') if `touse'

		if (`vcount' > 3 & `vcount' < 6) {
			qui replace `h' = . if `touse' & `mcount' >= 2
		}
		else if (`vcount' > 5 & `vcount' < 9) {
			qui replace `h' = . if `touse' & `mcount' >= 3
		}
		else if (`vcount' > 8) {
			qui replace `h' = . if `touse' & `mcount' >= 4
		}

	}

}

end
