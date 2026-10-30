version 17.0
clear all
set more off
set seed 123456

local project "datafile_path"
local datafile "`datafile"
local outdir "`output_path"
local reps 2000
local reps_override : environment FISHER_REPS
if "`reps_override'" != "" local reps = real("`reps_override'")
assert `reps' >= 10

capture mkdir "`outdir'"
capture log close _all
log using "`outdir'\analysis.log", replace text name(Sample1)

import delimited "`datafile'", clear varnames(1)
* N: Number of sample, Sample 1: N=17532, Sample 2: N=1928
assert _N == 17532
* Number of profiles, Sample 1: Profiles=3, Sample 2: Profiles=4
assert inlist(profile, 1, 2, 3)

local covariates "gender bmi domicile onlychild universitytier"

forvalues p = 1/3 {
    regress anxiety mpa `covariates' if profile == `p', robust
    estimates store profile`p'
}

estimates table profile1 profile2 profile3, b(%9.6f) se(%9.6f) stats(N r2 rmse) keep(mpa gender bmi domicile onlychild universitytier)

tempname results_handle
tempfile permutation_results
postfile `results_handle' byte comparison_order str90 comparison double delta_b p_bdiff using `permutation_results', replace

generate byte group_2v1 = .
replace group_2v1 = 0 if profile == 2
replace group_2v1 = 1 if profile == 1
bdiff, group(group_2v1) model(reg anxiety mpa `covariates') reps(`reps') nodots first detail
local p_value = min(1, 2*r(p))
post `results_handle' (1) ("Profile 1 vs Profile 2") (r(bdiff)) (`p_value')
drop group_2v1

generate byte group_2v3 = .
replace group_2v3 = 0 if profile == 2
replace group_2v3 = 1 if profile == 3
bdiff, group(group_2v3) model(reg anxiety mpa `covariates') reps(`reps') nodots first detail
local p_value = min(1, 2*r(p))
post `results_handle' (2) ("Profile 2 vs Profile 3") (r(bdiff)) (`p_value')
drop group_2v3

generate byte group_1v3 = .
replace group_1v3 = 0 if profile == 1
replace group_1v3 = 1 if profile == 3
bdiff, group(group_1v3) model(reg anxiety mpa `covariates') reps(`reps') nodots first detail
local p_value = min(1, 2*r(p))
post `results_handle' (3) ("Profile 1 vs Profile 3") (r(bdiff)) (`p_value')
drop group_1v3

*======If Sample 2=======
generate byte group_1v4 = .
replace group_1v4 = 0 if profile == 1
replace group_1v4 = 1 if profile == 4
bdiff, group(group_1v4) model(reg anxiety mpa `covariates') reps(`reps') nodots first detail
local p_value = min(1, 2*r(p))
post `results_handle' (3) ("Profile 1 vs Profile 4") (r(bdiff)) (`p_value')
drop group_1v4

generate byte group_2v4 = .
replace group_2v4 = 0 if profile == 2
replace group_2v4 = 1 if profile == 4
bdiff, group(group_2v4) model(reg anxiety mpa `covariates') reps(`reps') nodots first detail
local p_value = min(1, 2*r(p))
post `results_handle' (3) ("Profile 2 vs Profile 4") (r(bdiff)) (`p_value')
drop group_2v4

generate byte group_3v4 = .
replace group_3v4 = 0 if profile == 3
replace group_3v4 = 1 if profile == 4
bdiff, group(group_3v4) model(reg anxiety mpa `covariates') reps(`reps') nodots first detail
local p_value = min(1, 2*r(p))
post `results_handle' (3) ("Profile 3 vs Profile 4") (r(bdiff)) (`p_value')
drop group_3v4
*========================

postclose `results_handle'

preserve
use `permutation_results', clear
sort p_bdiff
generate p_holm = min(1, p_bdiff * (4 - _n))
replace p_holm = max(p_holm, p_holm[_n - 1]) in 2/3
* Sample 2: 
* generate p_holm = min(1, p_bdiff * (7 - n))
* replace p_holm = max(p_holm, p_holm[_n-1]) in 2/6
sort comparison_order
format delta_b %9.4f
format p_bdiff %9.4f
list comparison delta_b p_bdiff p_holm, noobs abbreviate(32)
export delimited using "`outdir'\analysis_output.csv", replace
restore

log close Sample1
