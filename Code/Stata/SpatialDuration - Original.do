*Program to Estimate Weibull Spatial Duration Model


clear


pr drop _all
set more off
set matsize 800

***********************
*Likelihood Evaluator
***********************

program define splag_ll_dur
args lnf mu rho lambda
qui replace `lnf'= ln(ones - `rho'*EIGS1) + ln(`lambda') + `lambda'*($ML_y1-`rho'*SL1-`mu') - exp(`lambda'*($ML_y1-`rho'*SL1-`mu'))
end

forvalues i = 1/3 {
**********************
*Open Data For Weights
**********************
if `i' == 1 {
use "C:\Research\Telephone\Code\Stata\Data\WeightsDistanceSqStandard.dta", clear 
}                 
else if `i' == 2 {
use "C:\\Research\\Telephone\\Code\\Stata\\Data\\ProximityWeightStandard.dta", clear 
}                 
else {
use "C:\Research\Telephone\Code\Stata\Data\RegionWeightStandard.dta", clear 
}                 

qui sum V1                                
global nobs = r(N)                         
mkmat V1-V$nobs, matrix(W)
matrix I_n = I($nobs)
matrix eigenvalues eig1 imaginaryv = W
matrix eig2 = eig1'
matrix ones=J($nobs,1,1)
drop _all

**************************
*Open Data for Regression
**************************
drop _all
use "C:\Research\Telephone\Code\Stata\Data\TownsHazardCons.dta", clear
drop if region == "PF"
gen lntiming = ln(installmonth)
global Y lntiming
global X  marketaccess1880 marketsize1880 y1880 city popshare1880 fringe border agriculture empratio82 indexdissim82  statetax localtax railstation railrevenues railweight postrevenues participation zentrum difcatholicszentrum liberal socialist   
mkmat $Y, matrix(Y)
matrix SL = W*Y
svmat SL, n(SL)
svmat eig2, n(EIGS)
svmat ones, n(ones)

************************
*Produce starting values
************************
stset installmonth
streg $X, dist(weibull) time
matrix stregbp=e(b)
local col = colsof(stregbp)
matrix stregb=stregbp[1,1..`col'-1]
matrix coleq stregb = mu
local stregp=exp(stregbp[1,`col'])

***************************
*Estimate spatial lag model
***************************
ml model lf splag_ll_dur (mu: $Y=$X) (rho:) (lambda:)
ml init stregb
ml init rho:_cons=0
ml init lambda:_cons=`stregp'
ml max


if `i' == 1 { 
estimates store Decay 
}
else if `i' == 2 { 
estimates store Band
}
else  { 
estimates store Region
}

//estimates save "C:\Research\Telephone\Code\Stata\Results\Decay.csv", replace
//estimates save "C:\Research\Telephone\Code\Stata\Results\Band.csv", replace
//estimates save "C:\Research\Telephone\Code\Stata\Results\Region.csv", replace
}
label variable marketaccess1880 "Market Access 1880"
label variable marketsize1880 "Market Size 1880"
label variable y1880 "Population 1880"
label variable city "Town Status"
label variable popshare1880 "County Population Share"
label variable fringe "Fringe Region"
label variable border "Border Region"
label variable agriculture "Agriculture"
label variable empratio82 "Employment Ratio 1882"
label variable indexdissim82 "Specialization 1882"
label variable statetax "State Tax"
label variable localtax "Local Tax"
label variable railstation "Railroad Station"
label variable railrevenues "Railroad Revenues"
label variable railweight "Railroad volume"
label variable postrevenues "Postal Revenues"
label variable participation "Election Participation"
label variable zentrum "Zentrum Vote Share"
label variable difcatholicszentrum "Zentrum vs Catholic"
label variable liberal "Liberal Vote Share"
label variable socialist "Socialist Vote Share" 
esttab Decay Band Region using C:\Research\Telephone\Code\Stata\Results\SpatialDurationResults.tex, replace label title(Diffusion of Local Exchanges with Spatial Correlation)  nonumbers mtitles( "Decay" "Band" "Region") 
