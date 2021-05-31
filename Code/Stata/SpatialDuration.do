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
*use "C:\Research\Telephone\Code\Stata\Data\WeightsDistanceSqStandard.dta", clear 
use "U:\Telephone\Stata\Data\WeightsDistanceTelStandard.dta", clear 
}                 
else if `i' == 2 {
*use "C:\\Research\\Telephone\\Code\\Stata\\Data\\ProximityWeightStandard.dta", clear 
use "U:\Telephone\Stata\Data\ProximityWeightStandard.dta", clear 
}                 
else {
*use "C:\Research\Telephone\Code\Stata\Data\RegionWeightStandard.dta", clear 
use "U:\Telephone\Stata\Data\RegionWeightStandard.dta", clear 
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
*use "C:\Research\Telephone\Code\Stata\Data\TownsHazardCons.dta", clear
use "U:\Telephone\Stata\Data\TownsHazardCons.dta", clear

drop if Region == "PF"
encode Region, generate(Region_factor)

gen lntiming = ln(InstallMonth)

global Y lntiming

global X MA_Post_Out_1880 Border Y1880 PostRevenues_pc TelegraphRevenues_pc Agriculture EmpRatio82 IndexDisSim82 StateTax RailStation RailRevenues Participation  Socialist Zentrum DifCatholicsZentrum City PopShare1880 Fringe


mkmat $Y, matrix(Y)
matrix SL = W*Y
svmat SL, n(SL)
svmat eig2, n(EIGS)
svmat ones, n(ones)

************************
*Produce starting values
************************
stset InstallMonth
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
*estimates save "U:\Telephone\Stata\Results\Decay.csv", replace
}
else if `i' == 2 { 
estimates store Band
*estimates save "U:\Telephone\Stata\Results\Band.csv", replace
}
else  { 
estimates store Region
*estimates save "U:\Telephone\Stata\Results\Region.csv", replace
}
}



label variable MA_Post_Out_1880 "Market Access"
label variable Border "Border Region"
label variable Y1880 "Population"
label variable PostRevenues_pc "Postal Revenues"
label variable TelegraphRevenues_pc "Telegraph Revenues"
label variable Agriculture "Agriculture"
label variable EmpRatio82 "Employment Ratio"
label variable IndexDisSim82 "Specialization"
label variable StateTax "State Tax"
label variable RailStation "Railroad Station"
label variable RailRevenues "Railroad Revenues"
label variable Participation "Election Participation"
label variable Socialist "Socialist Vote Share" 
label variable Zentrum "Zentrum Vote Share"
label variable DifCatholicsZentrum "Zentrum vs Catholic"
label variable City "City Status"
label variable PopShare1880 "County Population Share"
label variable Fringe "Fringe Region"



esttab Decay Band Region using U:\Telephone\Stata\Results\SpatialDurationResults.tex, replace label title(Diffusion of Local Exchanges with Spatial Correlation)  nonumbers mtitles( "Decay" "Band" "Region") 
