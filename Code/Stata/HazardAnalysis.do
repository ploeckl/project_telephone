
//Standard Duration analysis

clear

insheet using "U:\Telephone\Stata\Data\TownsHazardCons.csv"


drop if region == "PF"
encode region, generate(region_factor)


stset installmonth


//quietly
streg ma_post_out_1880 border y1880 postrevenues_pc telegraphrevenues_pc agriculture empratio82 indexdissim82 statetax railstation railrevenues participation socialist zentrum difcatholicszentrum city popshare1880 fringe,  vce(robust) distribution(weibull) time    
estimates store WBCons




//estimates save "C:\Onedrive\Research\Telephone\Code\Stata\Results\WBCons.csv", replace
//estimates save \\Client\C$\Onedrive\Research\Telephone\Code\Stata\Results\WBCons.csv, replace






//quietly 
streg  ma_post_out_1880 border y1880 postrevenues_pc telegraphrevenues_pc agriculture empratio82 indexdissim82 statetax railstation railrevenues participation socialist zentrum difcatholicszentrum city popshare1880 fringe,  vce(robust) strata(region_factor)  distribution(weibull) time    
estimates store WBConsStrata
//estimates save "C:\Onedrive\Research\Telephone\Code\Stata\Results\WBConsStrata.csv", replace
//estimates save \\Client\C$\Onedrive\Research\Telephone\Code\Stata\Results\WBConsStrata.csv, replace


//Duration Analysis with Time-varying variables

clear
//insheet using "C:\Research\Telephone\Code\Stata\Data\TownsHazardVary.csv"
//insheet using \\Client\C$\Onedrive\Research\Telephone\Code\Stata\Data\TownsHazardVary.csv
insheet using "U:\Telephone\Stata\Data\TownsHazardVary.csv"

drop if region == "PF"
encode region, generate(region_factor)


stset time, id(town) failure(failure)
stvary population marketaccess_tel marketaccess_pop


//quietly 
streg marketaccess_tel border population postrevenues_pc telegraphrevenues_pc agriculture empratio82 indexdissim82 statetax railstation railrevenues participation socialist zentrum difcatholicszentrum  city popshare1880 fringe, vce(robust) strata(region_factor) distribution(weibull) time
estimates store WBTVC
//estimates save "C:\Onedrive\Research\Telephone\Code\Stata\Results\WBTVC.csv" , replace
//estimates save \\Client\C$\Onedrive\Research\Telephone\Code\Stata\Results\WBTVC.csv, replace


label variable ma_pop_out_1880 "Market Access Population 1880"
label variable ma_post_out_1880 "Market Access Postal 1880"
label variable marketaccess_tel "Market Access Connected"
label variable population "Population varying"
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
label variable postrevenues_pc "Post Revenues"
label variable telegraphrevenues_pc "Telegraph Revenues"
label variable participation "Election Participation"
label variable zentrum "Zentrum Vote Share"
label variable difcatholicszentrum "Zentrum vs Catholic"
label variable liberal "Liberal Vote Share"
label variable socialist "Socialist Vote Share" 

esttab WBCons WBConsStrata WBTVC using U:\Telephone\Stata\Results\BaseHazard.tex, replace label title(Diffusion of local exchanges)  nonumbers mtitles( "Baseline" "Stratified" "Time-varying")  //indicate(Regional effects = _Sreg*)


