sysdir set PLUS "C:\Temp\Stata\ado\plus\"
sysdir set PERSONAL "C:\Temp\Stata\ado\personal\"
sysdir set OLDPLACE "C:\Temp\Stata\ado\plus\"
//Standard Duration analysis

clear
//insheet using "C:\Onedrive\Research\Telephone\Code\Stata\Data\TownsHazardCons.csv"
insheet using \\Client\C$\Onedrive\Research\Telephone\Code\Stata\Data\TownsHazardCons.csv

drop if region == "PF"

stset installmonth


quietly streg marketaccess1880 marketsize1880 y1880 city popshare1880 fringe border agriculture empratio82 indexdissim82  statetax localtax railstation railrevenues railweight postrevenues participation zentrum difcatholicszentrum liberal socialist ,  vce(robust) distribution(weibull) time    
estimates store WBCons
//estimates save "C:\Onedrive\Research\Telephone\Code\Stata\Results\WBCons.csv", replace
estimates save \\Client\C$\Onedrive\Research\Telephone\Code\Stata\Results\WBCons.csv, replace



quietly streg marketaccess1880 marketsize1880 y1880 city popshare1880 fringe border agriculture empratio82 indexdissim82  statetax localtax railstation railrevenues railweight postrevenues participation zentrum difcatholicszentrum liberal socialist ,  vce(robust) strata(region)  distribution(weibull) time    
estimates store WBConsStrata
//estimates save "C:\Onedrive\Research\Telephone\Code\Stata\Results\WBConsStrata.csv", replace
estimates save \\Client\C$\Onedrive\Research\Telephone\Code\Stata\Results\WBConsStrata.csv, replace


//Duration Analysis with Time-varying variables

clear
//insheet using "C:\Research\Telephone\Code\Stata\Data\TownsHazardVary.csv"
insheet using \\Client\C$\Onedrive\Research\Telephone\Code\Stata\Data\TownsHazardVary.csv

drop if region == "PF"

stset time, id(town) failure(failure)
stvary population marketaccess marketsize


quietly streg marketaccess marketsize population marketaccess1880 marketsize1880 city popshare1880 fringe border agriculture empratio82 indexdissim82  statetax localtax railstation railrevenues railweight postrevenues participation zentrum difcatholicszentrum liberal socialist , vce(robust) strata(region) distribution(weibull) time
estimates store WBTVC
//estimates save "C:\Onedrive\Research\Telephone\Code\Stata\Results\WBTVC.csv" , replace
estimates save \\Client\C$\Onedrive\Research\Telephone\Code\Stata\Results\WBTVC.csv, replace


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
label variable marketaccess "Market Access Time-varying"
label variable marketsize "Market Size Time-varying"
label variable population "Population Time-varying" 

esttab WBCons WBConsStrata WBTVC using example2.tex, replace label title(Diffusion of local exchanges)  nonumbers mtitles( "Baseline" "Stratified" "Time-varying")  indicate(Regional effects = _Sreg*)


