******************************************************************************
* Author: Jacopo Lunghi
* Last modified: 5 June 2024
* Paper: Street green space and electricity demand: Evidence from metered consumption data
* Replication code of Figure 9 and Table 5
*******************************************************************************

/* Necessary input files:
- "$user\month_2g_2020_2023_withprices.dta": monthly consumption data with price paid per kWh (Utility Provider)
- "$user\scenarios_outputs_comunelevel.csv": GVI scenarios (see Table 4)
- "$user\DCSS_FAM_POP_12032025153830449.csv": Households in Italy by municipality (ISTAT)
*/




clear all 
version 19

global path1 "C:\Users\jacop\Dropbox\research\hera\E2G"
global user "$path1\Jacopo\pathtoyourfolder"

*****************************************************
* Derive municipality-level median consumption
use "$user\month_2g_2020_2023_withprices.dta", clear
local vars exp_per_kw_bill p_per_kwh 
foreach m of local vars {
	bys comune: egen median_`m'=median(`m')	
}


preserve
duplicates drop comune, force
keep comune consumption_bill exp_per_kw_bill median_exp_per_kw_bill median_p_per_kwh
tempfile t
save `t'
restore

*****************************************************
* Households per municipality
preserve
import delimited "$user\DCSS_FAM_POP_12032025153830449.csv", clear
keep if tipo_dato=="NPHH_AV"
drop if length(itter)<5
drop if strpos(itter,"IT")
duplicates drop territorio, force
rename territorio comune

replace comune="Duino Aurisina" if comune=="Duino Aurisina-Devin Nabrežina"
replace comune="Popoli" if comune=="Popoli Terme"
replace comune="Sgonico" if comune=="Sgonico-Zgonik"
replace comune="San Dorligo della Valle" if comune=="San Dorligo della Valle-Dolina"
keep itter107 comune value
tempfile istat_hh
save `istat_hh'
restore

*
preserve
import delimited "$user\DCSS_FAM_POP_12032025153830449.csv", clear
keep if tipo_dato=="NPHH_AV"
keep if territorio=="Italia"
su value
global pop_hh=`r(mean)'
restore

****************************************************
* Merge with scenarios
import delimited "$user\scenarios_outputs_comunelevel.csv", clear

forvalues j=1(1)11 {
	replace  avg_delta_frac`j'="" if  avg_delta_frac`j'=="NA"
	replace avg_delta_abs`j'="" if avg_delta_abs`j'=="NA"
	destring avg_delta_frac`j', replace
	destring avg_delta_abs`j', replace
	replace avg_delta_frac`j' = -avg_delta_frac`j'
	replace avg_delta_abs`j' = -avg_delta_abs`j'
}

* Correction of names of comuni
replace comune="Arsiè" if comune=="ArsiÃ¨"
replace comune="Canicattì" if comune=="CanicattÃ¬"
replace comune="Cantù" if comune=="CantÃ¹"
replace comune="Castel San Niccolò" if comune=="Castel San NiccolÃ²"
replace comune="Castelfranco Piandiscò" if comune=="Castelfranco PiandiscÃ²"
replace comune="Cavaglià" if comune=="CavagliÃ "
replace comune="Città Sant'Angelo" if comune=="CittÃ  Sant'Angelo"
replace comune="Città della Pieve" if comune=="CittÃ  della Pieve"
replace comune="Città di Castello" if comune=="CittÃ  di Castello"
replace comune="Fenegrò" if comune=="FenegrÃ²"
replace comune="Forlì" if comune=="ForlÃ¬"
replace comune="Maserà di Padova" if comune=="MaserÃ  di Padova"
replace comune="Merì" if comune=="MerÃ¬"
replace comune="Montù Beccaria" if comune=="MontÃ¹ Beccaria"
replace comune="Muggiò" if comune=="MuggiÃ²"
replace comune="Nardò" if comune=="NardÃ²"
replace comune="Palù" if comune=="PalÃ¹"
replace comune="Platì" if comune=="PlatÃ¬"
replace comune="Ponte San Nicolò" if comune=="Ponte San NicolÃ²"
replace comune="Riccò del Golfo di Spezia" if comune=="RiccÃ² del Golfo di Spezia"
replace comune="Roncà" if comune=="RoncÃ "
replace comune="Roveredo di Guà" if comune=="Roveredo di GuÃ "
replace comune="Roverè Veronese" if comune=="RoverÃ¨ Veronese"
replace comune="San Donà di Piave" if comune=="San DonÃ  di Piave"
replace comune="Santa Maria Hoè" if comune=="Santa Maria HoÃ¨"
replace comune="Temù" if comune=="TemÃ¹"
replace comune="Travacò Siccomario" if comune=="TravacÃ² Siccomario"
replace comune="Viggiù" if comune=="ViggiÃ¹"


merge m:1 comune using `t'
drop if _merge==2
drop _merge


local vars comune month region year avg_cons_month avg_delta_frac avg_delta_abs scenario


local vars exp_per_kw_bill p_per_kwh 
forvalues j=1(1)11 {
	foreach m of local vars {
		gen delta`j'_median_`m'=avg_delta_abs`j'*median_`m'
	}
	
	bys month: egen y`j'_exp_per_kw_bill=mean(delta`j'_median_exp_per_kw_bill)
	bys month: egen y`j'_p_per_kwh=mean(delta`j'_median_p_per_kwh)
}

*
gen monthly_expenditure=consumption_bill* exp_per_kw_bill
tabstat monthly_expenditure median_p_per_kwh median_exp_per_kw_bill, save

merge m:1 comune using `istat_hh'
drop if _merge==2
drop _merge

***** Set up the table
preserve
duplicates drop comune, force
gen sample_hh=sum(value)
su sample_hh
global sample_hh=`r(mean)'
putexcel H3=mat(`r(mean)')
restore 

preserve
keep comune avg_cons_719_month avg_delta_abs* delta*_median_p_per_kwh y*_exp_per_kw_bill month y*_p_per_kwh
duplicates drop comune month, force

forvalues j=1(1)11 {
	bys comune: egen sdelta`j'_median=sum(avg_delta_abs`j')
	bys comune: egen sy`j'_exp_per_kw_bill=sum(y`j'_exp_per_kw_bill)
	bys comune: egen sy`j'_p_per_kwh=sum(y`j'_p_per_kwh)
}

duplicates drop comune, force

forvalues j=1(1)11 {
	rename sdelta`j'_median delta`j'
	rename sy`j'_exp_per_kw_bill exp`j'
	rename sy`j'_p_per_kwh p`j'
}

gen ivar=_n

reshape long delta exp p, i(ivar) j(scenario)

* Uniform the values with Table 4
gen new_scenario = .
replace new_scenario = scenario - 5 if inrange(scenario, 6, 10)
replace new_scenario = scenario + 5 if inrange(scenario, 1, 5)

tabstat delta p exp, nototal by(scenario) save

tabstatmat A
matrix B = A[., 2..3]
matrix C = B * $sample_hh * (1/1000)
matrix D = B * $pop_hh * (1/1000)
restore




****************************************************
* Fig. 9
preserve

gen new_scenario = .

* Uniform the values with Table 4
replace new_scenario = scenario - 5 if inrange(scenario, 6, 10)
replace new_scenario = scenario + 5 if inrange(scenario, 1, 5)

duplicates drop month, force
keep month y1_exp_per_kw_bill-y11_exp_per_kw_bill y1_p_per_kwh-y11_p_per_kwh
drop delta*
forvalues j=1(1)11 {
	rename y`j'_exp_per_kw_bill y_exp_per_kw_bill`j'
	rename y`j'_p_per_kwh y_p_per_kwh`j'
}
reshape long y_exp_per_kw_bill y_p_per_kwh, i(month) j(scenario)
reshape wide y_exp_per_kw_bill y_p_per_kwh, i(scenario) j(month)


graph bar y_p_per_kwh6 y_p_per_kwh7 y_p_per_kwh8, ///
    over(scenario, gap(50) label(angle(30) labsize(vsmall) ticks ) ///
	relabel(1 "GVI {&ge} 15, hist" 2 "GVI {&ge} 20, hist" 3 "GVI {&ge} 21, hist" 4 "GVI {&ge} 24, hist" 5 "GVI {&ge} 27, hist" ///
    6 "GVI*, SSP585" 7 "GVI {&ge} 15, SSP585" 8 "GVI {&ge} 15, SSP585" ///
    9 "GVI {&ge} 21, SSP585" 10 "GVI {&ge} 24, SSP585" 11 "GVI {&ge} 27, SSP585") ) ///
    stack ///
    bar(1, color(yellow) lcolor(black) lwidth(vthin)) ///
    bar(2, color(orange) lcolor(black) lwidth(vthin)) ///
    bar(3, color(red) lcolor(black) lwidth(vthin)) ///
    legend(order(1 "June" 2 "July" 3 "August") pos(6) size(small) cols(3)) ///
    ytitle("Monthly expenditure (euros, energy component)", size(small)) ylabel(, labsize(small)) //


graph bar y_exp_per_kw_bill6 y_exp_per_kw_bill7 y_exp_per_kw_bill8, ///
    over(scenario, gap(50) label(angle(30) labsize(vsmall) ticks ) ///
	relabel(1 "GVI {&ge} 15, hist" 2 "GVI {&ge} 20, hist" 3 "GVI {&ge} 21, hist" 4 "GVI {&ge} 24, hist" 5 "GVI {&ge} 27, hist" ///
    6 "GVI*, SSP585" 7 "GVI {&ge} 15, SSP585" 8 "GVI {&ge} 15, SSP585" ///
    9 "GVI {&ge} 21, SSP585" 10 "GVI {&ge} 24, SSP585" 11 "GVI {&ge} 27, SSP585") ) ///
    stack ///
    bar(1, color(yellow) lcolor(black) lwidth(vthin)) ///
    bar(2, color(orange) lcolor(black) lwidth(vthin)) ///
    bar(3, color(red) lcolor(black) lwidth(vthin)) ///
    legend(order(1 "June" 2 "July" 3 "August") pos(6) size(small) cols(3)) ///
    ytitle("Monthly expenditure (euros, overall component)", size(small)) ylabel(, labsize(small)) //

	
****
* Export output at will
****

restore

