clear all
set more off

cd "C:/Users\johnj\Dropbox\Julian\Academia\UIUC\Semester 11 Fall 2022\ECON474\Final Project\Reimers 2019\data"


use amazon_data.dta, clear

***************************************************
** Summary statistics - Amazon info (Table 1)

gen hc = format=="Hardcover"
gen pb = format=="Paperback"
gen other = 1 - hc - pb 

sum hc pb other age new used major prize price sales gb demand if t==0 & gb==0
sum hc pb other age new used major prize price sales gb demand if t==0 & gb==0 & post==0
sum hc pb other age new used major prize price sales gb demand if t==0 & gb==0 & post==1

foreach i in hc pb other age new used picture major prize price sales gb demand ntitle ntitleformat {
	di "t-test for `i':"
	ttest `i' if gb==0 & t==0, by(post) 
}
foreach i in Hardcover Paperback E-book {
	ttest ntitleformat if format=="`i'" & t==1, by(post)
}
sum gb if t==0 & gb>0

drop if t==0
***************************************************



***************************************************
** RDD: Price (Table 3)

replace year=year-1923
gen year2 = year^2
gen year3 = year^3
gen logprice = ln(amazonprice)
replace amazonprice=0 if format=="Gutenberg"
encode format, generate(formats)

eststo clear

eststo: reg logprice post plr year* prize_author canon* new age major i.formats i.t if year>=-8 & year<=7, vce(cluster year)
eststo: reg logprice post year* plr prize_author canon* new age major i.t if year>=-8 & year<=7 & format=="Hardcover", vce(cluster year)
eststo: reg logprice post year* plr prize_author canon* new age major i.t if year>=-8 & year<=7 & format=="Paperback", vce(cluster year)
eststo: reg logprice post plr year* prize_author canon* ntitleformat new age major i.formats i.t if year>=-8 & year<=7, vce(cluster year)
eststo: reg logprice post year* plr prize_author canon* ntitleformat new age major i.t if year>=-8 & year<=7 & format=="Hardcover", vce(cluster year)
eststo: reg logprice post year* plr prize_author canon* ntitleformat new age major i.t if year>=-8 & year<=7 & format=="Paperback", vce(cluster year)

esttab, se star(* 0.10 ** 0.05 *** 0.01) r2 ar2 drop(*.t)
*esttab using "results\rdd_price.tex", se star(* 0.10 ** 0.05 *** 0.01) r2 ar2 drop(*.t) replace
************************************************************************



 
************************************************************************
** Setting up for the RDD on editions

** Gutenberg editions are mechanical, so drop these:
drop if format=="Gutenberg"


** Aggregate dataset to the title-format level:
collapse (sum) sales* (mean) canon* prize_author amazonprice price picture major age plr ntitle* post year*, by(title format)

// save "C:/Users\johnj\Dropbox\Julian\Academia\UIUC\Semester 11 Fall 2022\ECON474\Final Project\Reimers 2019\amazon_data.dta"

** Expand to include 0s for unavailable title-editions 
gen hc = format=="Hardcover"
gen pb = format=="Paperback"
gen eb = format=="E-book"

egen hc_title=sum(hc), by(title)
egen pb_title=sum(pb), by(title)
egen eb_title=sum(eb), by(title)

gen all_title = hc_title + pb_title + eb_title
gen b = 4 - all_title
expand b, generate(newvar)



gsort title newvar format
drop if newvar==1 & all_title==2 & title[_n+1]!=title

** for those that just miss 1:
replace format = "Hardcover" if newvar==1 & hc_title==0
replace format = "Paperback" if newvar==1 & hc_title==1 & pb_title==0
replace format = "E-book" if newvar==1 & hc_title==1 & pb_title==1 & eb_title==0

** for those that miss 2:
sort title format newvar
replace format = "E-book" if format==format[_n-1] & newvar==1 & title==title[_n-1] & eb_title==0
sort title format newvar
drop if format==""
replace format = "Paperback" if format==format[_n-1] & newvar==1 & title==title[_n-1] & pb_title==0

replace ntitleformat=0 if newvar==1
replace ntitle=0 if ntitle==.
************************************************************************


// save "C:/Users\johnj\Dropbox\Julian\Academia\UIUC\Semester 11 Fall 2022\ECON474\Final Project\Reimers 2019\amazon_data.dta", replace


************************************************************************
* RDD - editions (Table 2)

eststo clear

** title level:
eststo: reg ntitle post plr prize_author canon* year* if year>=-8 & year<=7 & format=="Hardcover", vce(cluster year) 

** title-format level:
foreach i in Hardcover Paperback E-book {
	di "`i'"
	eststo: reg ntitleformat post plr prize_author canon* year* if year>=-8 & year<=7 & format=="`i'", vce(cluster year)
}

esttab, se star(* 0.10 ** 0.05 *** 0.01) r2 ar2
*esttab using "\results\rdd_ntitle.tex", se star(* 0.10 ** 0.05 *** 0.01) r2 ar2 replace
************************************************************************


************************************************************************
** For other statistics
bys post: sum ntitle* if format=="Hardcover"
count if ntitle == 0 & post==0 & format=="Hardcover"
count if ntitle == 0 & post==1 & format=="Hardcover"
************************************************************************



************************************************************************
** Alternative specifications: logs


gen logntitle = ln(ntitle)
gen logntitleformat = ln(ntitleformat)

eststo clear
eststo: reg logntitle post plr prize_author canon* year* if format=="Hardcover", vce(cluster year) // this is the same as if collapsed by title
foreach i in Hardcover Paperback E-book {
	di "`i'"
	eststo: reg logntitleformat post plr prize_author canon* year* if format=="`i'", vce(cluster year)
}

esttab, se star(* 0.10 ** 0.05 *** 0.01) r2 ar2
************************************************************************




************************************************************************
** Graph of average number of editions per publication year (Figure 1)

gen price_hc = amazonprice if format=="Hardcover"
gen price_pb = amazonprice if format=="Paperback"
gen price_eb = amazonprice if format=="E-book"

bys title: gen n1=_n

tab prize post if n1==1
tab canon_author post if n1==1
replace ntitle=. if n1!=1

collapse (mean) *sales ntitle* plr post picture major age prize canon*, by(year)
replace year = year+1923


graph twoway (scatter ntitle year, mcolor(blue) msymbol(X)) (mspline ntitle year if year<1923) (mspline ntitle year if year>=1923), xline(1922.5) xtitle("Year of original publication") ytitle("Avg. in-print editions per title") legend(off) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(1910 1915 1920 1925 1930 1935) scheme(s2mono)
*graph export "paper/ntitle_rdd.pdf", as(pdf) replace
************************************************************************



************************************************************************
** Evidence for identification (Figure 2)

** 1) PLR checkouts: 
graph twoway (scatter plr year, mcolor(blue) msymbol(X)) if year<1936, xline(1922.5) scheme(s2mono) xtitle("Year of original publication") ytitle("Avg. annual British library checkouts per title") legend(off) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(1910 1915 1920 1925 1930 1935) 
*graph export "paper/plr_rdd.pdf", as(pdf) replace


******************************
** 2) Editions within 74 years of original publication:
** Note: this requires characteristics of each individual edition:

use "edition_details.dta", clear

** look only at editions within 74 years of original publication:
gen age = pubyear - year
keep if age<75

gen hc = format=="Hardcover"
gen pb = format=="Paperback"
bys title: gen n = _n

** a) number of hc editions:
egen heditions75 = sum(hc), by(title)
gen temp = heditions75 if n==1
egen heditions75_py = mean(temp), by(year)
twoway (scatter heditions75_py year, mcolor(blue) msymbol(X)), xline(1922.5) ytitle(Avg. hardcover editions per title) xtitle(Year of original publication) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(1910 1915 1920 1925 1930 1935) scheme(s2mono)
*graph export "paper/hc_ntitle75.pdf", as(pdf) replace
drop temp


** b) prices of editions
egen price75_py = mean(bowkerprice), by(year)
twoway (scatter price75_py year, mcolor(blue) msymbol(X)), xline(1922.5) ytitle(Avg. price per edition) xtitle(Year of original publication) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(1910 1915 1920 1925 1930 1935) scheme(s2mono)
*graph export "paper/edition_prices75.pdf", as(pdf) replace

** c) number of pages
egen pagelength75_py = mean(pages), by(year)
twoway (scatter pagelength75_py year, mcolor(blue) msymbol(X)), xline(1922.5) ytitle(Avg. pages per edition) xtitle(Year of original publication) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(1910 1915 1920 1925 1930 1935) scheme(s2mono)
*graph export "paper/pages75.pdf", as(pdf) replace
drop temp*
************************************************************************


