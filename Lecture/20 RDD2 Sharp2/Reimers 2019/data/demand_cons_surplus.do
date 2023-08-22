clear all
set more off

** set directory:
cd "~\Dropbox\research_projects\copyright\"

use amazon_data.dta, clear

***************************************************
** Data cleaning: drop outliers, encode formats, create the DV

drop if price> 50
drop if amazonprice> 50 & amazonprice!=.
replace amazonprice=0 if format=="Gutenberg"
drop if t==0 /* first month of collection - with some issues */

encode format, generate(formats)
encode title, generate(tno)

** take copyright status from Project Gutenberg availability
gen temp = format=="Gutenberg"
egen temp1 = max(temp), by(title)
replace post1923 = 0 if temp1==1
drop temp*


***********************************************************************
** Demand estimation ** ** Demand estimation ** ** Demand estimation ** 
***********************************************************************

***************************************************
** create dependent variable

** Market Definition:
gen marketsize = 300000000  /* Each American decides whether to buy a book each month */
replace demand = demand+0.0001 /* many 0's. See below for robustness */
gen double share = demand/marketsize
egen double sumdemand = sum (demand) , by(t)
gen double outside_share = (marketsize - sumdemand)/marketsize

** Dependent variable:
gen double logshare=ln(share)
gen double lnoutside=ln(outside_share)
gen double diffshare = logshare-lnoutside
***************************************************



***************************************************
** Create instruments for prices and inside shares 

** Number of sellers per edition:
** set to min(100,max(sellers)) for e-books:
replace newnumb=0 if format=="Gutenberg"|format=="E-book"
replace usednumb=0 if format=="Gutenberg"|format=="E-book"
egen yyy = max(newnumb), by(t)
replace newnumb = yyy if format=="Gutenberg"
replace newnumb = 100 if format=="Gutenberg" & newnumb<100

** Number of editions per nest:
egen instrument_titleformat = count(plr), by(t title format)
egen instrument_title = count(plr), by(t title)
***************************************************



***************************************************
** Demand estimations: logit and nested logit (Table 4)

*****************************
** No nests: regular logit *

eststo clear
eststo c1: reg diffshare price new major age picture b2.formats i.t i.tno, vce(cluster tno)

** Elasticity:
gen alpha = _b[price]
gen double elas = alpha*(1-share)*price


**********************
** With instruments

eststo c2: ivregress 2sls diffshare new major age picture b2.formats i.t i.tno (price = newnumb), vce(cluster tno)
estat firststage

*Elasticity:
replace alpha = _b[price]
gen double elas_iv = alpha*(1-share)*price


********************************
** One level of nests: titles

egen titledemand = sum(demand) , by(title t)
gen double share_inside = ln(demand/titledemand)
label var share_inside "$\sigma$(title)"

eststo c3: ivregress 2sls diffshare new major age picture b2.formats i.tno i.t (price share_inside = newnumb instrument_titleformat), vce(cluster tno) first


*Elasticity:
gen sigma= _b[share_inside]
replace alpha = _b[price]
gen double elas_nl = alpha/(1-sigma)*price*(1-sigma*(demand/titledemand)-(1-sigma)*share)



*********************************
** Two nests: titles & formats

egen titleformatdemand = sum(demand) , by(title format t)

gen share_ins = ln(demand/titleformatdemand)
gen share_ins_ins = ln(titleformatdemand/titledemand)

label var share_ins "$\sigma$(title -- format)"
label var share_ins_ins "$\sigma$(title)"

eststo c4: ivregress 2sls diffshare new major age picture b2.formats i.tno i.t (price share_ins share_ins_ins = newnumb instrument_titleformat instrument_title), vce(cluster tno)

*esttab using results/nlogit_demand.tex, drop(*tno *.t) se r2 ar2 star(* 0.10 ** 0.05 *** 0.01) replace
esttab c1 c2 c3 c4, drop(*tno *.t) ar2 se star(* 0.10 ** 0.05 *** 0.01)


*Elasticity:
gen sigma1 = _b[share_ins_ins]
gen sigma2 = _b[share_ins]
replace alpha = _b[price]

gen double elas_2nl = alpha/(1-sigma1)*price*(1-(sigma1-sigma2)/(1-sigma2)*(demand/titledemand)-sigma2*(1-sigma1)/(1-sigma2)*(demand/sumdemand)-(1-sigma1)*share)
drop sigma* alpha 

sum elas* if price!=0
***************************************************




***********************************************************************
** Robustness checks ** ** Robustness checks ** ** Robustness checks ** 
***********************************************************************

***************************************************
** Testing the first stage: relevance & validity (Table A1)

eststo clear
** Relevance of instruments:
eststo: reg price new major age picture b2.formats i.tno i.t newnumb instrument_titleformat, vce(robust)
test newnumb instrument_titlefor

eststo: reg share_inside new major age picture b2.formats i.tno i.t newnumb instrument_titleformat, vce(robust) 
test newnumb instrument_titleformat


esttab, noomitted mtitles("DV: Price" "DV: inside share") keep(newnumb instrum*) b(%5.3f) se alignment(rrrrrr) compress replace label nonotes star(* 0.10 ** 0.05 *** 0.01) scalars("r2_a $\overline{R^2}$") nocons

corr diffshare share_inside price newnumb instrument_titleformat if format!="Gutenberg"
***************************************************



***************************************************
** Testing assumptions: number of sellers for electronic formats (footnote 30)

gen newnumb1 = newnumb
replace newnumb1 = 20 if format=="E-book"|format=="Gutenberg"
replace newnumb1 = yyy if (format=="Gutenberg") & yyy>newnumb1
eststo b1: ivregress 2sls diffshare new major age picture b2.formats i.tno i.t (price share_inside = newnumb instrument_titleformat), vce(cluster tno) 

replace newnumb1 = 50 if format=="E-book"|format=="Gutenberg"
replace newnumb1 = yyy if (format=="Gutenberg") & yyy>newnumb1
eststo b2: ivregress 2sls diffshare new major age picture b2.formats i.tno i.t (price share_inside = newnumb1 instrument_titleformat), vce(cluster tno) 

replace newnumb1 = 100 if format=="E-book"|format=="Gutenberg"
replace newnumb1 = yyy if (format=="Gutenberg") & yyy>newnumb1
eststo b3: ivregress 2sls diffshare new major age picture b2.formats i.tno i.t (price share_inside = newnumb1 instrument_titleformat), vce(cluster tno) 

replace newnumb1 = 150 if format=="E-book"|format=="Gutenberg"
replace newnumb1 = yyy if (format=="Gutenberg") & yyy>newnumb1
eststo b4: ivregress 2sls diffshare new major age picture b2.formats i.tno i.t (price share_inside = newnumb1 instrument_titleformat), vce(cluster tno) 

replace newnumb1 = 200 if format=="E-book"|format=="Gutenberg"
replace newnumb1 = yyy if (format=="Gutenberg") & yyy>newnumb1
eststo b5: ivregress 2sls diffshare new major age picture b2.formats i.tno i.t (price share_inside = newnumb1 instrument_titleformat), vce(cluster tno) 

esttab b1 b2 b3 b4 b5,  noomitted drop(*.tno *.t _cons ) b(%5.3f) se alignment(rrrrrr) compress replace label nonotes star(* 0.10 ** 0.05 *** 0.01) scalars("r2_a $\overline{R^2}$") nocons
***************************************************


***************************************************
** Testing assumption: adding small constant to unit sales (Table A2)

gen dem2 = round(demand)
eststo clear
local j=1

foreach i in 0.00001 0.0001 0.001 0.01 0.1 1 {
gen double newdemand = dem2 + `i'
gen double newshare = newdemand/marketsize
egen double newsumdemand = sum(newdemand) , by(t)
gen double newoutside_share = (marketsize - newsumdemand)/marketsize

gen double newlogshare=ln(newshare)
gen double newlnoutside=ln(newoutside_share)

gen double newdiffshare = newlogshare-newlnoutside


********************************
** Demand estimation:

gen double newshare_inside = ln(newdemand/newsumdemand)
egen newtitledemand = sum(newdemand) , by(title t)
replace newshare_inside = ln(newdemand/newtitledemand)

eststo: ivregress 2sls newdiffshare new major age picture b2.formats i.tno i.t (price newshare_inside = newnumb instrument_titleformat), vce(cluster tno)

drop newdemand newshare newsumdemand newoutside_share newlogshare newlnoutside newdiffshare newshare_inside newtitledemand 

gen elas`j' = _b[price]/(1-_b[newshare_inside])*price*(1-_b[newshare_inside]*(demand/titledemand)-(1-_b[newshare_inside])*share)

local j=`j'+1
}

esttab, noomitted mtitles("Add 0.000001" "Add 0.00001" "Add 0.0001" "Add 0.001" "Add 0.01" "Add 0.1" "Add 1") drop(*.tno *.t _cons ) b(%5.3f) se alignment(rrrrrr) compress replace label nonotes nostar scalars("r2_a $\overline{R^2}$") nocons
***************************************************



****************************************************************
** Title qualities ** ** Title qualities ** ** Title qualities** 
****************************************************************


***************************************************
** Add estimated qualities to dataset

ivregress 2sls diffshare new major age picture b2.formats i.tno i.t (price share_inside = newnumb instrument_titleformat), vce(cluster tno)

tempfile tempo1
preserve
	parmest, fast
	gen tno = real(substr(parm,1,index(parm,".tno")-1))
	keep if tno!=.
	keep tno estimate
	rename estimate quality
	save `tempo1'
restore


merge m:1 tno using `tempo1'
gsort -quality title
br title post year quality ntitle if title!=title[_n-1]
***************************************************


***************************************************
** Show editions by (normalized) quality (Figure 4)

gen qq = quality 
egen a = min(quality)
egen b = max(quality)
replace quality = 10*(quality-a)/(b-a)
twoway (scatter ntitle quality if post==0, mcolor(blue) msymbol(o))(scatter ntitle quality if post==1, mcolor(red) msymbol(X)), ytitle(Number of editions) xtitle(Creative quality) legend(order(1 "Public Domain" 2 "Copyright Protection")) scheme(s2mono) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(0 2 4 6 8 10)
*graph export "paper/ntitle_quality.pdf", as(pdf) replace
drop a b
***************************************************


***************************************************
** Show average estimated quality by publication year (Figure 3) 

preserve
	gsort tno quality
	bys tno: gen n1 = _n
	ttest quality if n1==1, by(post1923)

	keep if n1==1
	collapse (mean) quality, by(year)
	twoway (scatter quality year, mcolor(blue) msymbol(X)), xline(1922.5) ytitle(Mean creative quality) xtitle(Year of publication) scheme(s2mono) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(1910 1915 1920 1925 1930 1935)
	*graph export "paper/quality_by_year.pdf", as(pdf) replace
restore

***************************************************



********************************************************************
** Consumer surplus ** ** Consumer surplus ** ** Consumer surplus ** 
********************************************************************

gsort tno t -demand
bys tno: gen nnn = _n

*************************************************
** Amazon and GB only account for 40% of the market:

gen double scales = demand*2.5 
*replace scales = demand*4 if format=="Gutenberg"

gen double scaled_share = scales/marketsize
egen double scaled_sumdemand = sum (scales) , by(t)
gen double scaled_outside_share = (marketsize - scaled_sumdemand)/marketsize


gen double scaled_logshare=ln(scaled_share)
gen double scaled_lnoutside=ln(scaled_outside_share)

gen double scaled_diffshare = scaled_logshare-scaled_lnoutside
*************************************************


** save file to use in profit regressions:
save tempfile.dta, replace
use tempfile.dta, clear


*************************************************
** Re-run regression to obtain demand parameters

ivregress 2sls scaled_diffshare new major age picture b2.formats i.tno i.t (price share_inside = newnumb instrument_titleformat), vce(cluster tno)

gen double sigma = _b[share_inside]
gen double alpha = _b[price]
gen double ndelta = scaled_diffshare - sigma*share_inside
*************************************************



*************************************************
** I don't observe all editions -> scale up:

egen double observed = sum(new), by(tno format t)
egen double ttt = count(nn), by(tno format t)
replace observed = ttt if observed==0
drop ttt
gen double temp = exp(ndelta/(1-sigma))

collapse (mean) ntitle* ndelta temp observed sigma alpha post quality qq marketsize scales, by(tno format t)

egen double temp1 = sum(temp), by(tno format t)
gen double temp2 = (temp1*ntitleformat/observed) //^(1-sigma)
*************************************************


*************************************************
** To calculate consumer surplus per month:

egen double nsumexp = sum(temp2), by(tno t) 
bys tno t: gen nn = _n
replace nsumexp = nsumexp^(1-sigma)
replace nsumexp = 0 if nn>1

egen double sumDw = sum(nsumexp), by(t)
egen double sumDw_post = sum(nsumexp), by(t post)
egen double sumDw_title = sum(nsumexp), by(t tno)

gen double CS = -1/alpha *ln(1+sumDw) * marketsize
gen double CS_post = -1/alpha *ln(1+sumDw_post) * marketsize
gen double CS_title = -1/alpha * ln(1+sumDw_title)* marketsize

bys post: sum CS* if nn==1
*************************************************


*************************************************
** add up over 1 year

keep tno post1923 quality qq ntitle t CS*
duplicates drop tno post ntitle t, force

gsort t post -quality
replace CS = 0 if t==t[_n-1]
replace CS_post = 0 if t==t[_n-1] & post==post[_n-1]


egen double CSyear = sum(CS)
egen double CSyear_post = sum(CS_post), by(post)
egen double CSyear_title = sum(CS_title), by(tno)

keep tno post quality qq ntitle *year*
gsort tno post

duplicates drop tno, force
*************************************************



*************************************************
** summarize consumer surplus by IP status and title (Figure 5)

bys post: sum CS*

gsort post -quality 
bys post: gen prank = _n

reshape wide tno *CS* quality qq ntitle, i(prank) j(post)

gen out_of_print = ntitle1==.
gen double difftitle = CSyear_title0 - CSyear_title1

bys out_of_print: sum difftitle CSyear_title*
sum difftitle CSyear_title*


twoway (scatter difftitle prank, mcolor(blue) msymbol(X)) if difftitle<70000, xtitle(IP-specific ranking in creative quality) ytitle(Difference in cons. surplus) scheme(s2mono) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(1 25 50 75 100 125)
*graph export "paper/cs_diff_prank.pdf", replace
***********************************************




***********************************************************
** Aggregate effects on consumer surplus to the industry **
***********************************************************


***********************************************
** connect data to ALL gutenberg download counts

decode tno0, generate(title)
merge 1:1 title using "gutenberg_downloads.dta"
***********************************************


***********************************************
** fill in likely CS impacts based on download counts

gsort downloads have title

** for titles that would be in print anyway
gen double diff_inprint = difftitle if ntitle1!=.
gen diffin_obs = diff_inprint
replace diffin_obs = diffin_obs[_n-1] if diffin_obs==.
sum diffin_obs


** for titles that would otherwise be out of print
gen double diff_outprint = difftitle if ntitle1==.
gen diffout_obs = diff_outprint
replace diffout_obs = diffout_obs[_n-1] if diffout_obs==.
***********************************************


***********************************************
** scale weighted average impacts to likely number of impacted works (Table 5)
egen mdiff_in_obs = mean(diffin_obs)
egen mdiff_out_obs = mean(diffout_obs)

di mdiff_in_obs*100 + mdiff_out_obs*50
di mdiff_in_obs*150 + mdiff_out_obs*100
di mdiff_in_obs*175 + mdiff_out_obs*150
di mdiff_in_obs*200 + mdiff_out_obs*200
di mdiff_in_obs*250 + mdiff_out_obs*300
***********************************************


*************************************************
** Alternatively, a less conservative estimate:

gsort downloads -have title

gen diffin_high = diff_inprint
replace diffin_high = diffin_high[_n-1] if diffin_high==.

gen diffout_high = diff_outprint
replace diffout_high = diffout_high[_n-1] if diffout_high==.

egen mdiff_in_high = mean(diffin_high)
egen mdiff_out_high = mean(diffout_high)

di mdiff_in_high*100 + mdiff_out_high*50
di mdiff_in_high*150 + mdiff_out_high*100
di mdiff_in_high*175 + mdiff_out_high*150
di mdiff_in_high*200 + mdiff_out_high*200
di mdiff_in_high*250 + mdiff_out_high*300
*************************************************



********************************************************************
** Markups & mc by title-format **** Markups & mc by title-format **
********************************************************************

use tempfile.dta, clear

*************************************************
** rerun regression to store coefficients

ivregress 2sls scaled_diffshare new major age picture i.formats i.tno i.t (price share_inside = newnumb instrument_titleformat), vce(cluster tno)
predict double diffshare_hat
gen double xi = diffshare_hat - diffshare

gen double sigma= _b[share_inside]
gen double alpha = _b[price]
gen double delta = scaled_diffshare - sigma*share_inside

gen double b_new = _b[new]
gen double b_major = _b[major]
gen double b_age = _b[age]
gen double b_picture = _b[picture]
gen double b_cons = _b[_cons]
gen double b_hc = _b[2.formats]
gen double b_pb = _b[4.formats]
gen double b_e = _b[3.formats]
*************************************************


*************************************************
** use these coefficients to back out marginal costs

gen double elasticity = alpha/(1-sigma)*price*(1-sigma*(demand/titledemand)-(1-sigma)*scaled_share)
gen double mc = price*(1+1/elasticity) 

*drop if mc>40 

** Use industry information to get mc for e-books
replace mc = 0 if format=="Gutenberg" 
replace mc = 0.7 if format=="E-book" & post1923==1 & mc<0 /* Royalties from Greco*/
gen markup = (price-mc)/price
*replace mc=0 if mc<0
*keep if new==1 & price<50 & mc>=0

** Note royalties are transfers of wealth - subtract from them MC:
replace mc = mc-(0.15*price) if post==1
replace mc = 0 if format=="E-book"

bysort post1923: sum mc markup amazonprice price if price!=0 & new==1 & price<40 & mc>=0
bysort format: sum mc markup amazonprice price if new==1 & mc>=0 & price!=0 & price<40 & post==0
bysort format: sum mc markup amazonprice price if new==1 & mc>=0 & price!=0 & price<40 & post==1


** Evidence that mc are reasonable:
twoway (lfit mc pages) (scatter mc pages, mcolor(blue) msymbol(X))if format=="Paperback" & post==0 & price!=0 & new==1 & price<40 & mc>=0, scheme(s2mono) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(1 100 200 300 400 500 600 700) xtitle(Number of pages) ytitle(Estimated marginal cost) legend(order(1 "Fitted values"))
*replace mc=0 if mc<0
*************************************************



*************************************************
** use the coefficients to calculate market shares

** note: firms only make $ from new editions
gen delta_new = delta
replace delta_new = delta + b_new if new==0

gen double temp1 = exp(delta_new/(1-sigma))
egen double temp2 = sum(temp1), by(tno t)
gen double temp3 = temp2^(-sigma)
gen double temp5 = temp2^(1-sigma)
replace temp5 = 0 if nn>1
egen double temp4 = sum(temp5), by(t)
replace temp4 = 1+(temp4)

gen double new_share = temp1*temp3/(temp4)
drop temp*

sum new_share scaled_share

** drop Gutenberg because it makes $0:
drop if format=="Gutenberg"

*************************************************



*************************************************
** Annual operating profit when IP=0 is the (annual) physical FC:

gen op_profit_new = (price - mc) * marketsize * new_share	
collapse (mean) ntitle* new (min) nn (sum) op_profit*, by(tno asin post format quality qq) 

sum op_profit_new if post==0, detail
sum op_profit_new if post==1, detail

** fixed cost of production:
gen fc = op_profit_new if post==0

** per title-format:
collapse (mean) fc op_profit_new ntitle quality ntitleformat post, by(tno format)
*************************************************



*************************************************
** keep track of number of editions per in-print and out-of-print title:

tempfile temp1 
save `temp1'

** return to original dataset to collect all editions:
use amazon_data.dta, clear
gen cons = 1
collapse (sum) cons, by(title format)
keep if format =="Hardcover" | format=="Paperback" | format=="E-book"
replace format = "Kindle" if format=="E-book"

** create 0s for nonexistent title-formats
egen ff = group(format)
encode titl, gen(tno)
tsset tno ff
tsfill
replace cons = 0 if cons==.

** add information back to title and format
gsort tno -title
replace title = title[_n-1] if title==""
replace format = "Hardcover" if ff==1
replace format = "Paperback" if ff==3
replace format = "E-book" if ff==2
replace format = "E-book" if format=="Kindle"

** clean data and create edition counts
drop ff tno
ren cons wk_eds
egen w_eds = sum(wk_eds), by(title)

** save that file
tempfile tempeds
save `tempeds'
*************************************************



*************************************************
** add these numbers of editions to the previous data:

use `temp1'

decode tno, gen(title)

merge m:1 title format using `tempeds'
replace ntitleformat = wk_eds if ntitleformat==.
replace ntitle = w_eds if ntitle==.
replace ntitleformat = 0 if ntitleformat==.
replace ntitle = 0 if ntitle==.
drop w_eds wk_eds

** clean data: fix title and format variables:
drop tno 
encode title, gen(tno)
gen j = 1 if format=="Hardcover"
replace j = 2 if format=="Paperback"
replace j=3 if format=="E-book"
drop format _merge

** clean data: drop titles w/o demand info:
egen temp = mean(post), by(tno)
drop if temp==. 
replace post = temp if post==.
egen temp1 = mean(quality), by(tno)
replace quality = temp1 if quality==.
drop if quality==.
drop temp*
*************************************************



*************************************************
* fill in FC from similar public domain titles of the same format if no title-format info exists:

gsort j post -quality
replace fc = fc[_n-1] if fc==. & post==0 & j==j[_n-1]
gsort j post -quality
replace op_profit_new = op_profit_new[_n-1] if op_profit_new==. & post==post[_n-1] & j==j[_n-1]
*************************************************


*************************************************
** fixed costs per qualityrank-format:

** reshape data to get 1 obs per title:
drop fc title ntitle
reshape wide op_profit_new ntitleformat quality, i(tno post) j(j)
drop quality2 quality3
ren quality1 quality

** create quality rank:
gsort post -quality
bys post: gen rank = _n

** update FC per quality rank:
foreach i in 1 2 3 {
gen temp`i' = op_profit_new`i' if post==0
egen fc_`i' = mean(temp`i'), by(rank)
}
drop temp*
*************************************************



*************************************************
** Finally, title profit:

gen wprofit = ((op_profit_new1-fc_1) * ntitleformat1) + ((op_profit_new2-fc_2) * ntitleformat2) + ((op_profit_new3-fc_3) * ntitleformat3)

twoway (scatter wprofit rank, mcolor(blue) msymbol(X)) if post==1 & wprofit<100000, xtitle(IP-specific ranking in creative quality) ytitle(Joint profit for protected works) scheme(s2mono) graphregion(fcolor(white) lcolor(white)) xlabel(,grid  glcolor(gs15)) xlabel(1 25 50 75 100 125)
gen ntitle = ntitleformat1 + ntitleformat2 + ntitleformat3
sum wprofit if post==1 & ntitle>0
*************************************************

