// Replication code for the paper:
** Promises, lies and the accountability trap. Evidence from a survey experiment in Armenia and Georgia
** Koba Turmanidze

** Caucasus Survey 2017
** doi:10.1080/23761199.2017.1385994
** This work was supported by a grant from the Academic Swiss Caucasus Net


*******************************************************************************
*************************** Recode variables **********************************
*******************************************************************************

clear all
use "CRRC_Experiment_AM_GE_30.07.16.dta"

** survey location
recode country (1=0)(2=1), gen(tbilisi)
lab var tbilisi "Tbilisi"

** treatments
recode exp (1=0)(2=1)(3=2)(4=3), gen(treat)
lab var treat "Treatment conditions"
lab define treatment 0 "Abstract" 1 "State-driven" 2 "Market-driven" 3 "Inconsistent"
lab values exp treatment

** age groups
recode age(18/38=1)(39/59=2)(60/99=0)(else=.), gen(age20)

** gender
recode sex(1=0)(2=1),gen(female)

** education
recode q32(1/5=0)(6/7=1)(else=.), gen(edur)
lab var edur "Respondent's education"

** turnout
recode q18(1=1)(2=0)(else=.), gen(turnout)
lab var turnout "Turnout in elections"

** party support
recode q19(1=1)(2=0)(else=.), gen(psupp)
lab var psupp "Party support"

** turnout -- uncertain answers
recode q18(3=1)(-1=1)(1/2=0)(else=.), gen(turnout_u)
lab var turnout_u "Turnout uncertainty"

** party support -- uncertain answers
recode q19(3=1)(-1=1)(1/2=0)(else=.), gen(psupp_u)
lab var psupp_u "Party support uncertainty"

** salience of unemployment
recode q3(1/2=0)(3/4=1)(else=.), gen(u1)
recode q4(1/3=0)(4/5=1)(else=.), gen(u2)
gen unemp=0
replace unemp=1 if u1==1 & u2==1
replace unemp=. if u1==. | u2==.
lab var unemp "Salience of unemployment"

** salience of taxes
recode q5(1/2=0)(3/4=1)(else=.), gen(t1)
recode q6(1/3=0)(4/5=1)(else=.), gen(t2)
gen tax=0
replace tax=1 if t1==1 & t2==1
replace tax=. if t1==. | t2==.
lab var tax "Salience of taxes"

** relative economic conditions
recode q42 (1=1)(2=2)(3=3)(4=4)(5=5)(else=.), gen(relcond)
lab var relcond "Economic conditions relative to neighboring households"

** state intervention in economy 
recode q7(1=1)(2=0)(else=.), gen(st1_a)
recode q8(1=0)(2=1)(else=.), gen(st2_a)
recode q11(1=0)(2=1)(else=.), gen(st3_a)
recode q12(1=1)(2=0)(else=.), gen(st4_a)
egen state=rowtotal(st1_a st2_a st3_a st4_a)
lab var state "State intervention in the economy"

** political efficacy
recode q24 (1/2=0)(3/4=1)(else=.), gen(pe1)
recode q25 (1/2=0)(3/4=1)(else=.), gen(pe2)
recode q26 (1/2=1)(3/4=0)(else=.), gen(pe3)
recode q27 (1/2=1)(3/4=0)(else=.), gen(pe4)
egen peff=rowtotal(pe1 pe2 pe3 pe4)
lab var peff "Political efficacy"

** political knowledge
recode q28 (1=1)(0=0)(-1=0)(else=.), gen(kn1)
recode q29 (1=1)(0=0)(-1=0)(else=.), gen(kn2)
recode q30 (1=1)(0=0)(-1=0)(else=.), gen(kn3)
recode q31 (1=1)(0=0)(-1=0)(else=.), gen(kn4)
egen know=rowtotal(kn1 kn2 kn3 kn4)
lab var know "Political knowledge"

** risk taking
gen riskg=q13
gen riskf=q14
gen riskh=q15
mvdecode riskg riskf riskh, mv(-9/-1)
egen risk=rowmean(riskg riskf riskh)
lab var risk "Overall risk taking"

** order
order id tbilisi treat /// 
age20 female edur ///
turnout psupp turnout_u psupp_u /// 
unemp tax relcond ///
state peff know risk 

drop u1-riskh

save "CRRC_Experiment_AM_GE_CS_2017.dta", replace

*******************************************************************************
************************* Summary statistics **********************************
******************************************************************************* 

//// Table 2. Summary statistics

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

tab age20, gen(age20_)
local Summary "turnout psupp female age20_1 age20_2 age20_3 edur relcond unemp tax state know peff risk"  
logout, save(filename) excel replace: summarize `Summary' if tbilisi==1
logout, save(filename) excel replace: summarize `Summary' if tbilisi==0

*******************************************************************************
************************* Graphs in the paper *********************************
******************************************************************************* 

//// Graph 1. Treatment effects on uncertain answers, logit models
//// Pairwise comparisons with Sidak corrections, 95% CIs

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

qui: logit turnout_u i.treat ///
if tbilisi==1, ///
vce (cluster psu)
qui: margins treat, pwcompare(pveffects sort) mcompare(sidak) post asobserved vce(unconditional)
marginsplot, name(a, replace) ///
horizontal unique xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
recast(scatter) yscale(reverse) ///
title("(a) Tbilisi", color(dknavy*.8) tstyle(size(msmall)) span) ///
graphregion(color(white)) ///
xlabel(-.4(.1).4) xtitle(Comparisons of Pr(Turnout), color(dknavy*.8) tstyle(size(small))) ///
plot1opts(lpatt(solid) lwidth(thin) lcolor(navy) mcolor(navy) msym(d) mcol(navy)) 

qui: logit turnout_u i.treat ///
if tbilisi==0, ///
vce (cluster psu)
qui: margins treat, pwcompare(pveffects sort) mcompare(sidak) post asobserved vce(unconditional)
marginsplot, name(b, replace) ///
horizontal unique xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
recast(scatter) yscale(reverse) ///
title("(b) Yerevan", color(dknavy*.8) tstyle(size(msmall)) span) ///
graphregion(color(white)) ///
xlabel(-.4(.1).4) xtitle(Comparisons of Pr(Turnout), color(dknavy*.8) tstyle(size(small))) ///
plot1opts(lpatt(solid) lwidth(thin) lcolor(navy) mcolor(navy) msym(d) mcol(navy)) 

qui: logit psupp_u i.treat ///
if tbilisi==1, ///
vce (cluster psu)
qui: margins treat, pwcompare(pveffects sort) mcompare(sidak) post asobserved vce(unconditional)
marginsplot, name(c, replace) ///
horizontal unique xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
recast(scatter) yscale(reverse) ///
title("(c) Tbilisi", color(dknavy*.8) tstyle(size(msmall)) span) ///
graphregion(color(white)) ///
xlabel(-.4(.1).4) xtitle(Comparisons of Pr(Support), color(dknavy*.8) tstyle(size(small))) ///
plot1opts(lpatt(solid) lwidth(thin) lcolor(navy) mcolor(navy) msym(d) mcol(navy)) 

qui: logit psupp_u i.treat ///
if tbilisi==0, ///
vce (cluster psu)
qui: margins treat, pwcompare(pveffects sort) mcompare(sidak) post asobserved vce(unconditional)
marginsplot, name(d, replace) ///
horizontal unique xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
recast(scatter) yscale(reverse) ///
title("(d) Yerevan", color(dknavy*.8) tstyle(size(msmall)) span) ///
graphregion(color(white)) ///
xlabel(-.4(.1).4) xtitle(Comparisons of Pr(Support), color(dknavy*.8) tstyle(size(small))) ///
plot1opts(lpatt(solid) lwidth(thin) lcolor(navy) mcolor(navy) msym(d) mcol(navy)) 
 
graph combine a b c d, ///
title("Graph 1. Treatment effects on uncertain answers, logit models", color(dknavy*.9) tstyle(size(medium)) span) ///
subtitle("Pairwise comparisons with Sidak corrections, 95% CIs", color(dknavy*.8) tstyle(size(medium)) span) ///
graphregion(color(white)) ///
note("0=Ambiguous A (Abstract); 1=State-driven; 2=Market-driven; 3=Ambiguous B (Inconsistent)")


//// Graph 2. Treatment effects on turnout and party support (Logit models, treatments only)
//// Marginal effects, 95% confidence intervals

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

qui: logit turnout i.treat ///
if tbilisi==1, vce (cluster psu)
qui: margins, dydx(*) post
est store m1

qui: logit turnout i.treat ///
if tbilisi==0, vce (cluster psu)
qui: margins, dydx(*) post
est store m2

coefplot ///
(m1, label(Tbilisi) lpatt(solid)lcol(ebblue)msym(O)mcol(ebblue)ciopts(lpatt(solid)lcol(ebblue))) ///
(m2, label(Yerevan) lpatt(solid)lcol(eltgreen)msym(T)mcol(eltgreen)ciopts(lpatt(solid)lcol(eltgreen))), ///
name(tr1) ///
baselevels drop(_cons tbilisi 0.female 0.empl1 0.edur1 0.age20) ///
xscale() xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
xlabel(-.4(.1).4) xtitle(Effects on Pr (Turnout)) levels(95) ///
graphregion(color(white)) ///
coeflabels(_cons="Constant" 1.tbilisi="Tbilisi" 0.tbilisi="Yerevan" ///
0.treat="Ambiguous A (Abstract)" 1.treat="State-driven" 2.treat="Market-driven" 3.treat="Ambiguous B (Inconsistent)", ///
wrap(40) notick labcolor(black*.8) labsize(small) labgap(2)) /// 
title("Turnout", color(dknavy*.9) tstyle(size(medium))) ///
subtitle("", color(dknavy*.8) tstyle(size(medium)) span) ///
legend(rows(1))

qui: logit psupp i.treat ///
if tbilisi==1, vce (cluster psu)
qui: margins, dydx(*) post
est store m1

qui: logit psupp i.treat ///
if tbilisi==0, vce (cluster psu)
qui: margins, dydx(*) post
est store m2

coefplot ///
(m1, label(Tbilisi) lpatt(solid)lcol(ebblue)msym(O)mcol(ebblue)ciopts(lpatt(solid)lcol(ebblue))) ///
(m2, label(Yerevan) lpatt(solid)lcol(eltgreen)msym(T)mcol(eltgreen)ciopts(lpatt(solid)lcol(eltgreen))), ///
name(tr2) nolabels ///
baselevels drop(_cons tbilisi 0.female 0.empl1 0.edur1 0.age20) ///
xscale() xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
xlabel(-.4(.1).4) xtitle(Effects on Pr (Support)) levels(95) ///
graphregion(color(white)) ///
coeflabels(_cons="Constant" 1.tbilisi="Tbilisi" 0.tbilisi="Yerevan" ///
0.treat="Ambiguous A (Abstract)" 1.treat="State-driven" 2.treat="Market-driven" 3.treat="Ambiguous B (Inconsistent)", ///
wrap(40) notick labcolor(black*.8) labsize(small) labgap(2)) /// 
title("Party support", color(dknavy*.9) tstyle(size(medium))) ///
subtitle("", color(dknavy*.8) tstyle(size(medium)) span) ///
legend(rows(1))

graph combine tr1 tr2, ///
title("Graph 2. Treatment effects on turnout and party support (Logit models, treatments only)", color(dknavy*.9) tstyle(size(medium)) span) ///
subtitle("Marginal effects, 95% confidence intervals", color(dknavy*.8) tstyle(size(medium)) span) ///
graphregion(color(white))

//// Graph 3. Graph 3. Treatment effects on turnout and party support (Logit models with covariates
//// Marginal effects, 95% confidence intervals

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

qui: logit turnout i.treat i.female i.age20 i.edur relcond unemp tax state know peff risk ///
if tbilisi==1, vce (cluster psu)
qui: margins, dydx(*) post
est store m1

qui: logit turnout i.treat i.female i.age20 i.edur relcond unemp tax state know peff risk ///
if tbilisi==0, vce (cluster psu)
qui: margins, dydx(*) post
est store m2

coefplot ///
(m1, label(Tbilisi) lpatt(solid)lcol(ebblue)msym(O)mcol(ebblue)ciopts(lpatt(solid)lcol(ebblue))) ///
(m2, label(Yerevan) lpatt(solid)lcol(eltgreen)msym(T)mcol(eltgreen)ciopts(lpatt(solid)lcol(eltgreen))), ///
name(co1) ///
baselevels drop(_cons tbilisi 0.female 0.empl1 0.edur1 0.age20) ///
xscale() xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
xlabel(-.4(.1).4) xtitle(Effects on Pr (Turnout)) levels(95) ///
graphregion(color(white)) ///
coeflabels(_cons="Constant" 1.tbilisi="Tbilisi" 0.tbilisi="Yerevan" ///
0.treat="Ambiguous A (Abstract)" 1.treat="State-driven" 2.treat="Market-driven" 3.treat="Ambiguous B (Inconsistent)" ///
1.age20="Age group (18-38)" 2.age20="Age group (39-59)" ///
0.female="Male" 1.female="Female" ///
0.edur="Less than tertiary" 1.edur="Tertiary education" ///
unemp="Salience of unemployment" tax="Salience of taxes" ///
state="State intervention" know="Political knowledge" ///
risk="Risk taking" relcond="Economic conditions" peff="Political efficacy", ///
wrap(40) notick labcolor(black*.8) labsize(small) labgap(2)) /// 
title("Turnout", color(dknavy*.9) tstyle(size(medium))) ///
subtitle("", color(dknavy*.8) tstyle(size(medium)) span) ///
legend(rows(1))

qui: logit psupp i.treat i.female i.age20 i.edur relcond unemp tax state know peff risk ///
if tbilisi==1, vce (cluster psu)
qui: margins, dydx(*) post
est store m1

qui: logit psupp i.treat i.female i.age20 i.edur relcond unemp tax state know peff risk ///
if tbilisi==0, vce (cluster psu)
qui: margins, dydx(*) post
est store m2

coefplot ///
(m1, label(Tbilisi) lpatt(solid)lcol(ebblue)msym(O)mcol(ebblue)ciopts(lpatt(solid)lcol(ebblue))) ///
(m2, label(Yerevan) lpatt(solid)lcol(eltgreen)msym(T)mcol(eltgreen)ciopts(lpatt(solid)lcol(eltgreen))), ///
name(co2) nolabels ///
baselevels drop(_cons tbilisi 0.female 0.empl1 0.edur1 0.age20) ///
xscale() xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
xlabel(-.4(.1).4) xtitle(Effects on Pr (Support)) levels(95) ///
graphregion(color(white)) ///
coeflabels(_cons="Constant" 1.tbilisi="Tbilisi" 0.tbilisi="Yerevan" ///
0.treat="Ambiguous A (Abstract)" 1.treat="State-driven" 2.treat="Market-driven" 3.treat="Ambiguous B (Inconsistent)" ///
1.age20="Age group (18-38)" 2.age20="Age group (39-59)" ///
0.female="Male" 1.female="Female" ///
0.edur="Less than tertiary" 1.edur="Tertiary education" ///
unemp="Salience of unemployment" tax="Salience of taxes" ///
state="State intervention" know="Political knowledge" ///
risk="Risk taking" relcond="Economic conditions" peff="Political efficacy", ///
wrap(40) notick labcolor(black*.8) labsize(small) labgap(2)) /// 
title("Party support", color(dknavy*.9) tstyle(size(medium))) ///
subtitle("", color(dknavy*.8) tstyle(size(medium)) span) ///
legend(rows(1))

graph combine co1 co2, ///
title("Graph 3. Treatment effects on turnout and party support (Logit models with covariates)", color(dknavy*.9) tstyle(size(medium)) span) ///
subtitle("Marginal effects, 95% confidence intervals", color(dknavy*.8) tstyle(size(medium)) span) ///
graphregion(color(white))


//// Graph 4. Pairwise comparisons of treatment effects (Logit models)
//// Sidak's corrections, 95% Confidence Intervals

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

qui: logit turnout i.treat i.female i.age20 i.edur relcond unemp tax state know peff risk ///
if tbilisi==1, vce (cluster psu)

qui: margins treat, pwcompare(pveffects sort) mcompare(sidak) post asobserved vce(unconditional)
marginsplot, name(a, replace) ///
horizontal unique xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
recast(scatter) yscale(reverse) ///
title("(a) Tbilisi", color(dknavy*.8) tstyle(size(msmall)) span) ///
graphregion(color(white)) ///
xlabel(-.4(.1).4) xtitle(Comparisons of Pr(Turnout), color(dknavy*.8) tstyle(size(small))) ///
plot1opts(lpatt(solid) lwidth(thin) lcolor(navy) mcolor(navy) msym(d) mcol(navy))   

qui: logit turnout i.treat i.female i.age20 i.edur relcond unemp tax state know peff risk ///
if tbilisi==0, vce (cluster psu)

qui: margins treat, pwcompare(pveffects sort) mcompare(sidak) post asobserved vce(unconditional)
marginsplot, name(b, replace) ///
horizontal unique xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
recast(scatter) yscale(reverse) ///
title("(b) Yerevan", color(dknavy*.8) tstyle(size(msmall)) span) ///
graphregion(color(white)) ///
xlabel(-.4(.1).4) xtitle(Comparisons of Pr(Turnout), color(dknavy*.8) tstyle(size(small))) ///
plot1opts(lpatt(solid) lwidth(thin) lcolor(navy) mcolor(navy) msym(d) mcol(navy))  

qui: logit psupp i.treat i.female i.age20 i.edur relcond unemp tax state know peff risk ///
if tbilisi==1, vce (cluster psu)

qui: margins treat, pwcompare(pveffects sort) mcompare(sidak) post asobserved vce(unconditional)
marginsplot, name(c, replace) ///
horizontal unique xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
recast(scatter) yscale(reverse) ///
title("(c) Tbilisi", color(dknavy*.8) tstyle(size(msmall)) span) ///
graphregion(color(white)) ///
xlabel(-.4(.1).4) xtitle(Comparisons of Pr(Support), color(dknavy*.8) tstyle(size(small))) ///
plot1opts(lpatt(solid) lwidth(thin) lcolor(navy) mcolor(navy) msym(d) mcol(navy))  

qui: logit psupp i.treat i.female i.age20 i.edur relcond unemp tax state know peff risk ///
if tbilisi==0, vce (cluster psu)

qui: margins treat, pwcompare(pveffects sort) mcompare(sidak) post asobserved vce(unconditional)
marginsplot, name(d, replace) ///
horizontal unique xline(0, lcolor(red) lwidth(thin) lpattern(dash)) ///
recast(scatter) yscale(reverse) ///
title("(d) Yerevan", color(dknavy*.8) tstyle(size(msmall)) span) ///
graphregion(color(white)) ///
xlabel(-.4(.1).4) xtitle(Comparisons of Pr(Support), color(dknavy*.8) tstyle(size(small))) ///
plot1opts(lpatt(solid) lwidth(thin) lcolor(navy) mcolor(navy) msym(d) mcol(navy))  

graph combine a b c d, ///
title("Graph 4. Pairwise comparisons of treatment effects (Logit models)", color(dknavy*.9) tstyle(size(medium)) span) ///
subtitle("Sidak's corrections, 95% Confidence Intervals", color(dknavy*.8) tstyle(size(medium)) span) ///
graphregion(color(white)) ///
note("0=Ambiguous A (Abstract); 1=State-driven; 2=Market-driven; 3=Ambiguous B (Inconsistent)")


*******************************************************************************
********************* Tables in the online appendix ***************************
******************************************************************************* 

//// Table A1. Logit Estimates of the treatment effects on turnout and party support (odds ratios)

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

qui eststo: logit turnout i.treat if tbilisi==1, vce (cluster psu) or
qui eststo: logit turnout i.treat i.female i.age20 i.edur relcond ///
i.unemp i.tax state know peff risk ///
if tbilisi==1, vce (cluster psu) or

qui eststo: logit psupp i.treat if tbilisi==1, vce (cluster psu) or
qui eststo: logit psupp i.treat i.female i.age20 i.edur relcond ///
i.unemp i.tax state know peff risk ///
if tbilisi==1, vce (cluster psu) or

qui eststo: logit turnout i.treat if tbilisi==0, vce (cluster psu) or
qui eststo: logit turnout i.treat i.female i.age20 i.edur relcond ///
i.unemp i.tax state know peff risk ///
if tbilisi==0, vce (cluster psu) or

qui eststo: logit psupp i.treat if tbilisi==0, vce (cluster psu) or
qui eststo: logit psupp i.treat i.female i.age20 i.edur relcond ///
i.unemp i.tax state know peff risk ///
if tbilisi==0, vce (cluster psu) or

esttab using "filename", ///
eform b(2) se(2) scalars(ll bic aic) ///
compress varwidth(30) modelwidth(5) nogap onecell nobaselevels ///
varlabels(_cons "Constant" ///
0.treat "Ambiguous A (Abstract)" 1.treat "State-driven" 2.treat "Market-driven" 3.treat "Ambiguous B (Inconsistent)" ///
1.female "Female" ///
1.age20 "Age group (18 to 38)" 2.age20 "Age group (39 to 59)"  ///
1.edur "Tertiary education" ///
relcond "Economic conditions" ///
1.unemp "Salience of unemployment" ///
1.tax "Salience of taxes" ///
state "State intervention" ///
know "Political knowledge" ///
peff "Political efficacy" ///
risk "Risk taking" ///
1.tbilisi "Tbilisi" ///
1.treat#1.tbilisi "State-driven x Tbilisi" ///
2.treat#1.tbilisi "Market-driven x Tbilisi" ///
3.treat#1.tbilisi "Inconsistent x Tbilisi") ///
star(* 0.10 ** 0.05 *** 0.01) ///
title ({\b Table A1.} {\i Logit Estimates of the treatment effects on turnout and party support (odds ratios)}) ///
nonumbers mtitles("(1) Turnout Tbilisi" "(2) Turnout Tbilisi" "(3) Support Tbilisi" "(4) Support Tbilisi" "(5) Turnout Yerevan" "(6) Turnout Yerevan" "(7) Support Yerevan" "(8) Support Yerevan") ///
addnote("Robust clustered standard errors in parentheses")

//// Table A2. Logit estimates of heterogenous treatment effects on turnout (odds ratios, Tbilisi)

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

foreach v in i.female i.age20 i.edur c.relcond ///
i.unemp i.tax c.state c.know c.peff c.risk {
eststo: qui logit turnout i.treat##`v' if tbilisi==1, vce (cluster psu) or 
}

esttab using "filename", ///
eform b(2) se(2) scalars(ll bic aic) ///
compress varwidth(25) modelwidth(3) nogap onecell nobaselevels ///
varlabels(_cons "Constant" ///
0.treat "Ambiguous A (Abstract)" 1.treat "State-driven" 2.treat "Market-driven" 3.treat "Ambiguous B (Inconsistent)" ///
1.female "Female" ///
1.age20 "Age 18 to 38" 2.age20 "Age 39 to 59"  ///
1.edur "Tertiary education" ///
relcond "Economic conditions" ///
1.unemp "Salience of unemployment" ///
1.tax "Salience of taxes" ///
state "State intervention" ///
know "Political knowledge" ///
peff "Political efficacy" ///
risk "Risk taking" ///
1.treat#1.female "State x Female" ///
2.treat#1.female "Market x Female" ///
3.treat#1.female "Inconsistent x Female" ///
1.treat#1.age20 "State x Age 18 to 38" ///
2.treat#1.age20 "Market x Age 18 to 38" ///
3.treat#1.age20 "Inconsistent x Age 18 to 38" ///
1.treat#2.age20 "State x Age 39 to 59" ///
2.treat#2.age20 "Market x Age 39 to 59" ///
3.treat#2.age20 "Inconsistent x Age 39 to 59" ///
1.treat#1.edur "State x Education" ///
2.treat#1.edur "Market x Education" ///
3.treat#1.edur "Inconsistent x Education" ///
1.treat#c.relcond "State x Ec. Conditions" ///
2.treat#c.relcond "Market x Ec. Conditions" ///
3.treat#c.relcond "Inconsistent x Ec. Conditions" ///
1.treat#1.unemp "State x Unemployment" ///
2.treat#1.unemp "Market x Unemployment" ///
3.treat#1.unemp "Inconsistent x Unemployment" ///
1.treat#1.tax "State x Taxes" ///
2.treat#1.tax "Market x Taxes" ///
3.treat#1.tax "Inconsistent x Taxes" ///
1.treat#c.state "State x Intervention" ///
2.treat#c.state "Market x Intervention" ///
3.treat#c.state "Inconsistent x Intervention" ///
1.treat#c.know "State x Knowledge" ///
2.treat#c.know "Market x Knowledge" ///
3.treat#c.know "Inconsistent x Knowledge" ///
1.treat#c.peff "State x Efficacy" ///
2.treat#c.peff "Market x Efficacy" ///
3.treat#c.peff "Inconsistent x Efficacy" ///
1.treat#c.risk "State x Risk" ///
2.treat#c.risk "Market x Risk" ///
3.treat#c.risk "Inconsistent x Risk") ///
star(* 0.10 ** 0.05 *** 0.01) ///
title ({\b Table A2.} {\i Logit estimates of heterogenous treatment effects on turnout (odds ratios, Tbilisi)}) ///
nonumbers mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)") ///
addnote("Robust clustered standard errors in parentheses")

//// Table A3. Logit estimates of heterogenous treatment effects on turnout (odds ratios, Yerevan)

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

foreach v in i.female i.age20 i.edur c.relcond ///
i.unemp i.tax c.state c.know c.peff c.risk {
eststo: qui logit turnout i.treat##`v' if tbilisi==0, vce (cluster psu) or 
}

esttab using "filename", ///
eform b(2) se(2) scalars(ll bic aic) ///
compress varwidth(25) modelwidth(3) nogap onecell nobaselevels ///
varlabels(_cons "Constant" ///
0.treat "Ambiguous A (Abstract)" 1.treat "State-driven" 2.treat "Market-driven" 3.treat "Ambiguous B (Inconsistent)" ///
1.female "Female" ///
1.age20 "Age 18 to 38" 2.age20 "Age 39 to 59"  ///
1.edur "Tertiary education" ///
relcond "Economic conditions" ///
1.unemp "Salience of unemployment" ///
1.tax "Salience of taxes" ///
state "State intervention" ///
know "Political knowledge" ///
peff "Political efficacy" ///
risk "Risk taking" ///
1.treat#1.female "State x Female" ///
2.treat#1.female "Market x Female" ///
3.treat#1.female "Inconsistent x Female" ///
1.treat#1.age20 "State x Age 18 to 38" ///
2.treat#1.age20 "Market x Age 18 to 38" ///
3.treat#1.age20 "Inconsistent x Age 18 to 38" ///
1.treat#2.age20 "State x Age 39 to 59" ///
2.treat#2.age20 "Market x Age 39 to 59" ///
3.treat#2.age20 "Inconsistent x Age 39 to 59" ///
1.treat#1.edur "State x Education" ///
2.treat#1.edur "Market x Education" ///
3.treat#1.edur "Inconsistent x Education" ///
1.treat#c.relcond "State x Ec. Conditions" ///
2.treat#c.relcond "Market x Ec. Conditions" ///
3.treat#c.relcond "Inconsistent x Ec. Conditions" ///
1.treat#1.unemp "State x Unemployment" ///
2.treat#1.unemp "Market x Unemployment" ///
3.treat#1.unemp "Inconsistent x Unemployment" ///
1.treat#1.tax "State x Taxes" ///
2.treat#1.tax "Market x Taxes" ///
3.treat#1.tax "Inconsistent x Taxes" ///
1.treat#c.state "State x Intervention" ///
2.treat#c.state "Market x Intervention" ///
3.treat#c.state "Inconsistent x Intervention" ///
1.treat#c.know "State x Knowledge" ///
2.treat#c.know "Market x Knowledge" ///
3.treat#c.know "Inconsistent x Knowledge" ///
1.treat#c.peff "State x Efficacy" ///
2.treat#c.peff "Market x Efficacy" ///
3.treat#c.peff "Inconsistent x Efficacy" ///
1.treat#c.risk "State x Risk" ///
2.treat#c.risk "Market x Risk" ///
3.treat#c.risk "Inconsistent x Risk") ///
star(* 0.10 ** 0.05 *** 0.01) ///
title ({\b Table A3.} {\i Logit estimates of heterogenous treatment effects on turnout (odds ratios, Yerevan)}) ///
nonumbers mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)") ///
addnote("Robust clustered standard errors in parentheses")

//// Table A4. Logit estimates of heterogenous treatment effects on party support (odds ratios, Tbilisi)

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

foreach v in i.female i.age20 i.edur c.relcond ///
i.unemp i.tax c.state c.know c.peff c.risk {
eststo: qui logit psupp i.treat##`v' if tbilisi==1, vce (cluster psu) or 
}

esttab using "filename", ///
eform b(2) se(2) scalars(ll bic aic) ///
compress varwidth(25) modelwidth(3) nogap onecell nobaselevels ///
varlabels(_cons "Constant" ///
0.treat "Ambiguous A (Abstract)" 1.treat "State-driven" 2.treat "Market-driven" 3.treat "Ambiguous B (Inconsistent)" ///
1.female "Female" ///
1.age20 "Age 18 to 38" 2.age20 "Age 39 to 59"  ///
1.edur "Tertiary education" ///
relcond "Economic conditions" ///
1.unemp "Salience of unemployment" ///
1.tax "Salience of taxes" ///
state "State intervention" ///
know "Political knowledge" ///
peff "Political efficacy" ///
risk "Risk taking" ///
1.treat#1.female "State x Female" ///
2.treat#1.female "Market x Female" ///
3.treat#1.female "Inconsistent x Female" ///
1.treat#1.age20 "State x Age 18 to 38" ///
2.treat#1.age20 "Market x Age 18 to 38" ///
3.treat#1.age20 "Inconsistent x Age 18 to 38" ///
1.treat#2.age20 "State x Age 39 to 59" ///
2.treat#2.age20 "Market x Age 39 to 59" ///
3.treat#2.age20 "Inconsistent x Age 39 to 59" ///
1.treat#1.edur "State x Education" ///
2.treat#1.edur "Market x Education" ///
3.treat#1.edur "Inconsistent x Education" ///
1.treat#c.relcond "State x Ec. Conditions" ///
2.treat#c.relcond "Market x Ec. Conditions" ///
3.treat#c.relcond "Inconsistent x Ec. Conditions" ///
1.treat#1.unemp "State x Unemployment" ///
2.treat#1.unemp "Market x Unemployment" ///
3.treat#1.unemp "Inconsistent x Unemployment" ///
1.treat#1.tax "State x Taxes" ///
2.treat#1.tax "Market x Taxes" ///
3.treat#1.tax "Inconsistent x Taxes" ///
1.treat#c.state "State x Intervention" ///
2.treat#c.state "Market x Intervention" ///
3.treat#c.state "Inconsistent x Intervention" ///
1.treat#c.know "State x Knowledge" ///
2.treat#c.know "Market x Knowledge" ///
3.treat#c.know "Inconsistent x Knowledge" ///
1.treat#c.peff "State x Efficacy" ///
2.treat#c.peff "Market x Efficacy" ///
3.treat#c.peff "Inconsistent x Efficacy" ///
1.treat#c.risk "State x Risk" ///
2.treat#c.risk "Market x Risk" ///
3.treat#c.risk "Inconsistent x Risk") ///
star(* 0.10 ** 0.05 *** 0.01) ///
title ({\b Table A4.} {\i Logit estimates of heterogenous treatment effects on party support (odds ratios, Tbilisi)}) ///
nonumbers mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)") ///
addnote("Robust clustered standard errors in parentheses")

//// Table A5. Logit estimates of heterogenous treatment effects on party support (odds ratios, Yerevan)

clear all
use "CRRC_Experiment_AM_GE_CS_2017.dta"

foreach v in i.female i.age20 i.edur c.relcond ///
i.unemp i.tax c.state c.know c.peff c.risk {
eststo: qui logit psupp i.treat##`v' if tbilisi==0, vce (cluster psu) or 
}

esttab using "filename", ///
eform b(2) se(2) scalars(ll bic aic) ///
compress varwidth(25) modelwidth(3) nogap onecell nobaselevels ///
varlabels(_cons "Constant" ///
0.treat "Ambiguous A (Abstract)" 1.treat "State-driven" 2.treat "Market-driven" 3.treat "Ambiguous B (Inconsistent)" ///
1.female "Female" ///
1.age20 "Age 18 to 38" 2.age20 "Age 39 to 59"  ///
1.edur "Tertiary education" ///
relcond "Economic conditions" ///
1.unemp "Salience of unemployment" ///
1.tax "Salience of taxes" ///
state "State intervention" ///
know "Political knowledge" ///
peff "Political efficacy" ///
risk "Risk taking" ///
1.treat#1.female "State x Female" ///
2.treat#1.female "Market x Female" ///
3.treat#1.female "Inconsistent x Female" ///
1.treat#1.age20 "State x Age 18 to 38" ///
2.treat#1.age20 "Market x Age 18 to 38" ///
3.treat#1.age20 "Inconsistent x Age 18 to 38" ///
1.treat#2.age20 "State x Age 39 to 59" ///
2.treat#2.age20 "Market x Age 39 to 59" ///
3.treat#2.age20 "Inconsistent x Age 39 to 59" ///
1.treat#1.edur "State x Education" ///
2.treat#1.edur "Market x Education" ///
3.treat#1.edur "Inconsistent x Education" ///
1.treat#c.relcond "State x Ec. Conditions" ///
2.treat#c.relcond "Market x Ec. Conditions" ///
3.treat#c.relcond "Inconsistent x Ec. Conditions" ///
1.treat#1.unemp "State x Unemployment" ///
2.treat#1.unemp "Market x Unemployment" ///
3.treat#1.unemp "Inconsistent x Unemployment" ///
1.treat#1.tax "State x Taxes" ///
2.treat#1.tax "Market x Taxes" ///
3.treat#1.tax "Inconsistent x Taxes" ///
1.treat#c.state "State x Intervention" ///
2.treat#c.state "Market x Intervention" ///
3.treat#c.state "Inconsistent x Intervention" ///
1.treat#c.know "State x Knowledge" ///
2.treat#c.know "Market x Knowledge" ///
3.treat#c.know "Inconsistent x Knowledge" ///
1.treat#c.peff "State x Efficacy" ///
2.treat#c.peff "Market x Efficacy" ///
3.treat#c.peff "Inconsistent x Efficacy" ///
1.treat#c.risk "State x Risk" ///
2.treat#c.risk "Market x Risk" ///
3.treat#c.risk "Inconsistent x Risk") ///
star(* 0.10 ** 0.05 *** 0.01) ///
title ({\b Table A5.} {\i Logit estimates of heterogenous treatment effects on party support (odds ratios, Yerevan)}) ///
nonumbers mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)" "(11)" "(12)") ///
addnote("Robust clustered standard errors in parentheses")
