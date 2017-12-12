** Negative Binomial of remittances and outmigration on homicidal violence

/* -1 clean masterdata from nonexistent municipalities
clear
use C:\Tesis\Masterdata\Masterdata.dta
order idmun, a(NomMun)
sort idedomun year
replace idmun = idmun[_n-1] if idmun ==. & idedomun[_n-1] == idedomun
recode idmun (7121=121)
recode idmun (7120=120) 
drop if idmun > 570
distinct idedomun
/* Vamos a contar los municipios por estados : una bronca está en Chiapas
keep if idedo == 7
gen dup = 0
replace dup = 1 if idedomun[_n-1] == idedomun
drop if dup > 0

*** Se saltan el número 95 y hay 4 municipios nuevos. Las cifras ya cuadran
*/

order IIM_vivrem IIM_viv_total IIM_viv_emig IIM_viv_circ IIM_viv_ret IIM_indice IIM_mig_grado, a(idmun)
sort idedomun year

/* revisar missing values 2000: Sólo hay missing values en los municipios que no existían antes del 2000 
gen migmiss2000 = 0
replace migmiss2000 = 1 if IIM_viv_emig ==. & year == 2000
gsort- migmiss2000
li idedomun NomMun migmiss2000 in 1/50

* revisar missing values 2010: Sólo falta la información para los municipios que se crearon después de 2010
gen migmiss2010 = 0
replace migmiss2010 = 1 if IIM_viv_emig==. & year == 2010
gsort- migmiss2010
li idedomun NomMun migmiss2010 in 1/50

*/
save C:\Tesis\Masterdata\Masterdata0.dta, replace

* 0 Include ENIGH poverty


* 1 Include poverty


* 1.1.1 % of the population below the poverty line for year 2000
clear
cd "C:\Users\Pancho\Dropbox\Base EVES (Preliminar)\"
import delimited pobalimentaria2000.csv, delimiter(";") clear
sort pobalimentaria2000
rename clavedelmunicipio idedomun
rename pobalimentaria2000 pobalimentaria
drop municipio
gen year = 2000
sort idedomun year
merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata0.dta // se combinaron 2,456 mun. faltaron los 4 chiapanecos y Bacalar
drop _merge
sort idedomun year
save C:\Tesis\Masterdata\Masterdata1.dta, replace

* 1.1.2 we include illiteracy for the year 2000
import delimited analfabeta2000.csv, delimiter(";") clear
drop if nombre == "No especificado" | nombre == "Otros estados" |clave >= 32059 ///
| nombre == "Otros municipios" | clave >= 20600 & clave <= 20999
rename clave idedomun
gen illiterate = nosabeleeryescribir / total
keep idedomun illiterate
gen year = 2000
sort idedomun year
merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata1.dta // faltan los 4 chiapanecos, si incluye Bacalar 
drop _merge
save C:\Tesis\Masterdata\Masterdata2.dta, replace

* 1.2.1 now we do the same for the year 2010, first pobalimentaria
import delimited pobalimentaria2010.csv, delimiter(";") clear
rename clavedelmunicipio idedomun
distinct idedomun
gen year = 2010
sort idedomun year
merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata2.dta // faltan los 4 chiapanecos
drop _merge
save C:\Tesis\Masterdata\Masterdata3.dta, replace

* 1.2.2 illiteracy for the year 2010
import delimited analfabeta2010.csv, delimiter(";") clear 
distinct clave
drop if nombre == "No especificado" | nombre == "Otros estados" |clave >= 32059 ///
| nombre == "Otros municipios" | clave >= 20600 & clave <= 20999
rename clave idedomun
gen illiterate = nosabeleeryescribir / total
gen year = 2010
keep idedomun illiterate year
sort idedomun year
merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata3.dta  // faltan los 4 chiapanecos
drop _merge

* 1.3 now we create our poverty variable and impute the data for the 2001-2009 period

gen poverty =  pobalimentaria * illiterate
gen povertyVf = poverty if year==2010
gsort - idedomun - year
drop municipio
rename NomMun nommun
replace povertyVf = povertyVf[_n-1] if povertyVf==. & idedomun[_n-1]==idedomun

sort idedomun year
gen povertyVi = poverty if year == 2000
replace povertyVi = povertyVi[_n-1] if poverty==. & idedomun[_n-1]==idedomun
gen Rpoverty = ((povertyVf / povertyVi)^.1) - 1
replace poverty = poverty[_n-1] * (1+Rpoverty) if poverty==. & idedomun[_n-1]==idedomun
sum poverty
save C:\Tesis\Masterdata\Masterdata4.dta, replace

*if you want to check
//keep if year >=2000
//sort year
//collapse poverty, by(year)
//twoway connected poverty year


* 2.1 we include population older than 15 without secondary education for the year 2000
 import delimited pobbasiced2000.csv, delimiter(";") clear
 drop if 
 nombre == "No especificado" | nombre == "Otros estados" |clave >= 32059 ///
| nombre == "Otros municipios" | clave >= 20600 & clave <= 20999
 distinct clave
  rename clave idedomun
 rename ratebasiced edu15delay
 gen year = 2000 
 sort idedomun year
 drop nombre
 merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata4.dta
drop _merge
save C:\Tesis\Masterdata\Masterdata5.dta, replace


* 2.2 include population older than 15 w.o. secondary education for the year 2010
 import delimited pobbasiced2010.csv, delimiter(";") clear

 egen OK = anymatch(clave), values(1996 1997 2996 2997 3996 3997 4996 4997 5996 ///
 5997 6996 6997 7996 7997 8996 8997 8996 8997 9996 9997 10996 10997 11996 11997 ///
 12996 12997 13996 13997 14996 14997 15996 15997 16996 16997 17996 17997 18996 18997 ///
 19996 19997 20996 20997 21996 21997 22996 22997 23996 23997 24996 24997 ///
 25996 25997 26996 26997 27996 27997 28996 28997 29996 29997 30996 30997 ///
 31996 31997 32996 32997 1998 2998 3998 4998 5998 6998 7998 8998 9998 10998 11998 ///
 12998 13998 14998 15998 16998 17998 18998 19998 20998 21998 22998 23998 24998 ///
 25998 26998 27998 28998 29998 30998 31998 32998)
 drop if OK
 drop if clave >= 20571 & clave <= 20999
 distinct clave
 
 rename clave idedomun
 gen year = 2010
 sort idedomun year
 merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata5.dta
drop _merge

* 2.3 impute the secondary education data for the missing years 2001-2009

gen edu15delayVf = edu15delay if year == 2010
gsort - idedomun - year
replace edu15delayVf = edu15delayVf[_n-1] if edu15delayVf==. & idedomun[_n-1]==idedomun
sort idedomun year
gen edu15delayVi = edu15delay if year == 2000
replace edu15delayVi = edu15delayVi[_n-1] if edu15delayVi==. & idedomun[_n-1]==idedomun
gen Redu15delay = ((edu15delayVf / edu15delayVi)^.1) - 1
replace edu15delay = edu15delay[_n-1] * (1 + Redu15delay) if edu15delay==. & idedomun[_n+1]==idedomun
sum edu15delay
save C:\Tesis\Masterdata\Masterdata6.dta, replace

* 3.1 inglude Gini coefficient for the year 2000
import delimited gini2000.csv, delimiter(";") clear
gen year = 2000
sort idedomun year
 merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata6.dta
 drop _merge
 sort idedomun year
 save C:\Tesis\Masterdata\Masterdata7.dta, replace
 
* 3.2 include Gini coef. for the year 2010

cd "C:\Users\Pancho\Dropbox\Base EVES (Preliminar)\"
import delimited gini2010.csv, delimiter(";") clear
gen year = 2010
sort idedomun year
merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata7.dta
 drop _merge
 sort idedomun year
 
* 3.3 impute gini data for the 2001-2009 period
gen giniVf = gini if year == 2010
gsort - idedomun - year
replace giniVf = giniVf[_n-1] if giniVf==. & idedomun[_n-1]==idedomun
sort idedomun year
gen giniVi = gini if year==2000


replace giniVi = giniVi[_n-1] if giniVi==. & idedomun[_n-1]==idedomun
gen Rgini = ((giniVf / giniVi)^.1) - 1
replace gini = gini[_n-1] * (1 + Rgini) if gini==. & idedomun[_n+1]==idedomun
sum gini

* 3.4 gen Gini squared
gen gini2 = gini^2
gen lngini = ln(gini)

save C:\Tesis\Masterdata\Masterdata8.dta, replace


 * 4.1 include per capita income for the year 2010 (The other ones exist already)
clear
cd "C:\Users\Pancho\Dropbox\Base EVES (Preliminar)\"
import delimited  IDH_ingpc2010.csv, delimiter(";") clear
keep idedomun idh_ingpc
rename idh_ingpc IDH_ingpc
drop if idedomun ==.
gen year = 2010
sort idedomun year
 merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata8.dta
 drop _merge
 sort idedomun year
 
* 4.2.1 now we impute the per capita income data for the 2001 - 2004 period
rename IDH_ingpc pibpc
gen pibpc2005 = pibpc if year == 2005
gsort - idedomun - year
replace pibpc2005 = pibpc2005[_n-1] if pibpc2005==. & idedomun[_n-1]==idedomun
sort idedomun year
gen pibpc2000 = pibpc if year==2000
replace pibpc2000 = pibpc2000[_n-1] if pibpc2000==. & idedomun[_n-1]==idedomun
gen Rpibpc = ((pibpc2005 / pibpc2000)^.2) - 1
replace pibpc = pibpc[_n-1] * (1 + Rpibpc) if pibpc==. & idedomun[_n+1]==idedomun & year >= 2000 & year <= 2005

* 4.2.2 now we impute the per capita income data for the 2006-2009 period
gen pibpc2010 = pibpc if year == 2010
gsort - idedomun - year
replace pibpc2010 = pibpc2010[_n-1] if pibpc2010==. & idedomun[_n-1]==idedomun
sort idedomun year
replace pibpc2005 = pibpc2005[_n-1] if pibpc2005==. & idedomun[_n-1]==idedomun
gen R2pibpc = ((pibpc2010 / pibpc2005)^.2) - 1
replace pibpc = pibpc[_n-1] * (1 + R2pibpc) if pibpc==. & idedomun[_n+1]==idedomun & year >= 2005 & year <= 2010
sum pibpc

*4.3 create logarithmic transformation of GDP per capita
gen lnpibpc = ln(pibpc)


 
 * 5. Foreign Direct Investment
sort idedo year
merge m:1 idedo year using IED //se juntan
sort idedomun year
drop _merge

** We tell Stata that we have panel data

//shall we limit our time frame to the post 2000 years since we have
//municipal migration data starting from those years? Perhaps a little earlier
//in order to capture the ongoing ENPs in 2000 dynamics

* 5. municipal election dummy 

gen munelect = 1  //dummy for municipal election
replace munelect = 0 if vtotalr6 ==.

* 6. gubernatorial election dummy
gen gubelect = 1 // dummy for gubernatorial election
replace gubelect = 0 if vtotalr7 ==.

* 7. federal election dummy
gen fedelect = 1 // dummy for gubernatorial election
replace fedelect = 0 if vtotalr1==.

* 8. federal concurrence 
gen fedconcur = 0
replace fedconcur = 1 if munelect + fedelect == 2

* 9. state concurrence
gen stateconcur = 0
replace stateconcur = 1 if munelect + gubelect ==2

* 10. Municipal Taagepera

* 10.1. gen square proportions
gen munPANsq = (PAN_p6)^2
gen munPRIsq = (PRI_p6)^2
gen munPRDsq = (PRD_p6)^2
gen munOtrossq =(Otros_p6)^2

* 10.2. gen Taagepera
gen munENP = 1 / (munPANsq + munPRIsq + munPRDsq + munOtrossq)

* 10.3. we extrapolate the Effective number of parties to the non-election years
replace munENP = munENP[_n-1] if munENP==. & idedomun[_n-1]==idedomun

* 11. Gubernatorial Taagepera
* 11.1 gen square proportions
gen gubPANsq = (PAN_p7)^2
gen gubPRIsq = (PRI_p7)^2
gen gubPRDsq = (PRD_p7)^2
gen gubOtrossq = (Otros_p7)^2

* 11.2 gen Taagepera
gen gubENP = 1 / (gubPANsq + gubPRIsq + gubPRDsq + gubOtrossq)

* 11.3 extrapolate the gubernatorial Taagepera to non-election years 
replace gubENP = gubENP[_n-1] if gubENP==. & idedomun[_n-1]==idedomun

* 11.4 we generate the interaction between municipal and state electoral competitio
gen local comp = munENP*gubENP
sum localcomp

* 11.5 we elevate localcomp to the 2
gen localcomp2 = localcomp^2

* 12. Lagged homicides
sort idedomun year
 by idedomun: gen laghom_simbad = hom_simbad[_n-1] if idedomun[_n-1]==idedomun
 

/** 12.1 Non-zero homicide variable: this is to have a dycothomic variable to make the Logit model (Trejo's model 2)
by idedomun: gen nonzero = 0
sum nonzero
replace nonzero = 1 if homicidios > 0 
sum nonzero
by idedomun: replace nonzero = 1 if nonzero[_n-1] == 1 & idedomun[_n-1]==idedomun

sum homicidios, detail
gen nonzero = 0
replace nonzero = 1 if homicidios > 0 & idedomun[_n-1]==idedomun
sum nonzero*/

* 13. Rural corporatism
gen ruralcorp = PRI_p6 * part6
* 13.1 extrapolate for the next years
replace ruralcorp = ruralcorp[_n-1] if ruralcorp==. & idedomun[_n-1]==idedomun
sum ruralcorp

* 14. Population, we generate the natural logarythm but als the quadratic term
gen lnpop = ln(pob_total_est)
gen pop2 = pob_total_est^2
gen pop = pob_total_est
sum lnpop

* 15. Economic crises dummy variable
gen crisis = 0
replace crisis = 1 if year == 1982
replace crisis = 1 if year == 1985
replace crisis = 1 if year == 1986
replace crisis = 1 if year == 1995
replace crisis = 1 if year == 2000
replace crisis = 1 if year == 2001
replace crisis = 1 if year == 2002
replace crisis = 1 if year == 2003
replace crisis = 1 if year == 2008
replace crisis = 1 if year == 2009
replace crisis = 1 if year == 2010

/* 16. Economic policy changes
gen ecopol = 0
replace ecopol = 1 if year >= 1984
replace ecopol = 2 if year >= 1992
replace ecopol = 3 if year >= 1994
*/

* 17. Migration policy changes
gen migpol = 0
replace migpol = 1 if year >= 2004


***  we drop the years before 2000

keep if year >= 2000

* 18. Impute remittances for the missing 2001-2009
rename IIM_vivrem  rem
gen remVf = rem if year==2010
gsort - idedomun - year
replace remVf = remVf[_n-1] if remVf==. & idedomun[_n-1]==idedomun
sort idedomun year
gen remVi = rem if year==2000
replace remVi = remVi[_n-1] if remVi==. & idedomun[_n-1]==idedomun
gen Rrem = ((remVf/remVi)^.1) - 1
replace rem = rem[_n-1] * (1 + Rrem) if rem ==. & idedomun[_n-1]==idedomun
sum rem
* 18.2 logarithmic transformation of remmittances
gen lnrem = ln(rem)


* 19. Return migrants
rename IIM_viv_ret ret
gen retVf = ret if year==2010
gsort - idedomun - year
replace retVf = retVf[_n-1] if retVf==. & idedomun[_n-1]==idedomun
sort idedomun year
gen retVi = ret if year==2000
replace retVi = retVi[_n-1] if retVi==. & idedomun[_n-1]==idedomun
gen Rret = ((retVf/retVi)^.1) - 1
replace ret = ret[_n-1] * (1+ Rret) if ret ==. & idedomun[_n-1]==idedomun
sort year idedomun
*by year: sum ret  //ret is perhaps not a good variable, there are many missing variables
sum ret

* 21 Emigrants
rename IIM_viv_emig emig
gen emigVf = emig if year==2010
gsort - idedomun - year
replace emigVf = emigVf[_n-1] if emigVf==. & idedomun[_n-1]==idedomun
sort idedomun year
gen emigVi = emig if year==2000
replace emigVi = emigVi[_n-1] if emigVi==. & idedomun[_n-1]==idedomun
gen Remig = ((emigVf / emigVi)^.1) - 1
replace emig = emig[_n-1] * (1+Remig) if emig==. & idedomun[_n-1]==idedomun
sum emig
* 21.2 logarithmic transformation of emigrants
gen lnemig = ln(emig)


* 22 Now the complete migration index 
rename IIM_indice migindex
gen migindexVf = migindex if year==2010
gsort- idedomuny year
replace migindexVf = migindexVf[_n-1] if migindexVf==. & idedomun[_n-1]==idedomun
sort idedomuny year
gen migindexVi = migindex if year==2000
replace migindexVi = migindexVi[_n-1] if migindexVi==. & idedomun[_n-1]==idedomun
gen Rmigindex = ((migindexVf / migindexVi)^.1) - 1
replace migindex = migindex[_n-1] * (1+Rmigindex) if migindex==. & idedomun[_n-1]==idedomun


* 23. divorcios
gen divrate = (pob_divorcios / pob_total_est) * 100000

* 23.1 logaritmo natural de divorcios
gen lndivrate = ln(divrate)


* 20 what about sex ratio?
rename pob_relHM sexratio
order sexratio, a(hom_simbad)

* 20.1 now we impute the sex ratio data for the 2001 - 2004 period
gen sexratio2005 = sexratio if year == 2005
gsort - idedomun - year
replace sexratio2005 = sexratio2005[_n-1] if sexratio2005==. & idedomun[_n-1]==idedomun
sort idedomun year
gen sexratio2000 = sexratio if year==2000
replace sexratio2000 = sexratio2000[_n-1] if sexratio2000==. & idedomun[_n-1]==idedomun
gen Rsexratio = ((sexratio2005 / sexratio2000)^.2) - 1
replace sexratio = sexratio[_n-1] * (1 + Rsexratio) if sexratio==. & idedomun[_n+1]==idedomun & year >= 2000 & year <= 2005

* 20.2 now we impute the sex ratio data for the 2006-2009 period
gen sexratio2010 = sexratio if year == 2010
gsort - idedomun - year
replace sexratio2010 = sexratio2010[_n-1] if sexratio2010==. & idedomun[_n-1]==idedomun
sort idedomun year
replace sexratio2005 = sexratio2005[_n-1] if sexratio2005==. & idedomun[_n-1]==idedomun
gen R2sexratio = ((sexratio2010 / sexratio2005)^.2) - 1
replace sexratio = sexratio[_n-1] * (1 + R2sexratio) if sexratio==. & idedomun[_n+1]==idedomun & year >= 2005 & year <= 2010

* 21. percentage of young males
* rename young men var as "homjov", impute for missing years

rename pob_15a29HP homjov
gen homjovVf = homjov if year == 2010
gsort - idedomun - year
replace homjovVf = homjovVf[_n-1] if homjovVf==. & idedomun[_n-1]==idedomun
sort idedomun year
gen homjovVi = homjov if year==2000
replace homjovVi = homjovVi[_n-1] if homjovVi==. & idedomun[_n-1]==idedomun
gen Rhomjov = ((homjovVf / homjovVi)^.1) - 1
replace homjov = homjov[_n-1] * (1 + Rhomjov) if homjov==. & idedomun[_n+1]==idedomun

* 22. rename seg... as conviction rates "convrates"
rename seg_sentencia_total convrates


/* 23 Oportunidades
//gen oportunidades = deshum_oportfam / pob_vivienda_total

* 24.. Indigenous population
// simply use pob_ind, but also its natural logaritm
drop pob_indVf
gen pob_indVf = pob_ind if year == 2010
gsort - idedomun - year
replace pob_indVf = pob_indVf[_n-1] if pob_indVf==. & idedomun[_n-1]==idedomun
sort idedomun year
replace pob_indVf = pob_indVf[_n-1] if pob_indVf==. & idedomun[_n-1]==idedomun

//gen pob_indVint = pob_ind if year == 2005
//replace pob_indVint = pob_indVint[_n-1] if pob_indVint==. & idedomun[_n-1]==idedomun
//gsort- id
//replace pob_indVint = pob_indVint[_n-1] if pob_indVint==. & idedomun[_n-1]==idedomun //hay que hacerlo al derecho y al rev{es


gen pob_indVi = pob_ind if year == 2000
sort id
replace pob_indVi = pob_indVi[_n-1] if pob_indVi==. & idedomun[_n-1]==idedomun

gen Rpob_ind = ((pob_indVf / pob_indVi)^.1) - 1
replace pob_ind = pob_ind[_n-1] * (1 + Rpob_ind) if pob_ind==. & idedomun[_n-1]==idedomun

l NomMun year pob_ind in 1/200

gen lnpob_ind = ln(pob_ind)
*/

* 25. we generate regional dummy variables
egen border = anymatch(idedo), values(2 5 8 19 26 28)
egen norte = anymatch(idedo), values(2 5 8 10 19 24 25 26 28 32)
egen pacifico = anymatch(idedo), values(2 3 6 7 12 14 16 18 20 25 26)

*26 interaction % emig and rem
gen emigrem = emig*rem

* 27. create proportion of secondary schooling
gen secschool = 1 - edu15delay
sum secschool


* 28. generate quadratic and logarithmic terms for electoral competition

gen munENP2 = munENP^2
gen lnmunENP = ln(munENP)

** 29. gen quadratic and logarithmic terms for return migration

gen ret2 = ret^2
gen lnret = ln(ret)

* 30. generate interaction % education and remittances
gen remedu = rem * secschool
gen lnremedu = lnrem * secschool



* interaction % emigration and abstentionism
gen abst = 1 - part6
replace abst = abst[_n-1] if abst==. & idedomun[_n-1]==idedomun
gen socpolreject = abst*emig
sum socpolreject

**Mexican gdp growth rate
gen mexgdpgr = .
replace mexgdpgr = 5.3 if year == 2000
replace mexgdpgr = -0.61 if year == 2001
replace mexgdpgr = 0.13 if year == 2002
replace mexgdpgr = 1.42 if year == 2003
replace mexgdpgr = 4.3 if year == 2004
replace mexgdpgr = 3.03 if year == 2005
replace mexgdpgr = 5 if year == 2006
replace mexgdpgr = 3.15 if year == 2007
replace mexgdpgr = 1.4 if year == 2008
replace mexgdpgr = -4.7 if year == 2009
replace mexgdpgr = 5.11 if year == 2010

**U.S. gdp growth rate
gen usgdpgr = .
replace usgdpgr = 4.09 if year == 2000
replace usgdpgr = 0.98 if year == 2001
replace usgdpgr = 1.79 if year ==2002
replace usgdpgr = 2.81 if year == 2003
replace usgdpgr = 3.79 if year == 2004
replace usgdpgr = 3.35 if year == 2005
replace usgdpgr = 2.67 if year == 2006
replace usgdpgr = 1.77 if year == 2007
replace usgdpgr = 1.4 if year == 2008
replace usgdpgr =  -4.7 if year == 2009
replace usgdpgr = 2.53 if year == 2010

**detect the incumbent
gen mincumbent = .
replace mincumbent = 1 if PANtotal6 > PRItotal6 & PANtotal6 > PRDtotal6
replace mincumbent = 2 if PRItotal6 > PANtotal6 & PRItotal6 > PRDtotal6
replace mincumbent = 3 if PRDtotal6 > PANtotal6 & PRDtotal6 > PRItotal6
sort idedomuny
replace mincumbent = mincumbent[_n-1] if mincumbent==. & idedomun[_n-1]==idedomun



* II. order the variables that will be used
order hom_simbad laghom_simbad rem lnrem emig lnemig ret lnret remedu secschool sexratio homjov poverty ///
gini gini2 pibpc munENP lnmunENP ruralcorp stateconcur fedconcur munelect gubelect ///
fedelect ruralcorp divrate lndivrate lnpop convrates, a(year)


*
* Alcohol intoxications
cd "C:\Tesis\Masterdata\Egresos_hospitalarios"
import delimited "C:\Tesis\Masterdata\Egresos_hospitalarios\Alcohol_intoxications_municipio_residencia4.csv", clear

reshape long alcohol, i(idedomun) j(year)
keep idedomun year alcohol

merge 1:1 idedomun year using "C:\Tesis\Masterdata\Masterdata9.dta"
sort idedomun year
drop _merge

*
* circular migration

rename IIM_viv_circ circ
gen circVf = circ if year==2010
gsort - idedomun - year
replace circVf = circVf[_n-1] if circVf==. & idedomun[_n-1]==idedomun
sort idedomun year
gen circVi = circ if year==2000
replace circVi = circVi[_n-1] if circVi==. & idedomun[_n-1]==idedomun
gen Rcirc = ((circVf/circVi)^.1) - 1
replace circ = circ[_n-1] * (1+ Rcirc) if circ ==. & idedomun[_n-1]==idedomun
sort year idedomun
by year: sum circ  //circ may be not a good variable, there are many missing variables
sum circ
gen lncirc = ln(circ)


save C:\Tesis\Masterdata\Masterdata10.dta, replace
// findit clarify
*/

use C:\Tesis\Masterdata\Masterdata10.dta, clear
gen horate = hom_simbad/pob_total_est *100000
gen lnalcohol = ln(alcohol)
drop if year > 2010

* produce list of alcohol intoxications per year
//sort year
//by year: sum alcohol, detail


*descriptive statistics for many variables
// sum hom_simbad laghom_simbad rem lnrem emig lnemig ret lnret remedu secschool sexratio homjov  poverty ///
// gini gini2 pibpc munENP lnmunENP ruralcorp stateconcur fedconcur munelect gubelect ///
// fedelect ruralcorp divrate lndivrate lnpop convrates


	   
*************** Fixed effects / Random effects

* tell stata we have panel data
xtset idedomun year  // hasta acá todo va bien


* descriptive statistics for variables w.o. transformation
sum hom_simbad horate emig rem ret circ remedu munENP homjov secschool ///
 sexratio gini pibpc poverty ruralcorp divrate convrates border alcohol pop
tabstat hom_simbad emig rem ret remedu munENP homjov secschool ///
 sexratio gini pibpc poverty ruralcorp divrate convrates border alcohol pop, s(me ma mi sd med)

* descriptive statistics for variables with transformation
sum hom_simbad lnemig lnrem lnret lncirc lnremedu lnmunENP homjov secschool sexratio lngini lnpibpc poverty ruralcorp divrate convrates border lnalcohol lnpop

* Models new chapter 4

* Model 1: homicide counts Negative binomial regression
nbreg hom_simbad lnemig lnrem lnret lnremedu lncirc lnmunENP homjov secschool sexratio ///
 lngini lnpibpc poverty ruralcorp divrate convrates border lnalcohol lnpop
//outreg2 using "C:\Tesis\Chapter_4\tablas.rtf", ctitle(Coefficient) replace
//outreg2 using "C:\Tesis\Chapter_4\tablas.rtf", eform ctitle(IRR) append
// estimates store pooled

* Model 2 homicide counts fx negative binomial
xtnbreg hom_simbad lnemig lnrem lnret lnremedu lnmunENP homjov secschool sexratio ///
lngini lnpibpc poverty ruralcorp divrate convrates border lnalcohol lnpop, fe irr
vce, corr
//outreg2 using "C:\Tesis\Chapter_4\tablas.rtf", ctitle(Coefficient) replace
//outreg2 using "C:\Tesis\Chapter_4\tablas.rtf", eform ctitle(IRR) append
//estimates store fixed

/*  margins, at(lnemig=(0.1(1)3.8)) atmeans vsquish
marginsplot, title("Predicted Margins with 95% Confidence Intervals") ytitle("Predicted Number of Homicides") ///
 xtitle("Percent of Households with Emigrants") xlabel( 0.1 "1" 1.1 "3" 2.1 "8" 3.1 "22") ///
   recast(line) recastci(rarea) scheme(s1mono) 
*/

* Model 3 homicide rates nb 
gen horate = hom_simbad/pob_total_est *100000
tabstat horate, s(me ma mi sd med)
nbreg horate lnemig lnrem lnret lnremedu lnmunENP homjov secschool sexratio ///
lngini lnpibpc poverty ruralcorp divrate convrates border lnalcohol lnpop
// outreg2 using "C:\Tesis\Chapter_4\tablas.rtf", ctitle(Coefficient) replace
// outreg2 using "C:\Tesis\Chapter_4\tablas.rtf", eform ctitle(IRR) append

estsimp nbreg horate lnemig lnrem lnret lnremedu lnmunENP homjov secschool sexratio ///
lngini lnpibpc poverty ruralcorp divrate convrates border lnalcohol lnpop

setx lnemig median lnrem median lnret median lnremedu median lnmunENP median homjov median secschool median ///
sexratio median lngini median lnpibpc median poverty median ruralcorp median divrate median ///
 convrates median lnpop median
 

simqi, ev listx
simqi, fd(ev) changex(lngini -.84 -.7)

* margins outmigration
 margins, at(lnemig=(0.1(1)3.8)) atmeans vsquish
marginsplot, title("Predicted Margins with 95% Confidence Intervals") ytitle("Predicted Homicide Rate") ///
 xtitle("Percent of Households with Emigrants") xlabel( 0.1 "1" 1.1 "3" 2.1 "8" 3.1 "22") ///
   recast(line) recastci(rarea) scheme(s1mono)

   margins, at(lnrem=(0.1(1)3.8)) atmeans vsquish
marginsplot, title("Predicted Margins with 95% Confidence Intervals") ytitle("Predicted Homicide Rate") ///
 xtitle("Percent of Households that receive remittances") xlabel( 0.1 "1" 1.1 "3" 2.1 "8" 3.1 "22") ///
   recast(line) recastci(rarea) scheme(s1mono)

   
 * Model 4 horate nb fx
xtnbreg horate lnemig lnrem lnret lnremedu lnmunENP homjov secschool sexratio ///
lngini lnpibpc poverty ruralcorp divrate convrates lnalcohol border lnpop, fe irr
 outreg2 using "C:\Tesis\Chapter_4\tablas.rtf", ctitle(Coefficient) replace
outreg2 using "C:\Tesis\Chapter_4\tablas.rtf", eform ctitle(IRR) append

 margins, at(lnemig=(0.1(1)3.8)) atmeans vsquish
marginsplot, title("Predicted Margins with 95% Confidence Intervals") ytitle("Predicted Homicide Rate") ///
 xtitle("Percent of Households with Emigrants") xlabel( 0.1 "1" 1.1 "3" 2.1 "8" 3.1 "22") ///
   recast(line) recastci(rarea) scheme(s1mono) 


/* clarify horates
 ESTSIMP estimates a chosen model and generates random
 draws from the multivariate normal distribution (i.e., com-
 putes 7).
 SETX sets Xc to desired values such as means, medians, per-
 centiles, minima, maxima, specified values, and others.
 SIMQI computes desired quantities of interest such as pre-
 dicted values, expected values, and first differences.

 
  estsimp oprobit y xl x2
 setx xl 12.8 x2 mean
 simqi, prval(4) level(90)
 
  fd(ev) changex(x1 .2 .8) -> instructs simqi to simulate a change in the ///
  expected value of Y caused by increasing x1 from its starting value, 0.2, to its
   ending value, to 0.8.


 */
 estsimp nbreg horate lnemig lnrem lnret lnremedu lnmunENP homjov secschool sexratio ///
lngini lnpibpc poverty ruralcorp divrate convrates border lnpop

setx lnemig median lnrem median lnret median lnremedu median lnmunENP median homjov median secschool median ///
sexratio median gini median lnpibpc median poverty median ruralcorp median divrate median ///
 convrates median lnpop median
 
simqi, ev listx
simqi, fd(ev) changex(lnmunENP .87 1.1)
di 1.86 * .85
di ln(2.4)
di exp(.875)
di exp(.04)

 plotfds, continuous(lnemig lnrem lnret lnremedu lnmunENP homjov secschool sexratio ///
lngini lnpibpc poverty ruralcorp divrate convrates lnpop) disc(border) scheme(s1mono) 

 [discrete(varlist)] [sortorder(varlist)
            outcome(#) clevel(#) nosetx changexcont(value1 value2) label savedata(string) 
            twoway_options]
 

/* Grinter
mlogit vote income educ educ_inc
    . grinter income, inter(educ_inc) const02(educ) equation(3) yline(0) nomeantext
        cioptions(lcolor(blue) lpattern(solid))
*/
xtnbreg horate lnemig lnrem lnret lnremedu lnmunENP homjov secschool sexratio ///
lngini lnpibpc poverty ruralcorp divrate convrates border lnpop, fe irr
predict x
grinter lnrem, inter(lnremedu) const02(secschool) depvar(horate) yline(0) nomeantext
        
		
		cioptions(lcolor(black) lpattern(solid))
 
 
 

/* likelihood ratio test pooled vs. fixed
test fixed
, stats*/

* hausman test
//hausman fixed random  // chi2 625, reject the null hypothesis that both models  produce consistent estimators

* correlation matrix
//vce, corr


  
   
  -3.9 "0" -2.9 "0.05" -1.9 "0.14" -0.9 "0.5" 
   
   margins, at(lnmunENP=(0(.24)1.38)) atmeans vsquish
marginsplot, title("Predicted Margins with 95% Confidence Intervals") ytitle("Predicted Number of Homicides") ///
 xtitle("Effective number of parties") ///
   recast(line) recastci(rarea) scheme(s1mono)
   
   di exp(0) // 1
   di exp(.24) // 1.27
   di exp(.48) // 1.61
   di exp(.72) // 2.05
   di exp(.96) // 2.61
   di exp(1.2) // 3.32
  
   /// I need to translate the logarithm 
   // into the normal migration scale to facilitate interpretation


//collapse lnemig, by(emig)
//order emig, a(lnemig)
di -4.03 + 1.164  //-2.866
di exp(-3.9) // 0.02
di exp(-2.9) // .05
di exp(-1.9) //.14
di exp(-0.9) //0.4 
di exp(0.1) // 1
di exp(1.1) // 3
di exp(2.1) // 8
di exp(3.1) // 22

di log(1)
di log(5)
di log(10)
di log(15)
di log(20)


tab lnemig emig
twoway connected lnemig emig  // sirve para sacar la escala correcta de la probabilidad marginal
gsort- b1
twoway connected




