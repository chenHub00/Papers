rm(list = ls())

# set working directory
getwd()
#setwd("~/work")

################## 1. instalar y cargar paquetes ###########

# install.packages("stargazer"): #hace tablas de regresion y descriptivas
# install.packages("foreign"): #reads non-R files
# install.packages("MASS"): #functions and econometric models
# install.packages("dummies") #creates dummies
# install.packages("dummy")
# install.packages("fBasics")

# adapt my Rprofile.site to contain the following line,
# making my default library path a directory not included in my R installation:
#.libPaths(c("/usr/lib/R/library",.libPaths()))

# Other library path: ‘/home/sandra/R/x86_64-pc-linux-gnu-library/3.4’:

# install.packages("survival") #survival analysis
# install.packages("car") # companion to applied regression
# hay un problema con el car, dice :"Error, failed to lock directory"
# para solucionarlo voy a tratar
# install.packages("car", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
# install.packages("devtools")
# install.packages("Hmisc")
# install.packages("pastecs")
# install.packages("ineq")
# install.packages("tibble")
# install.packages("data.table")
# install.packages("readstata13")
# install.packages("dummies")
# install.packages("fBasics")

#check library list
# library()

# poner la base en el directorio de trabajo: "~/MEGA/work"

# cargar paquetes 
library(foreign)
library(stargazer)
library(MASS)
library(survival)
library(car)
library(devtools)
library(lattice)
library(Hmisc)
library(pastecs)
library(ineq)
library(plyr)
library(tibble)
library(data.table)
library(readstata13)
library(dummies)
library(data.table)
library(fBasics)

#cargar la base 
# d <- read.csv("BaseEVES.csv", stringsAsFactors = FALSE, fileEncoding="latin1")  ## latin1 sirve para leer los acentos

# corregir municipios en Chiapas
# d$idmun <- recode(d$idmun, "7121=121")
# d$idmun <- recode(d$idmun, "7120=120")


########## 2. Poverty
############### 2.1 Prepare food poverty variable in the dataset for year 2000 ##################

pobalimentaria2000 <- read.csv("pobalimentaria2000.csv", sep = ";",
                               stringsAsFactors = FALSE, fileEncoding="latin1")  ## latin1 sirve para leer los acentos
pobalimentaria2000 <- rename(pobalimentaria2000, replace = 
                               c("pobalimentaria2000" = "pobalimentaria"))
pobalimentaria2000 <- rename(pobalimentaria2000, replace = 
                               c("Clave.del.municipio" = "idedomun")) # cambiar el nombre del identificador
#  para que tengan el mismo nombre (rename clave mun.).
# str(pobalimentaria2000)     # < - trata a pobreza alimentaria como una cadena !!! hay que cambiarlo
pobalimentaria2000$pobalimentaria <- as.numeric(pobalimentaria2000$pobalimentaria) 
#str(pobalimentaria2000)  # Problema resuelto!!
pobalimentaria2000$year <- 2000 # aniadimos el anio para el que vamos a juntar la base
pobalimentaria2000 <- subset(pobalimentaria2000, select = -c(Municipio) )  # tiramos la variable municipio

################## 2.2 Prepare food poverty for the year 2010 ################
 
pobalimentaria2010 <- read.csv("pobalimentaria2010.csv", sep = ";",
                               stringsAsFactors = FALSE, fileEncoding="latin1")
pobalimentaria2010 <- rename(pobalimentaria2010, replace =
                               c("Clave.del.municipio" = "idedomun"))
pobalimentaria2010<- subset(pobalimentaria2010, select = -c(Municipio)) # tiramos Municipio
# str(pobalimentaria2010)     # < - pobreza alimentaria ya es numerica !!! no hay que convertirla.
# names(pobalimentaria2010)     # ademas, no hay que quitarle el anio al nombre de la variable.
# head(pobalimentaria2010)

# nomas hay que aniadir el anio para el que vamos a juntar la base
pobalimentaria2010$year <- 2010

# ahora hay que usar append (rbind) en lugar de merge pq pobalimentaria ya existe en nuestro archivo
pobalimentaria <- rbind(pobalimentaria2000, pobalimentaria2010)
# as_tibble(pobalimentaria)
# tail(pobalimentaria)

############# 2.3 Ahora la juntamos con la base EVES #################
#cargar la base OTRAVEZ?
d <- read.csv("BaseEVES.csv", stringsAsFactors = FALSE, fileEncoding="latin1")  ## latin1 sirve para leer los acentos

# leerla como un "Tibble"
d <- as_tibble(d)
d <- merge(d, pobalimentaria, by=c("idedomun","year"), all=TRUE)


# graph the year difference

# dosmil <- subset(d, year == 2000, select=c(idedomun, year, pobalimentaria)) 
# dosdiez <- subset(d, year == 2010, select=c(idedomun, year, pobalimentaria)) 
# par(pty="s")   #plot perfectly square graphs.
# tapply(d$pobalimentaria, d$year, summary) # muy buena esta tapply.
# plot(dosmil$pobalimentaria, dosdiez$pobalimentaria, pty = "s")
# boxplot(dosmil$pobalimentaria, dosdiez$pobalimentaria)
# rm(dosdiez, dosmil)

############ 3. Literacy ################
# 3.1 year 2000

illit2000 <- read.csv("analfabeta2000.csv", sep = ";",
                               stringsAsFactors = FALSE, fileEncoding="latin1")  ## latin1 sirve para leer los acentos
str(illit2000) # hay que crear una proporcion de gente que sabe leer y escribir.

illit2000$illit <- illit2000$No.sabe.leer.y.escribir / illit2000$Total # aqui esta la proporcion

# summary(illit2000$illit)
# head(illit2000$illit)
# as_tibble(illit2000)
stat.desc(illit2000)

illit2000 <- rename(illit2000, replace = 
                      c("Clave" = "idedomun")) # cambiar el nombre del identificador

illit2000$year <- 2000 # aniadimos el anio para el que vamos a juntar la base
illit2000 <- subset(illit2000, select = -c(Nombre, Total, 
                                           Sabe.leer.y.escribir, No.sabe.leer.y.escribir) )  # tiramos varias variables

################## 3.2 illiteracy year 2010 ################
illit2010 <- read.csv("analfabeta2010.csv", sep = ";",
                               stringsAsFactors = FALSE, fileEncoding="latin1")
str(illit2010)
illit2010 <- rename(illit2010, replace =
                      c("Clave" = "idedomun"))
illit2010$illit <- illit2010$No.sabe.leer.y.escribir / illit2010$Total
illit2010<- subset(illit2010, select = c(idedomun, illit)) # tiramos vars
illit2010$year <- 2010

######## 3.3 ahora de nuevo hay que usar append (rbind) y merge
illit <- rbind(illit2000, illit2010)

d <- merge(d, illit, by=c("idedomun","year"), all=TRUE)

# par(pty="s")   #plot perfectly square graphs.
# tapply(d$pobalimentaria, d$year, summary) # muy buena esta tapply.
#  plot(d$pobalimentaria, d$illit, pty = "s")


####### 4 now we create our poverty variable: poverty * illiteracy ####
##### and impute the data for the years lacking

d$poverty <- d$illit * d$pobalimentaria 

stat.desc(d$poverty)
summary(d$poverty)

##############  5  we include population older than 15 without secondary education for the year 2000 #################

wosec2000 <- read.csv("pobbasiced2000.csv", sep = ";",
                      stringsAsFactors = FALSE, fileEncoding="latin1")
summary(wosec2000)

wosec2000$wosec <- as.numeric(wosec2000$ratebasiced)
wosec2000 <- subset(wosec2000, select = -c(Nombre, ratebasiced))
wosec2000 <- rename(wosec2000, replace = c("Clave" = "idedomun") )
wosec2000$year <- 2000

wosec2010 <- read.csv("pobbasiced2010.csv", sep = ";",
                        stringsAsFactors = FALSE, fileEncoding = "latin1")
wosec2010 <- rename(wosec2010, replace = c("Clave" = "idedomun"))
wosec2010 <- rename(wosec2010, replace = c("edu15delay" = "wosec"))
wosec2010$wosec <- as.numeric(wosec2010$wosec)
wosec2010$year <- 2010
wosec <- rbind(wosec2000, wosec2010)
d <- merge(d, wosec, by=c("idedomun","year"), all=TRUE)
summary(d$wosec)

#############  6 Gini coefficient for the year 2000 ############

gini2005 <- d$IDH_gini
summary(gini2005)
gini2000 <- read.csv("gini2000.csv", sep = ";",
                  stringsAsFactors = FALSE, fileEncoding = "latin1")
gini2000$gini <- as.numeric(gini2000$gini)
gini2000$year <- 2000
summary(gini2000)

## for the year 2010

gini2010 <- read.csv("gini2010.csv", sep = ";",
                     stringsAsFactors = FALSE, fileEncoding = "latin1")
gini2010$year <- 2010
gini <- rbind(gini2000, gini2010)

d <- merge(d, gini, c("idedomun", "year"), all = TRUE)

############# 7 PIB per capita 2010 ##########

IDH_ingpc <- read.csv("IDH_ingpc2010.csv", sep = ";",
                      stringsAsFactors = FALSE, fileEncoding = "latin1")
IDH_ingpc <- subset(IDH_ingpc, select = c("idedomun", "IDH_ingpc"), drop = TRUE)
IDH_ingpc$year <- 2010
d$idedomuny <- paste0(d$idedomun, d$year)  # creamos un solo identificador
IDH_ingpc$idedomuny <- paste0(IDH_ingpc$idedomun, IDH_ingpc$year)

d <- data.frame(d, stringsAsFactors=FALSE) # declaramos que es un data frame
IDH_ingpc <- data.frame(IDH_ingpc, stringsAsFactors=FALSE)
swap <- IDH_ingpc$IDH_ingpc[match(d$idedomuny, IDH_ingpc$idedomuny)] # usamos match
ok <- !is.na(swap)  # creamos un ok
d$IDH_ingpc[ok] <- swap[ok]

# head(d$IDH_ingpc, n= 35)
# summary(d$IDH_ingpc, n= 35)

# create logarithmic expression of per capita GDP

d$loggdp <- log(d$IDH_ingpc)
stat.desc(d[,c("IDH_ingpc","loggdp")])
hist(d$IDH_ingpc, breaks = "FD", col = "green" )
hist(d$loggdp, breaks = "FD", col = "green")

#### 8 Foreign Direct Investment ####
ied <- read.dta13("IED.dta", convert.factors = TRUE)

as_tibble(ied)
ied <- data.frame(ied, stringsAsFactors=FALSE)
 d <- merge(d, ied, c("idedo","year"), all= TRUE)
head(d$IED)
d$ied <- d$IED
some(d[,c("idedo","year","ied")])

# explore panel data
#coplot(IED ~ year|idedo, type="l", data=d)

#### 9 Municipal, gubernatorial and federal elections dummies ####
d$munelect <- ifelse(is.na(d$vtotalr6), 0, 1)
d$gubelect <- ifelse(is.na(d$vtotalr7), 0, 1)
d$fedelect <- ifelse(is.na(d$vtotalr1), 0, 1)

# concurrence municipal + federal
d$munfed <- d$munelect + d$fedelect
d$fedconcur <- ifelse(d$munfed == 2, 1, 0)

# concurrence municipal + governor
d$ms <- d$munelect + d$gubelect
d$mungubconcur <- ifelse(d$ms == 2, 1, 0)

# concurrence municipal + governor + federal
d$mgf <- d$munelect + d$gubelect + d$fedelect
d$mungubfedconcur <- ifelse(d$mgf==3,1,0)

#### 10 Municipal Taagepera ####

d$munPANsq <- (d$PAN_p6)^2
d$munPRIsq <- (d$PRI_p6)^2
d$munPRDsq <- (d$PRD_p6)^2
d$munOtrossq <- (d$Otros_p6)^2

d$munENP <- 1 / (d$munPANsq + d$munPRIsq + d$munPRDsq + d$munOtrossq)
summary(d$munENP)

#### 11 gubernatorial Taagepera ####

d$gubPANsq <- (d$PAN_p7)^2
d$gubPRIsq <- (d$PRI_p7)^2
d$gubPRDsq <- (d$PRD_p7)^2
d$gubOtrossq = (d$Otros_p7)^2

d$gubENP = 1 / (d$gubPANsq + d$gubPRIsq + d$gubPRDsq + d$gubOtrossq)

#### 11.4 we generate the interaction between municipal and state electoral competition ####

d$localcomp <- d$munENP*d$gubENP

# we elevate localcomp to the 2
d$localcomp2 <- (d$localcomp)^2
summary(d$localcomp)

#### 12 add homicidios SIMBAD ####
violinegi <- read.dta13("homicidios_simbad.dta", convert.factors = TRUE)
violinegi <- subset(violinegi, select =  -c(NomMun))
head(violinegi)
stat.desc(violinegi)
# 
d <- merge(d, violinegi, by=c("idedomun","year"), all=TRUE)

# create homicide rate SIMBAD and change NAs in homicide variable to 0
d$horatesimbad <- (d$h*100000) / d$pob_total_est

d$horatesimbad <- ifelse(is.na(d$horatesimbad), 0, d$horatesimbad)
summary(d$horatesimbad)

# revisar los municipios con 999 y 930, y los distritos de Oaxaca >= 570, tirarlos
d <- subset(d, idmun <= 570)
summary(d$idedomun)
summary(d$horatesimbad)
as_tibble(d$idedomun, d$NomMun)

head(d[,c("idedo","idedomun","NomMun","year", "h", "horatesimbad")], n = 30)
# histogram homicide rates for year 2010
y2010 <- d[ which(d$year=='2010'), ]
y2010 <- subset(y2010, y2010$horatesimbad < 100)
hist(y2010$horatesimbad, breaks = 'FD', col='green')

# create logarithmic expression of homicide rates (year 2010)


#### 12 Lagged homicides ####

d$lag.hom <- c(NA, d$horatesimbad[-nrow(d)])
d$lag.hom[which(!duplicated(d$idedomun))] <- NA

head(d[,c("idedo","idedomun","NomMun","year", "h", "horatesimbad","lag.hom")], n = 40)

#### 13 Non-zero homicide variable: this is to have a dycothomic variable to make the Logit model (Trejo's model 2) ####

d$nonzero = 0
d$nonzero <- ifelse(d$homicidios > 0, 1, 0)
summary(d$nonzero)

#### 14. Rural corporatism ####
d$ruralcorp = d$PRI_p6 * d$part6
summary(d[,c("ruralcorp", "PRI_p6", "part6")])

##### 15. Population, we generate the natural logarythm but als the quadratic term #####
d$logpop = log(d$pob_total_est)
d$pop2 = d$pob_total_est^2
d$pop = d$pob_total_est
summary(d[, c("logpop", "pop2","pop")])

#### 16. Economic Crisis Dummy Variable ##### 
d$crisis = 0
d$crisis = ifelse(d$year == 1982, 1, 0)
d$crisis = ifelse(d$year == 1985, 1, 0)
d$crisis = ifelse(d$year == 1986, 1, 0)
d$crisis = ifelse(d$year == 1995, 1, 0)
d$crisis = ifelse(d$year == 2000, 1, 0)
d$crisis = ifelse(d$year == 2001, 1, 0)
d$crisis = ifelse(d$year == 2002, 1, 0)
d$crisis = ifelse(d$year == 2003, 1, 0)
d$crisis = ifelse(d$year == 2008, 1, 0)
d$crisis = ifelse(d$year == 2009, 1, 0)
d$crisis = ifelse(d$year == 2010, 1, 0)

#### 17. Migration policy changes ####
d$migpol = 0
# gen migpol = 0
#replace migpol = 1 if year >= 2004
d$migpol = ifelse(d$year >= 2004, 1, 0) 

#***  we drop the years before 2000
d <- subset(d, d$year >= 2000)
#keep if year >= 2000


#### 18. Impute remittances for the missing 2001-2009 ####
# d$remVf = ifelse(d$year ==2010,d$rem,NA)
# gen remVf = rem if year==2010
# rename remittances and transform the variable with log
d$rem <- d$IIM_vivrem
d$logrem <- log(d$IIM_vivrem)


#### 19. Return migrants ####
# rename return migration and transform the variable with log
d$ret <- d$IIM_viv_ret
d$logret <- log(d$IIM_viv_ret)

#### 21 Emigrants ####
# rename emigration and transform the variable with log
d$emig <- d$IIM_viv_emig
d$logemig <- log(d$IIM_viv_emig)

#### 22 Now the complete migration index ####
d <- rename(d, replace = c("IIM_indice"="migindex"))
# rename IIM_indice migindex
d$logmigindex <- log(d$migindex)

# rename circular migration and transform the variable with log
d$circ <- d$IIM_viv_circ
d$logcirc <- log(d$IIM_viv_circ)

#### 23. divorcios ####
# divorce rate y transformacion logaritmica de esta variable
d$divrate = (d$pob_divorcios/d$pob_total_est)*100000
#gen divrate = (pob_divorcios / pob_total_est) * 100000

# promedios de medias de homicidios para dos periodos 1996-2000 y 2006-2010.
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=1)}

#setkey()
# d$mo.hr <- aggregate(d$horatesimbad, by=list(d$idedomun), FUN=mav)

head(d[,c("idedo","idedomun","NomMun","year", "h", "horatesimbad","mo.hr")], n = 40)
# aggregate(x$Frequency, by=list(Category=x$Category), FUN=sum)

#### 23.1 logaritmo natural de divorcios ####
d$lndivrate = log(d$divrate)
#gen lndivrate = ln(divrate)

# * 20 what about sex ratio?
# rename pob_relHM sexratio
# order sexratio, a(hom_simbad)
# 
# * 20.1 now we impute the sex ratio data for the 2001 - 2004 period
# gen sexratio2005 = sexratio if year == 2005
# gsort - idedomun - year
# replace sexratio2005 = sexratio2005[_n-1] if sexratio2005==. & idedomun[_n-1]==idedomun
# sort idedomun year
# gen sexratio2000 = sexratio if year==2000
# replace sexratio2000 = sexratio2000[_n-1] if sexratio2000==. & idedomun[_n-1]==idedomun
# gen Rsexratio = ((sexratio2005 / sexratio2000)^.2) - 1
# replace sexratio = sexratio[_n-1] * (1 + Rsexratio) if sexratio==. & idedomun[_n+1]==idedomun & year >= 2000 & year <= 2005
# 
# * 20.2 now we impute the sex ratio data for the 2006-2009 period
# gen sexratio2010 = sexratio if year == 2010
# gsort - idedomun - year
# replace sexratio2010 = sexratio2010[_n-1] if sexratio2010==. & idedomun[_n-1]==idedomun
# sort idedomun year
# replace sexratio2005 = sexratio2005[_n-1] if sexratio2005==. & idedomun[_n-1]==idedomun
# gen R2sexratio = ((sexratio2010 / sexratio2005)^.2) - 1
# replace sexratio = sexratio[_n-1] * (1 + R2sexratio) if sexratio==. & idedomun[_n+1]==idedomun & year >= 2005 & year <= 2010
# 
# * 21. percentage of young males
# * rename young men var as "homjov", impute for missing years
# 
# rename pob_15a29HP homjov
# gen homjovVf = homjov if year == 2010
# gsort - idedomun - year
# replace homjovVf = homjovVf[_n-1] if homjovVf==. & idedomun[_n-1]==idedomun
# sort idedomun year
# gen homjovVi = homjov if year==2000
# replace homjovVi = homjovVi[_n-1] if homjovVi==. & idedomun[_n-1]==idedomun
# gen Rhomjov = ((homjovVf / homjovVi)^.1) - 1
# replace homjov = homjov[_n-1] * (1 + Rhomjov) if homjov==. & idedomun[_n+1]==idedomun
# 
# * 22. rename seg... as conviction rates "convrates"
# rename seg_sentencia_total convrates
# 
# 
# /* 23 Oportunidades
# //gen oportunidades = deshum_oportfam / pob_vivienda_total
# 

#### 24.. Indigenous population ####
# // simply use pob_ind, but also its natural logaritm
d$lnpob_ind = log(d$pob_ind)

#### 25. we generate regional dummy variables ####
#   egen border = anymatch(idedo), values(2 5 8 19 26 28)
#   egen norte = anymatch(idedo), values(2 5 8 10 19 24 25 26 28 32)
#   egen pacifico = anymatch(idedo), values(2 3 6 7 12 14 16 18 20 25 26)
#   
####  26 interaction % emig and rem ####
#   gen emigrem = emig*rem
  
#### 27. create proportion of secondary schooling ####
#   gen secschool = 1 - edu15delay
#   sum secschool

#### 28. generate quadratic and logarithmic terms for electoral competition ####
#   
#   gen munENP2 = munENP^2
#   gen lnmunENP = ln(munENP)
#   
#### 29. gen quadratic and logarithmic terms for return migration ####
d$ret2 = d$ret^2
d$lnret = log(d$ret)
# gen ret2 = ret^2
# gen lnret = ln(ret)
#   
#   * 30. generate interaction % education and remittances
#   gen remedu = rem * secschool
#   gen lnremedu = lnrem * secschool
#   
#   
#   
#### interaction % emigration and abstentionism ####
d$abst = 1 - d$part6
#   gen abst = 1 - part6
#   replace abst = abst[_n-1] if abst==. & idedomun[_n-1]==idedomun
#   gen socpolreject = abst*emig
#   sum socpolreject
#   
#### Mexican gdp growth rate ####
#   gen mexgdpgr = .
#   replace mexgdpgr = 5.3 if year == 2000
#   replace mexgdpgr = -0.61 if year == 2001
#   replace mexgdpgr = 0.13 if year == 2002
#   replace mexgdpgr = 1.42 if year == 2003
#   replace mexgdpgr = 4.3 if year == 2004
#   replace mexgdpgr = 3.03 if year == 2005
#   replace mexgdpgr = 5 if year == 2006
#   replace mexgdpgr = 3.15 if year == 2007
#   replace mexgdpgr = 1.4 if year == 2008
#   replace mexgdpgr = -4.7 if year == 2009
#   replace mexgdpgr = 5.11 if year == 2010
#   
#### **U.S. gdp growth rate ####
#   gen usgdpgr = .
#   replace usgdpgr = 4.09 if year == 2000
#   replace usgdpgr = 0.98 if year == 2001
#   replace usgdpgr = 1.79 if year ==2002
#   replace usgdpgr = 2.81 if year == 2003
#   replace usgdpgr = 3.79 if year == 2004
#   replace usgdpgr = 3.35 if year == 2005
#   replace usgdpgr = 2.67 if year == 2006
#   replace usgdpgr = 1.77 if year == 2007
#   replace usgdpgr = 1.4 if year == 2008
#   replace usgdpgr =  -4.7 if year == 2009
#   replace usgdpgr = 2.53 if year == 2010
#   
#### detect the incumbent ####
#   gen mincumbent = .
#   replace mincumbent = 1 if PANtotal6 > PRItotal6 & PANtotal6 > PRDtotal6
#   replace mincumbent = 2 if PRItotal6 > PANtotal6 & PRItotal6 > PRDtotal6
#   replace mincumbent = 3 if PRDtotal6 > PANtotal6 & PRDtotal6 > PRItotal6
#   sort idedomuny
#   replace mincumbent = mincumbent[_n-1] if mincumbent==. & idedomun[_n-1]==idedomun
#   
#   
#   
#### II. order the variables that will be used ####
#   order hom_simbad laghom_simbad rem lnrem emig lnemig ret lnret remedu secschool sexratio homjov poverty ///
#     gini gini2 pibpc munENP lnmunENP ruralcorp stateconcur fedconcur munelect gubelect ///
#     fedelect ruralcorp divrate lndivrate lnpop convrates, a(year)
#   
## Alcohol intoxication

save(d, file = 'Dframe.Rdata')

##### II. Modelos ############
# descriptive statistics
ds <- subset(d, select = c("horatesimbad","poverty", "wosec", "gini", "IDH_ingpc", "loggdp", "ied", "divrate", "ruralcorp", 
                           "munENP", "rem", "ret" ) )
stargazer(ds, type = "text", title="Descriptive statistics", digits=2, out="table1.txt")


#Model 1
m1 <- glm.nb(d$horatesimbad ~ d$ret + d$poverty + d$wosec + d$gini + d$IDH_ingpc  + d$loggdp + d$ied + d$divrate + d$ruralcorp + factor(d$year), data = d)
summary(m1)
