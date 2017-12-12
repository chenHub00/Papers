rm(list = ls())

# set working directory
setwd("~/MEGA/work")

################## 1 ###########
#instalar y cargar paquetes

# install.packages("stargazer"): #hace tablas de regresion y descriptivas
# install.packages("foreign"): #reads non-R files
# install.packages("MASS"): #functions and econometric models

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

########## 22. Poverty
############### 2.1 ################## 
# Prepare food poverty variable in the dataset for year 2000

pobalimentaria2000 <- read.csv("~/MEGA/Tesis/BaseEVES/pobalimentaria2000.csv", sep = ";",
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

################## 2.2 ################
# Prepare food poverty for the year 2010

pobalimentaria2010 <- read.csv("~/MEGA/Tesis/BaseEVES/pobalimentaria2010.csv", sep = ";",
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


############# 2.3 #################
# Ahora la juntamos con la base EVES
#cargar la base 
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

illit2000 <- read.csv("~/MEGA/Tesis/BaseEVES/analfabeta2000.csv", sep = ";",
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

################## 3.2 ################
# illiteracy year 2010

illit2010 <- read.csv("~/MEGA/Tesis/BaseEVES/analfabeta2010.csv", sep = ";",
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


####### 4 now we create our poverty variable: poverty * illiteracy
##### and impute the data for the years lacking

d$poverty <- d$illit * d$pobalimentaria 

stat.desc(d$poverty)
summary(d$poverty)

##############  5  #################
# we include population older than 15 without secondary education for the year 2000

wosec2000 <- read.csv("~/MEGA/Tesis/BaseEVES/pobbasiced2000.csv", sep = ";",
                      stringsAsFactors = FALSE, fileEncoding="latin1")
 summary(wosec2000)

wosec2000$wosec <- as.numeric(wosec2000$ratebasiced)
wosec2000 <- subset(wosec2000, select = -c(Nombre, ratebasiced))
wosec2000 <- rename(wosec2000, replace = c("Clave" = "idedomun") )
wosec2000$year <- 2000

wosec2010 <- read.csv("~/MEGA/Tesis/BaseEVES/pobbasiced2010.csv", sep = ";",
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
gini2000 <- read.csv("~/MEGA/Tesis/BaseEVES/gini2000.csv", sep = ";",
                  stringsAsFactors = FALSE, fileEncoding = "latin1")
gini2000$gini <- as.numeric(cgini2000$gini)
gini2000$year <- 2000
summary(gini2000)

## for the year 2010

gini2010 <- read.csv("~/MEGA/Tesis/BaseEVES/gini2010.csv", sep = ";",
                     stringsAsFactors = FALSE, fileEncoding = "latin1")
gini2010$year <- 2010
gini <- rbind(gini2000, gini2010)

d <- merge(d, gini, c("idedomun", "year"), all = TRUE)

############# 7 PIB per capita 2010

IDH_ingpc <- read.csv("~/MEGA/Tesis/BaseEVES/IDH_ingpc2010.csv", sep = ";",
                      stringsAsFactors = FALSE, fileEncoding = "latin1")
IDH_ingpc <- subset(IDH_ingpc, select = c("idedomun", "IDH_ingpc"), drop = TRUE)
IDH_ingpc$year <- 2010
summary(IDH_ingpc)

e <- subset(d,  select = c("idedomun", "year", "IDH_ingpc"))
as_tibble(e)
head(e, n=35)

# merge 1:1 idedomun year using C:\Tesis\Masterdata\Masterdata8.dta # Stata solution

e <- merge(e, IDH_ingpc, c("idedomun","year"), all = FALSE)




           
           by = intersect(names(idedomun), names(year))
e <- merge(e, IDH_ingpc, by = intersect(names(idedomun), names(year)),
      by.x = NULL, by.y = NULL, all = FALSE)

head(e, n=35)
, 
auxind<-match(IDH_ingpc$IDH_ingpc, e$IDH_ingpc)  # Stores the repeated rows in df1
dfuni<-(rbind(e[,"IDH_ingpc"],e)[-auxind,])  # Merges both data.frames and erases the repeated rows from the first three colums of df1


e <- merge(e, IDH_ingpc, c("idedomun", "year")
           
           
           
           
           , by.x = NULL, by.y = NULL)
,
      all = TRUE, all.x = all, all.y = all, sort = TRUE)



e <- merge(e, IDH_ingpc, c("idedomun", "year"), all = TRUE)


head(e, n = 35)
as_tibble(e)


merge(x, y, by = intersect(names(x), names(y)),
      by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
      sort = TRUE, suffixes = c(".x",".y"),
      incomparables = NULL, ...)
head(d)
summary(d)

d <- cbind(d, wosec, by=c("idedomun","year"), all=TRUE)
d <- merge(d, IDH_ingpc2010, all = TRUE)
as_tibble(d)
summary(d$IDH_ing) 
(d$idedomun, d$year, d$IDH_ingpc, n=35)

zz <- merge(df1, df2, all = TRUE)
e <- subset(d,  select = c("idedomun", "year", "IDH_ingpc"))
head(e, n = 35)
head(IDH_ingpc2010)

 e[match(IDH_ingpc2010$idedomun, IDH_ingpc2010$year,e$idedomun,e$year), ] <- e


geoIncendios[match(outliers$id, geoIncendios$id), ] <- outliers

e <- subset(e, select = )
wosec2000 <- subset(wosec2000, select = -c(Nombre, ratebasiced))
e <- subset(e, )
summary(e)
some(e)
head(e, n = 35)

mapvalues = (e, e$IDH_ingpc, IDH_ingpc2010)
mapvalues(e$IDH_ingpc, from = c(2, 3), to = c(5, 6))

e[ingpc] = map[IDH_ingpc2010]


gini <- rbind(gini2000, gini2010)
f  <- replace(e, IDH_ingpc2010, values = c("NA"))

ingpc <- r(e, IDH_ingpc2010, replace)

head(IDH_ingpc2010)
head(ingpc, 35)

summary(ingpc)
describe(d$IDH_ingpc)
describe(IDH_ingpc2010)


d$IDH_ingpc[match(IDH_ingpc2010$x1,df1$x1)] <- df2$x2

for(idedomun, year in:nrow(IDH_ingpc2010)){
  d$IDH_ingpc[]
}


for(id in 1:nrow(df2)){
  df1$x2[df1$x1 %in% df2$x1[id]] <- df2$x2[id]
}




