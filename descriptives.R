
############ 1. Descriptivos lectura de datos ################
# Este codigo es para identificar los elementos necesarios en la
# tabla final
# durante la construccion de las tables
str(pobalimentaria)
# idedomun no tiene el formato correcto
# se requiere una cadena, en lugar del entero
# poner un cero cuando la longitud del entero sea 4

table(pobalimentaria$year)
# 2456 municipios en ambos periodos

str(d)
# que varibles se usan de esta tabla?
# idedomun, NomEdo, NomMun, year
# 


# illit2000
# Clave  # estado municipio
# Nombre        # municipio
# Total                  # poblacion en edad de saber leer y escribir?
# Sabe.leer.y.escribir   
# No.sabe.leer.y.escribir
summary(illit2000$illit)
head(illit2000$illit)

table(illit$year)
# 2000 2010 
# 2578 2578 

#
par(pty="s")   #plot perfectly square graphs.
tapply(d$pobalimentaria, d$year, summary) # muy buena esta tapply.
plot(d$pobalimentaria, d$illit, pty = "s")


# 
summary(d$gini)

summary(d$remedu)


# Not sure
summary(d$emigerm)  


######### 2. Descriptivos de la tabla final ###########
load('newD.Rdata')

str(ds)
# summary variables
stargazer(ds, type = "text", title="Descriptive statistics", digits=2, out="table1.txt")

# by year, 

# detailed
summary(ds$ret)

