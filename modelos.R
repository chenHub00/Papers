

#Model 1
m1 <- glm.nb(horatesimbad ~ logemig + logrem + logret + logcirc + poverty + wosec + 
                 gini + loggdp + ied + divrate + ruralcorp + factor(year), data = newd)

summary(m1)

#Modelo 2
# en niveles
m2 <- glm.nb(homicide_counts ~ emig + rem + ret + circ + poverty + wosec + 
                 gini + IDH_ingpc + ied + divrate + ruralcorp + factor(year), data = newd)


summary(m2)

# modelo 3
m3 <- glm.nb(homicide_counts ~ emig + rem + ret + circ + 
                  pop+poverty + wosec + 
                 gini + IDH_ingpc + ied + divrate + ruralcorp + factor(year), data = newd)


summary(m2)
