cd "C:\Users\Pancho\Dropbox\Base EVES (Preliminar)\"
clear
import delimited "poblacion_1980.csv", delimiter(";")
sort idedomuny
merge 1:1 idedomuny using BaseEVES


replace gini = gini[_n-5] if missing(gini)


