
load('DS.Rdata')

str(ds)
# summary variables
stargazer(ds, type = "text", title="Descriptive statistics", digits=2, out="table1.txt")

# by year, 

# detailed
summary(ds$ret)

# five-year averages?