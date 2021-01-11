setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("WestRoxbury.csv",header=T)
housing.df

dim(housing.df)
head(housing.df)   

xtotal <- model.matrix(~0 + REMODEL, data=housing.df)
xtotal <- as.data.frame(xtotal)  

t(t(names(xtotal))) 
head(xtotal)

xtotal <- xtotal[,-2] 
head(xtotal)

