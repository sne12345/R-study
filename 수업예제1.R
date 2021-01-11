
setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
laptop.df <- read.csv("LaptopSales.csv",header=T)

dim(laptop.df)
head(laptop.df)
View(laptop.df)

laptop.df[1:10,1]
laptop.df[1:10,5]

subset.df1 <- laptop.df[1:10,5:9]
subset.df2 <- laptop.df[c(1:10,40:50),c(1,2,4,8:10)]

laptop.df$Retail.Price[1:10]

length(laptop.df$Retail.Price)
mean(laptop.df$Retail.Price,na.rm=F) 
summary(laptop.df$Retail.Price)
summary(laptop.df)
