setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("WestRoxbury.csv")

set.seed(10)
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.6)
valid.rows <- setdiff(rownames(housing.df),train.rows)

train.df <- housing.df[train.rows,]
valid.df <- housing.df[valid.rows,]

library(forecast)

reg1 <- lm(TOTAL.VALUE~LIVING.AREA+BEDROOMS, data=train.df)
pred <- predict(reg1, newdata = valid.df)
accuracy(pred, valid.df$TOTAL.VALUE)

reg2 <- lm(TOTAL.VALUE~TAX+KITCHEN+FLOORS, data=train.df)
pred <- predict(reg2, newdata = valid.df)
accuracy(pred, valid.df$TOTAL.VALUE)

