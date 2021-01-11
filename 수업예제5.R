install.packages("forecast")
library(forecast)

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("WestRoxbury.csv")

set.seed(10)

train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.6)
valid.rows <- setdiff(rownames(housing.df),train.rows)

train.df <- housing.df[train.rows,]
valid.df <- housing.df[valid.rows,]

lm(TOTAL.VALUE~LOT.SQFT+YR.BUILT+FLOORS, data = housing.df)

# 학습데이터로 회귀분석 진행
reg <- lm(TOTAL.VALUE~LOT.SQFT+YR.BUILT, data = train.df)
reg1 <- lm(TOTAL.VALUE~LOT.SQFT+YR.BUILT+FLOORS, data = train.df)

# 검증데이터에서 예측값 추정
pred <- predict(reg, newdata = valid.df)
accuracy(pred, valid.df$TOTAL.VALUE)

pred1 <- predict(reg1, newdata = valid.df)
accuracy(pred1, valid.df$TOTAL.VALUE)


install.packages("forecast")
library(forecast)

# 학습데이터로 회귀분석 진행
reg <- lm(TOTAL.VALUE~LOT.SQFT+YR.BUILT, data = train.df)
reg1 <- lm(TOTAL.VALUE~LOT.SQFT+YR.BUILT+FLOORS, data = train.df)

# 검증데이터에서 예측값 추정
pred <- predict(reg, newdata = valid.df)
accuracy(pred, valid.df$TOTAL.VALUE)
pred1 <- predict(reg1, newdata = valid.df)
accuracy(pred1, valid.df$TOTAL.VALUE)

