setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
car.df <- read.csv("ToyotaCorolla.csv")

# 숫자 깔끔하게 하기 위해서 위에서부터 1000개 뽑음
car.df <- car.df[1:1000,]
# Gears, Guarantee_Period, Airco 변수 추가 
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 16, 17, 18, 21, 25)

# partition data
set.seed(1) 
# 1000개의 데이터 중에서 랜덤으로 600개 추출
train.index <- sample(c(1:1000), 600)

train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
# use . after ~ to include all the remaining columns in train.df as predictors.
options(scipen=999)
car.lm <- lm(Price ~ ., data = train.df)

library(forecast)
# use predict() to make predictions on a new set.
car.lm.pred <- predict(car.lm, valid.df)

# 소수점 아래 지우기
options(scipen=999, digits = 0)
some.residuals <- valid.df$Price[1:20] - car.lm.pred[1:20]
# 20개 데이터 확인, 오차
data.frame("Predicted" = car.lm.pred[1:20], "Actual" = valid.df$Price[1:20], "Residual" = some.residuals)

# 소수점 셋째자리까지 표시
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(car.lm.pred, valid.df$Price)

all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400 # length는 생략
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
