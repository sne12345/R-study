setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
car.df <- read.csv("ToyotaCorolla.csv")

library(forecast)

car.df <- car.df[1:1000, ]
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

set.seed(1)
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
car.lm <- lm(Price ~ ., data = train.df)
summary(car.lm)

# create model with no predictors for bottom of search range
car.lm.null <- lm(Price~1, data = train.df)
summary(car.lm.null)

# 전방선택방법
car.lm.step <- step(car.lm.null,   
                    scope=list(lower=car.lm.null, upper=car.lm), direction =  
                      "forward")
summary(car.lm.step) 
# adj-R이 전역탐색과 결과동일 but 더 빠름

# 후방소거법
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step) 

# 단계적 선택방법
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step) 

library(forecast)
# use predict() to make predictions on a new set.
car.lm.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.pred, valid.df$Price)

# 예측력과 설명력이 비례하지는 않기 때문에 설명력이 큰 변수를 골라도
# 예측오차가 꼭 최소는 아니다.

car.lm.pred <- predict(car.lm.null, valid.df)
accuracy(car.lm.pred, valid.df$Price)

