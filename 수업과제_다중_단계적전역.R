library(forecast)
library(leaps)

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
airfare.df <- read.csv("Airfares.csv")
airfare.df <- airfare.df[,-c(1,2,3,4)]

airfare.df$SW_Yes <- ifelse(airfare.df$SW=="Yes",1,0)
airfare.df$VACATION_Yes <- ifelse(airfare.df$VACATION=="Yes",1,0)
airfare.df$SLOT_Free <- ifelse(airfare.df$SLOT=="Free",1,0)
airfare.df$GATE_Free <- ifelse(airfare.df$GATE=="Free",1,0)

airfare.df <- airfare.df[,-c(3,4,10,11)]

# 데이터 분할 : 학습데이터 60% 검증데이터 40%
set.seed(1)
train.index <- sample(row.names(airfare.df), 0.6*dim(airfare.df)[1])
valid.index <- setdiff(row.names(airfare.df), train.index)
train.df <- airfare.df[train.index, ]
valid.df <- airfare.df[valid.index, ]

car.lm <- lm(FARE ~ ., data = train.df)
car.lm.pred <- predict(car.lm, valid.df)
accuracy(car.lm.pred, valid.df$Price)

# 단계적
car.lm.step <- step(car.lm, direction = "both")
summary(car.lm.step) 

car.lm.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.pred, valid.df$FARE)

# 전역 탐색 방법
search <- regsubsets(FARE ~ ., data = train.df, nbest = 1,
                     nvmax = dim(train.df)[2],method = "exhaustive")
sum <- summary(search)

# show models
sum$which
# show metrics
sum$rsq
sum$adjr2

# 예측 정확도 구하기 
car.lme7 <- lm(FARE ~ DISTANCE+SW_Yes+VACATION_Yes,data=train.df)
car.lm.pred <- predict(car.lme7, valid.df)
accuracy(car.lm.pred, valid.df$FARE)

