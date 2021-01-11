setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")

library(forecast)
toyota.df <- read.csv("ToyotaCorolla.csv")

set.seed(1)
training <- sample(toyota.df$Id, 600)
validation <- sample(setdiff(toyota.df$Id, training), 400)

reg <- lm(Price~., data=toyota.df[training,-c(1,2,8,11)], na.action = na.exclude) 
# , subset=training 도 가능
pred_t <- predict(reg, na.action = na.pass) # 학습데이터를 이용한 예측값 계산
pred_v <- predict(reg, newdata = toyota.df[validation,-c(1,2,8,11)], na.action = na.pass) 
# 검증데이터를 이용한 예측값 계산
# 경고메세지 무시해도 됨

# 예측 성능 평가
# 학습데이터
accuracy(pred_t, toyota.df[training,]$Price)
# 검증데이터
accuracy(pred_v, toyota.df[validation,]$Price)

