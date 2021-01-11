setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")

library(forecast)
toyota.df <- read.csv("ToyotaCorolla.csv")

set.seed(1)
training <- sample(toyota.df$Id, 600)
validation <- sample(setdiff(toyota.df$Id, training), 400)

reg1 <- lm(Price~Mfg_Month+KM+HP, data=toyota.df[training,], na.action = na.exclude) 
# , subset=training 도 가능
pred_t <- predict(reg1, na.action = na.pass) # 학습데이터를 이용한 예측값 계산
pred_v <- predict(reg1, newdata = toyota.df[validation,], na.action = na.pass) 
# 검증데이터를 이용한 예측값 계산
# 경고메세지 무시해도 됨

# 예측 성능 평가
# 학습데이터
accuracy(pred_t, toyota.df[training,]$Price)
# 검증데이터
accuracy(pred_v, toyota.df[validation,]$Price)
# 더 많은 x를 가지고 있을 때 학습데이터는 더 오차가 작지만,
# 의미있는 소수의 x만을 넣는다면 검증데이터의 오차가 더 작아질 수도 있다.
