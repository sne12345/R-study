## 보스턴 문제 b.다중선형회귀모델 만들기
setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("BostonHousing.csv")

## 숫자 깔끔하게 하기 위해서 데이터 500개 뽑기
housing.df <- housing.df[1:500, ]
## CRIM, CHAS, RM 변수와 y값인 MEDV만 뽑아내기
selected.var <- c(1,4,6,13)


## 랜덤하게 섞은 데이터를 학습데이터와 검증데이터로 나누기
set.seed(111) 
train.index <- sample(c(1:500), 300)
train.df <- housing.df[train.index, selected.var]
valid.df <- housing.df[-train.index, selected.var]
housing.lm <- lm(MEDV ~ ., data = train.df)

## 회귀계수 알아보기
summary(housing.lm)


## 보스턴 문제 c.새로운 데이터로 주택가격 중앙값 예측하기, 예측오차 구하기

## 새로운 데이터 추가
new.df <- data.frame(CRIM = 0.1, CHAS = 0, RM = 6)

## 새로운 데이터 MEDV 예측 
housing.lm.pred <- predict(housing.lm, new.df)

accuracy(housing.lm.pred, housing.df$MEDV)


## MEDV를 제외한 나머지 변수들로 데이터 구성
housing1.df <- housing.df[,c(1:12,14)]

## 13개 데이터 관련성 분석
cor(housing1.df, housing1.df)


cor.mat <- round(cor(housing1.df),2)
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x=X1, y=X2, fill=value))+geom_tile()+geom_text(aes(x=X1, y=X2, label=value))




###______________________




## 보스턴 문제 b.주택가격 중앙값 예측하기
library(forecast)
housing.lm.pred <- predict(housing.lm, valid.df)
some.residuals <- valid.df$MEDV[1:20] - housing.lm.pred[1:20]
data.frame("Predicted" = housing.lm.pred[1:20], "Actual" = valid.df$MEDV[1:20], "Residual" = some.residuals)
accuracy(housing.lm.pred, valid.df$MEDV)

library(leaps)
# 매개변수 무슨의미? nbest는 n개 변수를 선택했을때 변수선택순위 n개까지보여줌
# nvmax는 최대 변수를 몇개까지 쓸건지 결정
# dim(train.df1)[2] : 열의개수 / dim(train.df1)[1] : 행의개수
search <- regsubsets(MEDV ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2],method = "exhaustive")
sum <- summary(search)


# show models
sum$which
# show metrics
sum$rsq
sum$adjr2





