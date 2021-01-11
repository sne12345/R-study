setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
airfare.df <- read.csv("Airfares.csv")

afc.df <- airfare.df[,-c(1,2,3,4,7,8,14,15,18)]

# 상관관계표
cor <- as.data.frame(cor(airfare.df[,18],afc.df))
cor_t <- as.data.frame(t(cor))

## 산점도(scatter plot)
plot(FARE ~ COUPON, data=airfare.df)
plot(FARE ~ NEW, data=airfare.df)
plot(FARE ~ HI, data=airfare.df)
plot(FARE ~ S_INCOME, data=airfare.df)
plot(FARE ~ E_INCOME, data=airfare.df)
plot(FARE ~ S_POP, data=airfare.df)
plot(FARE ~ E_POP, data=airfare.df)
plot(FARE ~ DISTANCE, data=airfare.df)
plot(FARE ~ PAX, data=airfare.df)



# 앞의 네 개 변수 제외하기
af1.df <- airfare.df[,c(5:18)]

# 가변수 변환
VACATION <- as.data.frame(model.matrix(~ 0 + VACATION, data=af1.df))
SW <- as.data.frame(model.matrix(~ 0 + SW, data=af1.df))
SLOT <- as.data.frame(model.matrix(~ 0 + SLOT, data=af1.df))
GATE <- as.data.frame(model.matrix(~ 0 + GATE, data=af1.df))

# 원본데이터에서 가변수 변환한 변수 빼주고, 
#  cbind로 변환된 데이터를 원본데이터와 합치기
af2.df <- cbind(af1.df[,-c(3,4,10,11)], VACATION, SW, SLOT, GATE)
head(af2.df)

#데이터 분할 (학습데이터 60%, 검증데이터 40%)
set.seed(111)
train.index <- sample(row.names(af2.df), 0.6*dim(af2.df)[1])
valid.index <- setdiff(row.names(af2.df), train.index)
train.df <- af2.df[train.index, ]
valid.df <- af2.df[valid.index, ]

# 회귀분석 모델 적합 및 검증 세트로 평가
# 학습데이터로 회귀분석 진행
library(forecast)
reg <- lm(FARE~., data = train.df)

# 검증데이터에서 예측값 추정 및 평가
pred <- predict(reg, newdata = valid.df)
accuracy(pred, valid.df$FARE)





# 단계적 선택
af.lm.step <- step(reg, direction = "both")
options(scipen = 999)
summary(af.lm.step) 



# 전역탐색 
library(leaps)
search <- regsubsets(FARE ~ ., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)

# show models
sum$adjr2
sum$which


