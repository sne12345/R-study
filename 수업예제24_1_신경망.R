setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")

library(caret) #preProcess함수
library(neuralnet) #neuralnet함수
library(forecast) #accuracy함수

## 데이터 불러오기
toyota.df <- read.csv("ToyotaCorolla.csv")

# 데이터 전처리: 범주형 변수에 대한 가변수 생성
toyota.df$Fuel_Type_CNG <- 1*(toyota.df$Fuel_Type == "CNG")
toyota.df$Fuel_Type_Diesel <- 1*(toyota.df$Fuel_Type == "Diesel")

# 데이터 분할 : 학습데이터 60% 검증데이터 40%
set.seed(1)
train.index <- sample(row.names(toyota.df), 0.6*dim(toyota.df)[1])
valid.index <- setdiff(row.names(toyota.df), train.index)
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[valid.index, ]

# 정규화(normalize) : 0에서 1사이로 변환
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

summary(toyota.df)

# 학습데이터에서 인공신경망 적합
nn <- neuralnet(Price ~ Age_08_04+
                  KM+
                  Fuel_Type_CNG+
                  Fuel_Type_Diesel+
                  HP+
                  Automatic+
                  Doors,
                data = train.norm.df, linear.output = T,
                hidden = 5)
## 예측이기 때문에 linear.output에 T를 넣어준다.
plot(nn)
## 예측이기 때문에 Output이 종속변수인 Price 하나로 나온다.

# 검증데이터에서 예측 정확도 계산
valid.pred=compute(nn, valid.norm.df[, c("Age_08_04",
                                         "KM",
                                         "Fuel_Type_CNG",
                                         "Fuel_Type_Diesel",
                                         "HP",
                                         "Automatic",
                                         "Doors")])
pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$Price)



## ==================<복습필수!>비교할때는 정규화해야함=====================
lm1 <- lm(Price ~ Age_08_04+
            KM+
            Fuel_Type_CNG+
            Fuel_Type_Diesel+
            HP+
            Automatic+
            Doors,
          data = train.norm.df)
pred <- predict(lm1,valid.norm.df[, c("Age_08_04",
                                  "KM",
                                  "Fuel_Type_CNG",
                                  "Fuel_Type_Diesel",
                                  "HP",
                                  "Automatic",
                                  "Doors")])
accuracy(pred,valid.norm.df$Price)

