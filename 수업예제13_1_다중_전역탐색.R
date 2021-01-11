setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
car.df <- read.csv("ToyotaCorolla.csv")

library(forecast)
install.packages("leaps")

car.df <- car.df[1:1000, ]
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)

set.seed(1)
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
car.lm <- lm(Price ~ ., data = train.df)
summary(car.lm)

library(leaps)
# 가변수 변환 필요
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=train.df))

# 원본데이터에서 Fuel_Type 빼주고, 세가지 가변수 중 한 열 빼줌 -> cbind로 원본데이터와 합치기
train.df1 <- cbind(train.df[,-4], Fuel_Type[,-1])
head(train.df1)  # 상위 6개 보여줌

# 매개변수 무슨의미? nbest는 n개 변수를 선택했을때 변수선택순위 n개까지보여줌
# nvmax는 최대 변수를 몇개까지 쓸건지 결정
# dim(train.df1)[2] : 열의개수 / dim(train.df1)[1] : 행의개수
search <- regsubsets(Price ~ ., data = train.df1, nbest = 1, nvmax = dim(train.df1)[2],method = "exhaustive")
sum <- summary(search)

# show models
sum$which
# show metrics
sum$rsq
sum$adjr2
  
car.lme7 <- lm(Price~Age_08_04+KM+HP+Quarterly_Tax+Weight+Fuel_TypeDiesel+Fuel_TypePetrol,data=train.df1)
# adj-R 값
summary(car.lme7)
