install.packages("caret")
install.packages("FNN")

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
mower.df <- read.csv("RidingMowers.csv") #데이터불러오기

#데이터 분할 (학습데이터 60%, 검증데이터 40%)
set.seed(111)
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])
valid.index <- setdiff(row.names(mower.df), train.index)
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]

## 새로운 레코드(new household)
new.df <- data.frame(Income = 60, Lot_Size = 20)

## 산점도(scatter plot)
plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 2))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=2)
text(60, 20, "X")
legend("topleft", c("owner", "non-owner", "newhousehold"), pch = c(1, 2, 4))

# 정규화(normalize)를 위한 초기화
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df

# caret패키지의 preProcess() 함수 사용
library(caret)
norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2]) #학습데이터 정규화
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2]) #검증데이터 정규화
mower.norm.df[, 1:2] <- predict(norm.values, mower.df[, 1:2]) #전체데이터 정규화
new.norm.df <- predict(norm.values, new.df) #새 레코드 정규화


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


# FNN패키지의 knn() 함수 활용
library(FNN)
nn <- knn(train = train.norm.df[, 1:2], test = new.norm.df, cl = train.norm.df[, 3], k = 9)

# 근접 이웃 번호 
row.names(train.df)[attr(nn, "nn.index")]
# -> 4번, 14번, 3번 순으로 가깝다

# 분류 결과 확인 
nn
# -> 4번과 0.33만큼 거리, 14번과 0.578만큼 거리, 3번과 0.7만큼 거리



