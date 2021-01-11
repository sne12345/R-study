setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스") #working directory설정
bank.df <- read.csv("UniversalBank.csv") #데이터불러오기
bank.df <- bank.df[,-c(1,5)] # ID와 ZIP code 제거

#데이터 분할 (학습데이터 60%, 검증데이터 40%)
set.seed(111)
train.index <- sample(row.names(bank.df), 0.6*dim(bank.df)[1])
valid.index <- setdiff(row.names(bank.df), train.index)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]

## 새로운 레코드(new household)
new.df <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education = 2, Mortgage = 0, Securities.Account = 0, CD.Account=0, Online=1, CreditCard=1)

# 정규화(normalize)를 위한 초기화
train.norm.df <- train.df
valid.norm.df <- valid.df
bank.norm.df <- bank.df

# caret패키지의 preProcess() 함수 사용
library(caret)
norm.values <- preProcess(train.df[, -8], method=c("center", "scale"))
train.norm.df[, -8] <- predict(norm.values, train.df[, -8]) #학습데이터 정규화
valid.norm.df[, -8] <- predict(norm.values, valid.df[, -8]) #검증데이터 정규화
bank.norm.df[, -8] <- predict(norm.values, bank.df[, -8]) #전체데이터 정규화
new.norm.df <- predict(norm.values, new.df) #새 레코드 정규화

# FNN패키지의 knn() 함수 활용
library(FNN)
nn <- knn(train = train.norm.df[, -8], test = new.norm.df, cl = train.norm.df[, 8], k = 1)

# 근접 이웃 번호 
row.names(train.df)[attr(nn, "nn.index")]

# 분류 결과 확인 : 유크리디안 거리
nn

# accuracy.df 데이터 초기화
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# k값을 1에서 14까지 변화시키면서 분류 정확도 값 저장
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, -8], valid.norm.df[, -8],
                  cl = train.norm.df[, 8], k = i)
  accuracy.df[i, 2] <- confusionMatrix(factor(knn.pred), factor(valid.norm.df[, 8]))$overall[1]
}

accuracy.df
# -> k가 3일 때 가장 정확도가 높다.

# k가 3인 Confusion matrix 그리기
knn.pred <- knn(train.norm.df[, -8], valid.norm.df[, -8],
                cl = train.norm.df[, 8], k = 3)
confusionMatrix(factor(knn.pred), factor(valid.norm.df[, 8]))



## 새로운 레코드(new household)
new1.df <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education = 2,
                      Mortgage = 0, Securities.Account = 0, CD.Account = 0, Online = 1,
                      CreditCard = 1)

#새 레코드 정규화
new1.norm.df <- predict(norm.values, new1.df) 

# FNN패키지의 knn() 함수 활용
nn <- knn(train = train.norm.df[, -8], test = new1.norm.df, cl = train.norm.df[, 8], k = 3)

# 근접 이웃 번호 
row.names(train.df)[attr(nn, "nn.index")]

# 분류 결과 확인 : 유크리디안 거리
nn
