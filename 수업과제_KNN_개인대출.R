setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[,-c(1,5)]

#데이터 분할 (학습데이터 60%, 검증데이터 40%)
set.seed(1)
train.index <- sample(row.names(bank.df), 0.6*dim(bank.df)[1])
valid.index <- setdiff(row.names(bank.df), train.index)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]

## 새로운 레코드(new household)
new.df <- data.frame(Age = 48, Experience = 22, Income = 152, Family = 1, CCAvg = 3.5, Education = 3, Mortgage = 0, Securities.Account = 0,
                     CD.Account = 0, Online = 1, CreditCard = 0)

# 정규화(normalize)를 위한 초기화
train.norm.df <- train.df
valid.norm.df <- valid.df

# caret패키지의 preProcess() 함수 사용
norm.values <- preProcess(train.df[, c(1:7,9:12)], method=c("center", "scale"))
train.norm.df[, c(1:7,9:12)] <- predict(norm.values, train.df[, c(1:7,9:12)]) #학습데이터 정규화
valid.norm.df[, c(1:7,9:12)] <- predict(norm.values, valid.df[, c(1:7,9:12)]) #검증데이터 정규화
new.norm.df <- predict(norm.values, new.df) #새 레코드 정규화

# FNN패키지의 knn() 함수 활용
nn <- knn(train = train.norm.df[, c(1:7,9:12)], test = new.norm.df, 
          cl = train.norm.df[, 8], k = 3)

# 근접 이웃 번호 
row.names(train.df)[attr(nn, "nn.index")]

# 분류 결과 확인 
nn


# accuracy.df 데이터 초기화
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# k값을 1에서 14까지 변화시키면서 분류 정확도 값 저장
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, c(1:7,9:12)], valid.norm.df[, c(1:7,9:12)],
                  cl = train.norm.df[, 8], k = i)
  accuracy.df[i, 2] <- confusionMatrix(factor(knn.pred), 
                                       factor(valid.norm.df[, 8]))$overall[1]
}
# -> factor 각각 붙여주기
accuracy.df

knn.pred <- knn(train.norm.df[, c(1:7,9:12)], valid.norm.df[, c(1:7,9:12)],
                cl = train.norm.df[, 8], k = 3)
confusionMatrix(factor(knn.pred), factor(valid.norm.df[, 8]))



# -----------------------------------------------
new_1.df <- data.frame(Age = 48, Experience = 22, Income = 152, Family = 1, CCAvg = 3.5, Education = 3, Mortgage = 0, Securities.Account = 0,
                     CD.Account = 0, Online = 1, CreditCard = 0)
new_1.norm.df <- predict(norm.values, new_1.df) #새 레코드 정규화

# FNN패키지의 knn() 함수 활용
nn <- knn(train = train.norm.df[, c(1:7,9:12)], test = new_1.norm.df, 
          cl = train.norm.df[, 8], k = 3)

# 분류 결과 확인 
nn

#-------------------------------------------------
train.rows <- sample(rownames(bank.df), dim(bank.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(bank.df), train.rows), dim(bank.df)[1]*0.3)
test.rows <- setdiff(rownames(bank.df), union(train.rows, valid.rows))

train.data <- bank.df[train.rows, ]
valid.data <- bank.df[valid.rows, ]
test.data <- bank.df[test.rows, ]

# 정규화(normalize)를 위한 초기화
train.norm.df <- train.data
valid.norm.df <- valid.data
test.norm.df <- test.data

# caret패키지의 preProcess() 함수 사용
norm.values <- preProcess(train.data[, c(1:7,9:12)], method=c("center", "scale"))
train.norm.df[, c(1:7,9:12)] <- predict(norm.values, train.data[, c(1:7,9:12)]) #학습데이터 정규화
valid.norm.df[, c(1:7,9:12)] <- predict(norm.values, valid.data[, c(1:7,9:12)]) #검증데이터 정규화
test.norm.df[, c(1:7,9:12)] <- predict(norm.values, test.data[, c(1:7,9:12)]) #평가데이터 정규화

# 검증 데이터 예측
knn.pred <- knn(train.norm.df[, c(1:7,9:12)], valid.norm.df[, c(1:7,9:12)],
                cl = train.norm.df[, 8], k = 3)
confusionMatrix(factor(knn.pred), factor(valid.norm.df[, 8]))

# 평가 데이터 예측
knn.pred <- knn(train.norm.df[, c(1:7,9:12)], test.norm.df[, c(1:7,9:12)],
                cl = train.norm.df[, 8], k = 3)
confusionMatrix(factor(knn.pred), factor(test.norm.df[, 8]))
