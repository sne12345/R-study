setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)] # ID와 우편번호(zip code) 열 삭제

# 데이터 분할
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# 디폴트(기본) 분류 규칙 찾기 classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp=0, minsplit=1)

# 분류 정확도 계산하는 caret패키지 설치
library(caret) 

# 1. 디폴트 분류 나무 - 학습데이터
default.pred.train <- predict(default.ct, train.df,type = "class") # predict()함수로 소속 클래스 예측
confusionMatrix(factor(default.pred.train), factor(train.df$Personal.Loan)) # 혼동행렬(confusion matrix)생성

# 2. 디폴트 분류 나무 - 검증데이터
default.pred.valid <- predict(default.ct, valid.df,type = "class")
confusionMatrix(factor(default.pred.valid), factor(valid.df$Personal.Loan))

# 3. 깊은 분류 나무 - 학습데이터
deeper.pred.train <- predict(deeper.ct, train.df,type = "class")
confusionMatrix(factor(deeper.pred.train), factor(train.df$Personal.Loan))

# 4. 깊은 분류 나무 - 검증데이터
deeper.pred.valid <- predict(deeper.ct, valid.df,type = "class")
confusionMatrix(factor(deeper.pred.valid), factor(valid.df$Personal.Loan))

