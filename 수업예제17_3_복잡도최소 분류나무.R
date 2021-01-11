setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)] # ID와 우편번호(zip code) 열 삭제

# 데이터 분할
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# 복잡도 지수(cp)를 최소화하는 최적 가지치기 된 나무모델 그리기
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class",
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct) # 복잡도 지수(cp), 교차검정 오류(xerror) 계산

# rpart함수를 최적화한 prune함수
pruned.ct <- prune(cv.ct,
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

# Accuracy 구하기 
pruned.pred.train <- predict(pruned.ct, train.df,type = "class") # predict()함수로 소속 클래스 예측
confusionMatrix(factor(pruned.pred.train), factor(train.df$Personal.Loan)) # 혼동행렬(confusion matrix)생성

