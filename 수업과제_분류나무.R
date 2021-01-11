library(rpart)#분류규칙 계산을 위한 패키지
library(rpart.plot)# 분류나무 그림을 위한 패키지

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
data.df <- read.csv("Telecom_v2.csv")
data.df <- data.df[ , -c(1)] 

# 데이터 분할
set.seed(1)
train.index <- sample(c(1:dim(data.df)[1]), dim(data.df)[1]*0.6)
train.df <- data.df[train.index, ]
valid.df <- data.df[-train.index, ] 

# 디폴트(default) 분류규칙 찾고 분류나무 그리기
default.ct <- rpart(Churn ~ ., data = train.df, method = "class")
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

# 복잡도 지수(cp)를 최소화하는 최적 가지치기 된 나무모델 그리기
cv.ct <- rpart(Churn ~ ., data = train.df, method = "class", cp = 0.0001, minsplit = 5, xval = 5)
printcp(cv.ct) # 복잡도 지수(cp), 교차검정 오류(xerror) 계산

# rpart함수를 최적화한 prune함수
pruned.ct <- prune(cv.ct, cp = 0.008)
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

library(caret) #분류정확도 산출을 위한 패키지

# 1. 디폴트 분류 나무 - 학습데이터
default.pred.train <- predict(default.ct, train.df,type = "class") # predict()함수로 소속 클래스 예측
cm<-confusionMatrix(factor(default.pred.train), factor(train.df$Churn)) # 혼동행렬(confusion matrix)생성
cm$overall[1]#분류정확도 출력

# 2. 디폴트 분류 나무 - 검증데이터
default.pred.valid <- predict(default.ct, valid.df,type = "class")
cm<-confusionMatrix(factor(default.pred.valid), factor(valid.df$Churn))
cm$overall[1]#분류정확도 출력

# 3. 최적 분류 나무 - 학습데이터
pruned.pred.train <- predict(pruned.ct, train.df,type = "class")
cm<-confusionMatrix(factor(pruned.pred.train), factor(train.df$Churn))
cm$overall[1]#분류정확도 출력

# 4. 최적 분류 나무 - 검증데이터
pruned.pred.valid <- predict(pruned.ct, valid.df,type = "class")
cm<-confusionMatrix(factor(pruned.pred.valid), factor(valid.df$Churn))
cm$overall[1]#분류정확도 출력

