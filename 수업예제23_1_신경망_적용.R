library(neuralnet)
library(caret)

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")

###치즈 tinydata 인공신경망 example 
df <- read.csv("tinydata.csv")

nn <-neuralnet(Acceptance ~ Salt+Fat, data=df, linear.output=F, hidden=3)

nn$weights
plot(nn,rep="best")

predict <- compute(nn,data.frame(df$Salt,df$Fat))
predicted.class <- apply(predict$net.result,1,which.max)
confusionMatrix(factor(ifelse(predicted.class==2,"like","dislike")),factor(df$Acceptance))

###잔디깍기 기계 인공신경망 example
df <- read.csv("RidingMowers.csv")

#정규화(normalize) : 0에서 1사이로 변환
norm.values <- preProcess(df, method="range")
norm.df <- predict(norm.values, df)

nn <-neuralnet(Ownership ~ Income+Lot_Size, data=norm.df, linear.output=F, hidden=3)

nn$weights
plot(nn,rep="best")

predict <- compute(nn,norm.df[,c(1,2)]) 
## [,1] , [,2] 는 알파벳 순으로 Nonowner, Owner
predicted.class <- apply(predict$net.result,1,which.max)
confusionMatrix(factor(ifelse(predicted.class==2,"Owner","Nonowner")),factor(norm.df$Ownership))



### 개인대출 수락 인공신경망 example : 데이터 분할 적용
bank.df <- read.csv("UniversalBank.csv")

#데이터 전처리 : 모형에 포함되지 않는 변수 제거
df <- bank.df[,-c(1,5)]

#수치형 반응변수를 문자형 변수로 변환
df$Personal.Loan <- ifelse(df$Personal.Loan==1,"Yes","No")

# 데이터 분할 : 학습데이터 60% 검증데이터 40%
set.seed(1)
train.index <- sample(row.names(df), 0.6*dim(df)[1])  
valid.index <- setdiff(row.names(df), train.index)  
train.df <- df[train.index, ]
valid.df <- df[valid.index, ]

# 정규화(normalize) : 0에서 1사이로 변환
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

nn <-neuralnet(Personal.Loan ~ ., data=train.norm.df, linear.output=F, hidden=3)

nn$weights
plot(nn,rep="best")

predict <- compute(nn,valid.norm.df[,-8])
predicted.class <- apply(predict$net.result,1,which.max)
confusionMatrix(factor(ifelse(predicted.class==2,"Yes","No")),factor(valid.norm.df$Personal.Loan))





#로지스틱 회귀분석 예측 모형과 비교
bank.df <- read.csv("UniversalBank.csv")

#데이터 전처리 : 모형에 포함되지 않는 변수 제거
df <- bank.df[,-c(1,5)]

# 데이터 분할 : 학습데이터 60% 검증데이터 40%
set.seed(1)
train.index <- sample(row.names(df), 0.6*dim(df)[1])  
valid.index <- setdiff(row.names(df), train.index)  
train.df <- df[train.index, ]
valid.df <- df[valid.index, ]

logit <- glm(Personal.Loan ~ ., data=train.df,family="binomial")
summary(logit)

# 로지스틱 회귀분석의 성능은 인공신경망의 성능보다 낮다
pred <- predict(logit,valid.df,type="response")
confusionMatrix(factor(ifelse(pred >  0.5, 1, 0)), factor(valid.df$Personal.Loan))
