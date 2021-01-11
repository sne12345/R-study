setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스") #working directory설정

#데이터 불러오기
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)] # 분석에 사용되지 않는 열 삭제(ID, zipcode)

# 학력(Education)변수를 범주형 변수로 변환 : factor()함수 이용
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3),labels = c("학부", "석사","박사"))

#데이터 분할 :  학습데이터 대 검증데이터 6:4 비율
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

pred <- predict(logit.reg,valid.df,type="response")
library(caret)
confusionMatrix(factor(ifelse(pred >  0.5, 1, 0)), factor(valid.df$Personal.Loan))
