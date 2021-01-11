setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스") #working directory설정
bank.df <- read.csv("UniversalBank.csv")

# 분할 전 전체 데이터에서 증권계좌 개설한 비율
table(bank.df$Securities.Account)

#데이터 분할 :  학습데이터 대 검증데이터 6:4 비율
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# 로지스틱 회귀분석 적합 : glm()함수 family = "binomial" 이용  
logit.reg <- glm(Securities.Account ~ ., data = train.df, family = "binomial")

options(scipen=999) # 소숫점 모두 표시
summary(logit.reg)

exp(3.94846333) # CD.Account이 하나 증가할 때, 오즈가 51.85562배 증가한다.
exp(-0.50923369)  # Online가 한 단위 증가할 때, 오즈가 0.6009559배 증가한다.
exp(-1.24045333)  # CreditCard이 한 단위 증가할 때, 오즈가 0.2892531배 증가한다.

pred <- predict(logit.reg,valid.df,type="response")
library(caret)
confusionMatrix(factor(ifelse(pred >  0.5, 1, 0)), factor(valid.df$Securities.Account))
