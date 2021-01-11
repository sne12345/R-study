setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스") #working directory설정

#데이터 불러오기
bank.df <- read.csv("BostonHousing.csv")

#데이터 분할 :  학습데이터 대 검증데이터 6:4 비율
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# 로지스틱 회귀분석 적합 : glm()함수 family = "binomial" 이용  
logit.reg <- glm(CAT..MEDV ~ RM+TAX+CRIM, data = train.df, family = "binomial")

options(scipen=999) # 소숫점 모두 표시
summary(logit.reg)
exp(4.018514) # RM이 하나 증가할 때, 오즈가 55.62배 증가한다.
exp(-0.004178)  # TAX가 한 단위 증가할 때, 오즈가 0.99배 증가한다.
exp(-0.043743)  # CRIM이 한 단위 증가할 때, 오즈가 0.96배 증가한다.

pred <- predict(logit.reg,valid.df,type="response")
library(caret)
confusionMatrix(factor(ifelse(pred >  0.3, 1, 0)), factor(valid.df$Securities.Account))
