setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[,-c(1,5)]

# 학력(Education)변수를 범주형 변수로 변환 : factor()함수 이용
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3),labels = c("학부", "석사","박사"))

# 분할 전 전체 데이터에서 증권계좌 개설한 비율
table(bank.df$Securities.Account)

#데이터 분할 :  학습데이터 대 검증데이터 6:4 비율
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# 로지스틱 회귀분석 적합 : glm()함수 family = "binomial" 이용  
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial")
options(scipen=999) # 소숫점 모두 표시
summary(logit.reg)


pred <- predict(logit.reg,valid.df,type="response")
confusionMatrix(factor(ifelse(pred >  0.5, 1, 0)), 
                factor(valid.df$Personal.Loan))


pred <- predict(logit.reg,valid.df,type="response")
confusionMatrix(factor(ifelse(pred >  0.2, 1, 0)), 
                factor(valid.df$Personal.Loan))
