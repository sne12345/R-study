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

# 로지스틱 회귀분석 적합 : glm()함수 family = "binomial" 이용  
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial")
options(scipen=999) # 소숫점 모두 표시
summary(logit.reg)

e1 <- exp(-6.272749 + 0.0384*0)
e2 <- exp(-6.272749 + 0.0384*100)
# 소득이 0인 사람보다 소득이 100,000인 사람의 개인대출 수락 확률이 46배 높다
e2 / e1

exp(0.5476224)
exp(-0.5602502)
