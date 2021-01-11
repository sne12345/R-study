setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스") #working directory설정

#데이터 불러오기
df <- read.csv("BostonHousing.csv")

# 다중 선형 회귀 분석 - RM & MEDV 
plot(df$RM, df$MEDV)
m1 <- lm(MEDV~RM,data=df)
summary(m1)

# 로지스틱 선형 회귀 분석 - RM & CAT..MEDV -> 가변수
plot(df$RM, df$CAT..MEDV)
m2 <- glm(CAT..MEDV~RM, data=df, family="binomial")
summary(m2)

e1 <- exp(-32.5547) 
e2 <- exp(-32.5547+ 4.6640) 
e2 / e1 # 방이 하나 증가할 때, 오즈가 106.0595배 증가
