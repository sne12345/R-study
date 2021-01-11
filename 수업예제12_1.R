setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
car.df <- read.csv("ToyotaCorolla.csv")

# 숫자 깔끔하게 하기 위해서 위에서부터 1000개 뽑음
car.df <- car.df[1:1000,]
# Gears, Guarantee_Period, Airco 변수 추가 
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 16, 17, 18, 21, 25)

# partition data
set.seed(1) 
# 1000개의 데이터 중에서 랜덤으로 600개 추출
train.index <- sample(c(1:1000), 600)

train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)

# use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)
# 여기까지는 설명모델
# Residuals -> 오차
