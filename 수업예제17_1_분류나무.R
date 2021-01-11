install.packages("rpart.plot")
install.packages("rpart")

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
bank.df <- read.csv("UniversalBank.csv")

library(rpart)
library(rpart.plot)
bank.df <- bank.df[ , -c(1, 5)] # ID와 우편번호(zip code) 열 삭제

# 데이터 분할
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# 디폴트(기본) 분류 규칙 찾기 classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")

# 디폴트(기본) 분류 나무 그리기 plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

# 디퍼(깊은) 분류 규칙 찾기
deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp=0, minsplit=1)

# 디퍼(깊은) 분류 나무 그리기
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)
