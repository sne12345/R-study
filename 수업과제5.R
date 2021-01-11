setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
car.df <- read.csv("ToyotaCorolla.csv")
# use first 1000 rows of data
car.df <- car.df[1:1000, ]
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
# partition data
set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)

# use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)
# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.
library(leaps)
# create dummies for fuel type
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=train.df))
# replace Fuel_Type column with 2 dummies
train.df1 <- cbind(train.df[,-4], Fuel_Type[,-1])
head(train.df1)
search <- regsubsets(Price ~ ., data = train.df1, nbest = 1, nvmax = dim(train.df1)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models
sum$which
# show metrics
sum$rsq
sum$adjr2
