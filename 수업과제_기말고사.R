library(caret) #preProcess함수
library(neuralnet) #neuralnet함수
library(forecast) #accuracy함수
library(FNN) #knn함수

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("LTV.csv")
housing.df <- housing.df[,-1]

# 가변수 변환
housing.df$cartype_SUV <- ifelse(housing.df$cartype=="SUV",1,0)
housing.df$cartype_Sports <- ifelse(housing.df$cartype=="Sports",1,0)

# 학습데이터 생성
set.seed(10) # 랜덤 넘버 발생 : 교수님과 같은 랜덤순서를 갖게 하기 위해 실행
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.5)
train.df <- housing.df[train.rows,]

# 검증데이터 생성
valid.rows <- setdiff(rownames(housing.df),train.rows)
valid.df <- housing.df[valid.rows,]

# 다중선형회귀분석
car.lm <- lm(LTV ~ payment+pnum+SUV+Sports, data = train.df)
summary(car.lm)

# 정규화
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

lm1 <- lm(LTV ~ payment+pnum+SUV+Sports, data = train.norm.df)
pred <- predict(lm1,valid.norm.df[, c("payment","pnum","SUV","Sports")])
accuracy(pred,valid.norm.df$LTV)


# 인공신경망 (노드 2개)
nn <- neuralnet(LTV ~ payment+pnum+SUV+Sports, data = train.norm.df
                , linear.output = T, hidden = 2)

# 검증데이터에서 예측 정확도 계산
valid.pred=compute(nn, valid.norm.df[, c("payment","pnum","SUV","Sports")])
pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$LTV)

# 인공신경망 (노드 3개)
nn <- neuralnet(LTV ~ payment+pnum+SUV+Sports, data = train.norm.df
                , linear.output = T, hidden = 3)

# 검증데이터에서 예측 정확도 계산
valid.pred=compute(nn, valid.norm.df[, c("payment","pnum","SUV","Sports")])
pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$LTV)




# 로지스틱 회귀분석 적합 : glm()함수 family = "binomial" 이용  
logit.reg <- glm(response ~ payment+pnum+SUV+Sports, data = train.df, family = "binomial")
summary(logit.reg)

# 오즈
e1 <- exp(0.385547)
e2 <- exp(0.441000)

# Accuracy 계산
pred <- predict(logit.reg,valid.df,type="response")
confusionMatrix(factor(ifelse(pred > 0.13, 1, 0)), factor(valid.df$response))


# 정규화
train.norm.df <- train.df
valid.norm.df <- valid.df

norm.values <- preProcess(train.df[, c(1:3,6:7)], method=c("center", "scale"))
train.norm.df[,c(1:3,6:7)] <- predict(norm.values, train.df[, c(1:3,6:7)]) 
valid.norm.df[, c(1:3,6:7)] <- predict(norm.values, valid.df[, c(1:3,6:7)]) 

# k = 1
knn.pred <- knn(train.norm.df[,c(1:3,6:7)], valid.norm.df[, c(1:3,6:7)],
                cl = train.norm.df[, 5], k = 1)
confusionMatrix(factor(knn.pred),factor(valid.norm.df[, 5]))
649 / (36+649)

# k = 2
knn.pred <- knn(train.norm.df[,c(1:3,6:7)], valid.norm.df[, c(1:3,6:7)],
                cl = train.norm.df[, 5], k = 2)
confusionMatrix(factor(knn.pred),factor(valid.norm.df[, 5]))
519 / (166+519)

# k = 3
knn.pred <- knn(train.norm.df[,c(1:3,6:7)], valid.norm.df[, c(1:3,6:7)],
                cl = train.norm.df[, 5], k = 3)
confusionMatrix(factor(knn.pred),factor(valid.norm.df[, 5]))
529 / (156+529)


