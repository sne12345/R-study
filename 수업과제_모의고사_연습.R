library(neuralnet)
library(caret)

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
df <- read.csv("bsoap.csv")
df <- df[,-1]

df$genderfemale <- ifelse(df$gender=="female",1,0) #범주형 변수 가변수 변환

# 데이터 분할 : 학습데이터 60% 검증데이터 40%
set.seed(1)
train.index <- sample(row.names(df), 0.6*dim(df)[1])  
valid.index <- setdiff(row.names(df), train.index)  
train.df <- df[train.index, ]
valid.df <- df[valid.index, ]

# 다중선형회귀분석 
car.lm <- lm(sales ~ genderfemale+age+hs+child+affluence, data = train.df)
summary(car.lm)

# 정규화(normalize) : 0에서 1사이로 변환
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

# 다중선형회귀분석 
car.lm <- lm(sales ~ genderfemale+age+hs+child+affluence, data = train.norm.df)
summary(car.lm)

# 예측 모델 / 정확도 분석
car.lm.pred <- predict(car.lm, valid.norm.df[,c("genderfemale","age","hs","child","affluence")])
accuracy(car.lm.pred, valid.norm.df$sales)


# 인공신경망 적합(노드 3)
nn <- neuralnet(sales ~ genderfemale+age+hs+child+affluence,
                data = train.norm.df, linear.output = T,
                hidden = 3)

# 검증데이터에서 예측 정확도 계산
valid.pred=compute(nn, valid.norm.df[, c("genderfemale","age","hs","child","affluence")])
pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$sales)


# 인공신경망 적합(노드 4)
nn <- neuralnet(sales ~ genderfemale+age+hs+child+affluence,
                data = train.norm.df, linear.output = T,
                hidden = 4)

# 검증데이터에서 예측 정확도 계산
valid.pred=compute(nn, valid.norm.df[, c("genderfemale","age","hs","child","affluence")])
pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$sales)

# 인공신경망 적합(노드 5)
nn <- neuralnet(sales ~ genderfemale+age+hs+child+affluence,
                data = train.norm.df, linear.output = T,
                hidden = 5)

# 검증데이터에서 예측 정확도 계산
valid.pred=compute(nn, valid.norm.df[, c("genderfemale","age","hs","child","affluence")])
pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$sales)


# 로지스틱 회귀분석 적합 : glm()함수 family = "binomial" 이용  
logit.reg <- glm(bl ~ genderfemale+age+hs+child+affluence, data = train.df, family = "binomial")
summary(logit.reg)

# 로지스틱 분류 정확도
pred <- predict(logit.reg,valid.df,type="response")
confusionMatrix(factor(ifelse(pred >  0.5, 1, 0)), factor(valid.df$bl))


# 정규화(normalize)를 위한 초기화
train.norm.df <- train.df
valid.norm.df <- valid.df

# caret패키지의 preProcess() 함수 사용
norm.values <- preProcess(train.df[, c(2:6,8)], method=c("center", "scale"))
train.norm.df[, c(2:6,8)] <- predict(norm.values, train.df[, c(2:6,8)]) #학습데이터 정규화
valid.norm.df[, c(2:6,8)] <- predict(norm.values, valid.df[, c(2:6,8)]) #검증데이터 정규화

# FNN패키지의 knn() 함수 활용
nn <- knn(train = train.norm.df[, c(2:6,8)], valid.norm.df[, c(2:6,8)], 
          cl = train.norm.df[, 7], k = 5)

# 분류 결과 확인 
nn

# 분류 정확도 분석
confusionMatrix(factor(nn),factor(valid.norm.df[, 7]))
