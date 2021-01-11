
# train 50%, valid 30%, test 20% 할당
train.rows <- sample(rownames(bank.df), dim(bank.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(bank.df), train.rows), dim(bank.df)[1]*0.3)
test.rows <- setdiff(rownames(bank.df), union(train.rows, valid.rows))
train.data <- bank.df[train.rows, ]
valid.data <- bank.df[valid.rows, ]
test.data <- bank.df[test.rows, ]

# 정규화(normalize)를 위한 초기화
train.norm.data <- train.data
valid.norm.data <- valid.data
test.norm.data <- test.data
bank.norm.data <- bank.df

# 정규화
norm.values <- preProcess(train.data[, -c(1,5,10)], method=c("center", "scale"))
train.norm.data[, -c(1,5,10)] <- predict(norm.values, train.data[, -c(1,5,10)]) #학습데이터 정규화
valid.norm.data[, -c(1,5,10)] <- predict(norm.values, valid.data[, -c(1,5,10)]) #검증데이터 정규화
test.norm.data[, -c(1,5,10)] <- predict(norm.values, test.data[, -c(1,5,10)]) #검증데이터 정규화
bank.norm.data[, -c(1,5,10)] <- predict(norm.values, bank.df[, -c(1,5,10)]) #전체데이터 정규화

# training set을 기준으로 validation set의 Confusion matrix 그리기
knn.pred <- knn(train.norm.data[, -c(1,5,10)], valid.norm.data[, -c(1,5,10)],
                cl = train.norm.data[, 10], k = 7)
confusionMatrix(factor(knn.pred), factor(valid.norm.data[, 10]))

# training set을 기준으로 test set의 Confusion matrix 그리기
knn.pred <- knn(train.norm.data[, -c(1,5,10)], test.norm.data[, -c(1,5,10)],
                cl = train.norm.data[, 10], k = 7)
confusionMatrix(factor(knn.pred), factor(test.norm.data[, 10]))
