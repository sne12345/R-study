install.packages("caret")
install.packages("e1071")
install.packages("pROC")

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
owner.df <- read.csv("ownerExample.csv")

library("caret")
library("e1071")
library("pROC")

# 오분류행렬 -> nonowner가 c1
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5,"owner","nonowner")),owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.25,"owner","nonowner")),owner.df$Class)
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.75,"owner","nonowner")),owner.df$Class)

# 정확도
(10+11)/24

# 민감도(owner가 관심사일때)
11/(1+11)

# 특이도(owner가 관심사일때)
10/(10+2)

# ROC 곡선 그리기 -> 자동으로 owner가 관심사라고 가정
r<- roc(owner.df$Class,owner.df$Probability)
plot.roc(r)

# AUC면적 구하기
auc(r)

# 예를들어서, 컷오프가 0.25일 때, sensitivity는 0.9167, specificity는 0.6667이다.
# 이 민감도와 특이도가 클수록 예측성능이 높다는 의미이므로 ROC곡선이 대각선과 멀어질수록
# 예측 성능이 높다고 할 수 있다.
