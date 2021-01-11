
library(caret) #preProcess함수
library(neuralnet) #neuralnet함수

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
df <- read.csv("Telecom_v2.csv")

#데이터 전처리 : 모형에 포함되지 않는 변수 제거
df <- df[,-c(1,9)]

df$genderfemale <- ifelse(df$gender=="Female",1,0) #범주형 변수 가변수 변환
df$PartnerYes <- ifelse(df$Partner=="Yes",1,0)
df$DependentsYes <- ifelse(df$Dependents=="Yes",1,0)
df$PhoneServiceYes <- ifelse(df$PhoneService=="Yes",1,0)

df <- df[,-c(1,3,4,6)]

# 정규화
norm.values <- preProcess(df, method="range")
norm.df <- predict(norm.values, df)

# 노드의 수 : 3개
nn <- neuralnet(Churn~.,data=norm.df,linear.output = F, hidden = 3)

# 연결강도 계산(display weights)
nn$weights
# 신경망 다이어그램 그리기(plot network)
plot(nn, rep="best") # -> 세타 먼저 + 연결강도들  
# 분류결과 표시(display predictions)
prediction(nn)

predict <- compute(nn,norm.df[,-4])
predicted.class <- apply(predict$net.result,1,which.max)
confusionMatrix(factor(ifelse(predicted.class==2,"Yes","No")),factor(norm.df$Churn))


# 노드의 수 : 4개
nn <- neuralnet(Churn~.,data=norm.df,linear.output = F, hidden = 4)

# 연결강도 계산(display weights)
nn$weights
# 신경망 다이어그램 그리기(plot network)
plot(nn, rep="best") # -> 세타 먼저 + 연결강도들  
# 분류결과 표시(display predictions)
prediction(nn)

predict <- compute(nn,norm.df[,-4])
predicted.class <- apply(predict$net.result,1,which.max)
confusionMatrix(factor(ifelse(predicted.class==2,"Yes","No")),factor(norm.df$Churn))


# 노드의 수 : 5개
nn <- neuralnet(Churn~.,data=norm.df,linear.output = F, hidden = 5)

# 연결강도 계산(display weights)
nn$weights
# 신경망 다이어그램 그리기(plot network)
plot(nn, rep="best") # -> 세타 먼저 + 연결강도들  
# 분류결과 표시(display predictions)
prediction(nn)

predict <- compute(nn,norm.df[,-4])
predicted.class <- apply(predict$net.result,1,which.max)
confusionMatrix(factor(ifelse(predicted.class==2,"Yes","No")),factor(norm.df$Churn))
