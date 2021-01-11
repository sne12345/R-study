setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
bank.df <- read.csv("Telecom_v2.csv")
bank.df <- bank.df[,-c(1,9)]

bank.df$gender_Female <- ifelse(bank.df$gender=="Female",1,0) #범주형 변수 가변수 변환
bank.df$Partner_Yes <- ifelse(bank.df$Partner=="Yes",1,0) #범주형 변수 가변수 변환
bank.df$Dependents_Yes <- ifelse(bank.df$Dependents=="Yes",1,0) #범주형 변수 가변수 변환
bank.df$PhoneService_Yes <- ifelse(bank.df$PhoneService=="Yes",1,0) #범주형 변수 가변수 변환
bank.df <- bank.df[,-c(1,3,4,6)]

#정규화(normalize) : 0에서 1사이로 변환
norm.values <- preProcess(bank.df, method="range")
norm.df <- predict(norm.values, bank.df)

# 은닉층 노드의 수 : 3개
nn <-neuralnet(Churn ~ ., data=norm.df, linear.output=F, hidden=3)

predict <- compute(nn,norm.df[,c(1:3,5:8)]) 
## [,1] , [,2] 는 알파벳 순으로 Nonowner, Owner
predicted.class <- apply(predict$net.result,1,which.max)
confusionMatrix(factor(ifelse(predicted.class==2,"Yes","No")),
                factor(norm.df$Churn))


# 은닉층 노드의 수 : 5개
nn <-neuralnet(Churn ~ ., data=norm.df, linear.output=F, hidden=5)

predict <- compute(nn,norm.df[,c(1:3,5:8)]) 
## [,1] , [,2] 는 알파벳 순으로 Nonowner, Owner
predicted.class <- apply(predict$net.result,1,which.max)
confusionMatrix(factor(ifelse(predicted.class==2,"Yes","No")),
                factor(norm.df$Churn))


# 은닉층 노드의 수 : 7개
nn <-neuralnet(Churn ~ ., data=norm.df, linear.output=F, hidden=7)

predict <- compute(nn,norm.df[,c(1:3,5:8)]) 
## [,1] , [,2] 는 알파벳 순으로 Nonowner, Owner
predicted.class <- apply(predict$net.result,1,which.max)
confusionMatrix(factor(ifelse(predicted.class==2,"Yes","No")),
                factor(norm.df$Churn))

