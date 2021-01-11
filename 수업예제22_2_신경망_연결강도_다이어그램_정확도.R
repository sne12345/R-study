install.packages("neuralnet")
library(neuralnet) # 인공신경망을 위한 neuralnet 설치 및 이용

df <- read.csv("tinydata.csv") #데이터불러오기

nn <- neuralnet(Acceptance~Fat+Salt,data=df,linear.output = F, hidden = 3)

# 연결강도 계산(display weights)
nn$weights
# 신경망 다이어그램 그리기(plot network)
plot(nn, rep="best") # -> 세타 먼저 + 연결강도들  
# 분류결과 표시(display predictions)
prediction(nn)

library(caret) #혼동행렬을 이용한 분류 정확도 검증
predict <- compute(nn, data.frame(df$Salt, df$Fat)) #출력노드 결과값 계산
predicted.class=apply(predict$net.result,1,which.max)-1 # 1 또는 0으로 변환
confusionMatrix(factor(ifelse(predicted.class=="1", "like","dislike")), factor
                (df$Acceptance))
