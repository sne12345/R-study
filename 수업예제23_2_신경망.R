library(caret) #혼동행렬을 이용한 분류 정확도 검증
predict <- compute(nn, data.frame(df$Salt, df$Fat)) #출력노드 결과값 계산
predicted.class=apply(predict$net.result,1,which.max)-1 # 1 또는 0으로 변환
confusionMatrix(factor(ifelse(predicted.class=="1", "like","dislike")), factor
                (df$Acceptance))