setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")

dist <- read.csv("roadshopbrand.csv")
dist[is.na(dist)]<-0 
row.names(dist) <- dist[, 1] 
dist <- dist[, -1] #1열을 삭제 

fit <- cmdscale(dist)   # 거리 정보를 2차원 좌표로 변환

x <- -fit[ , 1] #x축 좌표 
y <- -fit[ , 2] #y축 좌표  

#plot 함수 사용
plot(x, y, pch = 19) #2차원 평면에 도식화
text(x, y, rownames(fit), cex = 0.8) #개체 이름 보이기

#ggplot 함수 사용(ggplot2패키지 설치)
install.packages("ggrepel")
library(ggplot2)
library(ggrepel) #텍스트 중첩 방지 
mds<- data.frame(name = rownames(fit), x = -fit[,1], y= -fit[ , 2])
ggplot(mds, aes(x, y,label=name))+geom_point()+geom_text_repel()

# x축은 높을수록 더 건강한 느낌, 낮을수록 색조가 강한 느낌
# y축은 높을수록 연령층이 낮음음