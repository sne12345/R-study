setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
dist <- read.csv("seoulmap.csv")

# na값을 0으로 바꾸기
dist[is.na(dist)] <- 0

# 표 정리하기
row.names(dist) <- dist[,1]
dist <- dist[,-1]

# 거리정보를 이용해서 2차원에 각 개체들의 위치정보 가져오기
fit <- cmdscale(dist)

# 남북과 동서가 바뀌었기 때문에 되돌려줌 
x <- -fit[,1]
y <- -fit[,2]

# pch는 마커모양, cex는 글자크기 
plot(x,y,pch=19)
text(x,y,rownames(dist),cex=0.8)
# -> 네이버 지도로 지점간 거리를 기준으로 한 것이기 때문에 오차가 있을 수 있다

library("ggplot2")
library("ggrepel") # 텍스트 중첩 방지

mds <- data.frame(name=rownames(fit),x,y)

# 텍스트 중첩 방지
ggplot(mds, aes(x,y,label=name)) + geom_point() + geom_text_repel() + theme()






