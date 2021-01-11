setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("BostonHousing.csv")

round(cor(housing.df),2)

heatmap(round(cor(housing.df),2),Rowv=NA, Colv=NA)

install.packages("gplots")
library(gplots)

heatmap.2(cor(housing.df),Rowv=FALSE, Colv=FALSE, dendrogram = "none", cellnote=round(cor(housing.df),2), notecol="black", key=FALSE,trace="none", margins=c(4,4))

install.packages("ggplot2")
library(ggplot2)
install.packages("reshape")
library(reshape)

cor.mat <- round(cor(housing.df),2)
melted.cor.mat <- melt(cor.mat)
# 카피페이스트 : 한번 해보기
ggplot(melted.cor.mat, aes(x=X1, y=X2, fill=value))+geom_tile()+geom_text(aes(x=X1, y=X2, label=value))







# x : 비소매업종 점유 구역 비율이 낮을수록 주택가격의 중앙값이 높다.
# y : 학생 대 교사의 비율이 낮을수록 주택가격의 중앙값이 높다.
install.packages("ggplot")
library(ggplot2)
ggplot(housing.df, aes(y=PTRATIO, x=INDUS, colour=MEDV)) + geom_point(alpha=0.6)






plot(housing.df[,c(1,3,12,13)])
install.packages("GGally")
library(GGally)
ggpairs(housing.df[,c(1,3,12,13)])



          