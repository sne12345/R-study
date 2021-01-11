setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("BostonHousing.csv")

data.for.plot <- aggregate(housing.df$CRIM, by=list(housing.df$CHAS), FUN=mean)
names(data.for.plot) <- c("CHAS","MeanCRIM")
barplot(data.for.plot$MeanCRIM, names.arg=data.for.plot$CHAS,xlab="CHAS",ylab="Avg.CRIM",col =c("yellowgreen","darkorange"))

# 산점도 그리기
plot(housing.df$MEDV ~ housing.df$LSTAT,col="darkorchid4")
# 히스토그램 그리기
hist(housing.df$MEDV, xlab="MEDV",col="steelblue3")

# 박스플랏 그리기

# CAT..MEDV / NOX
boxplot(housing.df$NOX ~housing.df$CAT..MEDV, col="steelblue3")
# -> 중앙값이 3만 달러 이상인 그룹에서 이하인 그룹보다 일산화질소 농도가 더 낮다.

# CAT..MEDV / LSTAT
boxplot(housing.df$LSTAT ~housing.df$CAT..MEDV, col="orange")
# -> 중앙값이 3만 달러 이상인 그룹에서 이하인 그룹보다 저소득층의 비율이 더 낮다.

# CAT..MEDV / INDUS
boxplot(housing.df$INDUS ~housing.df$CAT..MEDV, col="yellowgreen")
# -> 중앙값이 3만 달러 이상인 그룹에서 이하인 그룹보다 비소매업종 점유 구역 비율이 더 낮다.

ggplot(housing.df, aes(y=RM, x=DIS, colour=MEDV)) + geom_point(alpha=0.6)


