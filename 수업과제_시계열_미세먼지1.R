library(forecast)
setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
airpol.data <- read.csv("Airpollution.csv")

ridership.ts <- ts(airpol.data$Korea,start = c(2013, 1), end = c(2019, 11), freq = 12)
plot(ridership.ts, xlab = "Time", ylab = "Korea (in 000s)", ylim = c(10, 90))

# 선형 추세선 추가
lm.trend <- tslm(ridership.ts ~ trend)
summary(lm.trend)
plot(ridership.ts, xlab = "Time", ylab = "pollution", ylim = c(20,80), bty = "l")
lines(lm.trend$fitted, lwd = 2)

# 다항(비선형) 추세선 추가
lm.trend2 <- tslm(ridership.ts ~ trend+I(trend^2))
summary(lm.trend2)
plot(ridership.ts, xlab = "Time", ylab = "Pollution", ylim = c(20,80), bty = "l")
lines(lm.trend2$fitted, lwd = 2)

# 계절변동 예측 가변수 생성(1월 기준)  
lm.season <- tslm(ridership.ts ~ season)
summary(lm.season)
