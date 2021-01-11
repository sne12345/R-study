# 잔차 : 실제값와 예측값의 차이
# 오차 : y = a + bx + e 에서 'e', x와 y의 관계식에서 설명되지 않는 부분

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("WestRoxbury.csv")

xt <- model.matrix(~0+BEDROOMS+REMODEL,data=housing.df)
xt <- as.data.frame(xt)

housing.df1<-cbind(housing.df$TOTAL.VALUE,xt)

# 다 포함시키면 하나가 NA 뜸
lm(housing.df$TOTAL.VALUE~BEDROOMS+REMODELNone+REMODELOld+REMODELRecent,data=housing.df1)
# 기준 설정하기(하나 누락시키기)
lm(housing.df$TOTAL.VALUE~BEDROOMS+REMODELNone+REMODELRecent,data=housing.df1)


