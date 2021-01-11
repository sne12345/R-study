setwd("C:\Users\LG\Desktop\학교\마케팅애널리틱스")
telecom.df <- read.csv("Telecom.csv")
telecom.df

teltotal <- model.matrix(~0 +  Contract +  MonthlyCharges , data = telecom.df)
# 범주형 변수를 두가지 넣으면 안되나요? 
# -> 가능, but 한 개의 열(기준)이 자동으로 빠져서 나온다
teltotal <- as.data.frame(teltotal)  
t(t(names(teltotal)))

teltotal <- teltotal[,-1]
head(teltotal)
