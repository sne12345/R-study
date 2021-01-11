library(neuralnet)
library(caret)
library(forecast) #accuracy함수

setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
toyota.df <- read.csv("ToyotaCorolla.csv")

# 데이터 전처리: 범주형 변수에 대한 가변수 생성
toyota.df$Fuel_Type_CNG <- 1*(toyota.df$Fuel_Type == "CNG")
toyota.df$Fuel_Type_Diesel <- 1*(toyota.df$Fuel_Type == "Diesel")

# 데이터 분할 : 학습데이터 60% 검증데이터 40%
set.seed(1)
train.index <- sample(row.names(toyota.df), 0.6*dim(toyota.df)[1])
valid.index <- setdiff(row.names(toyota.df), train.index)
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[valid.index, ]

# 정규화(normalize) : 0에서 1사이로 변환
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

# 학습데이터에서 인공신경망 적합
nn <- neuralnet(Price ~ Age_08_04+
                  KM+
                  Fuel_Type_CNG+
                  Fuel_Type_Diesel+
                  HP+
                  Automatic+
                  Doors+
                  Quarterly_Tax+ 
                  Mfr_Guarantee+
                  Guarantee_Period+  
                  Airco+ 
                  Automatic_airco+  
                  CD_Player+ 
                  Powered_Windows+
                  Sport_Model+
                  Tow_Bar,
                  data = train.norm.df, linear.output = T, hidden = 3)
## 예측이기 때문에 linear.output에 T를 넣어준다.
plot(nn)

train.pred=compute(nn, train.norm.df[, c("Age_08_04",
                                         "KM",
                                         "Fuel_Type_CNG",
                                         "Fuel_Type_Diesel",
                                         "HP",
                                         "Automatic",
                                         "Doors","Quarterly_Tax", 
                                           "Mfr_Guarantee",
                                           "Guarantee_Period",  
                                           "Airco", 
                                           "Automatic_airco",  
                                           "CD_Player", 
                                           "Powered_Windows",
                                           "Sport_Model",
                                           "Tow_Bar")])
pred <- train.pred$net.result
train.df1 <- data.frame(pred)
accuracy(train.df1$pred, train.norm.df$Price)

valid.pred=compute(nn, valid.norm.df[, c("Age_08_04",
                                         "KM",
                                         "Fuel_Type_CNG",
                                         "Fuel_Type_Diesel",
                                         "HP",
                                         "Automatic",
                                         "Doors","Quarterly_Tax", 
                                         "Mfr_Guarantee",
                                         "Guarantee_Period",  
                                         "Airco", 
                                         "Automatic_airco",  
                                         "CD_Player", 
                                         "Powered_Windows",
                                         "Sport_Model",
                                         "Tow_Bar")])
pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$Price)



# -------------------------------------------------
# 학습데이터에서 인공신경망 적합
nn <- neuralnet(Price ~ Age_08_04+
                  KM+
                  Fuel_Type_CNG+
                  Fuel_Type_Diesel+
                  HP+
                  Automatic+
                  Doors+
                  Quarterly_Tax+ 
                  Mfr_Guarantee+
                  Guarantee_Period+  
                  Airco+ 
                  Automatic_airco+  
                  CD_Player+ 
                  Powered_Windows+
                  Sport_Model+
                  Tow_Bar,
                data = train.norm.df, linear.output = T, hidden = 4)

valid.pred=compute(nn, valid.norm.df[, c("Age_08_04",
                                         "KM",
                                         "Fuel_Type_CNG",
                                         "Fuel_Type_Diesel",
                                         "HP",
                                         "Automatic",
                                         "Doors","Quarterly_Tax", 
                                         "Mfr_Guarantee",
                                         "Guarantee_Period",  
                                         "Airco", 
                                         "Automatic_airco",  
                                         "CD_Player", 
                                         "Powered_Windows",
                                         "Sport_Model",
                                         "Tow_Bar")])
pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$Price)

# 학습데이터에서 인공신경망 적합
nn <- neuralnet(Price ~ Age_08_04+
                  KM+
                  Fuel_Type_CNG+
                  Fuel_Type_Diesel+
                  HP+
                  Automatic+
                  Doors+
                  Quarterly_Tax+ 
                  Mfr_Guarantee+
                  Guarantee_Period+  
                  Airco+ 
                  Automatic_airco+  
                  CD_Player+ 
                  Powered_Windows+
                  Sport_Model+
                  Tow_Bar,
                data = train.norm.df, linear.output = T, hidden = 5)

valid.pred=compute(nn, valid.norm.df[, c("Age_08_04",
                                         "KM",
                                         "Fuel_Type_CNG",
                                         "Fuel_Type_Diesel",
                                         "HP",
                                         "Automatic",
                                         "Doors","Quarterly_Tax", 
                                         "Mfr_Guarantee",
                                         "Guarantee_Period",  
                                         "Airco", 
                                         "Automatic_airco",  
                                         "CD_Player", 
                                         "Powered_Windows",
                                         "Sport_Model",
                                         "Tow_Bar")])
pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$Price)
