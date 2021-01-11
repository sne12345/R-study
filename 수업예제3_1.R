setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("WestRoxbury.csv",header = T)
housing.df

# 인위적으로 결측치 만들기
rows.to.missing <- sample(row.names(housing.df),10)
housing.df[rows.to.missing,]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)

# 결측치에 중앙값을 넣기
housing.df[rows.to.missing,]$BEDROOMS <- median(housing.df$BEDROOMS, na.rm = TRUE)
summary(housing.df$BEDROOMS)

# 결측치에 평균값을 넣기
housing.df[rows.to.missing,]$BEDROOMS <- mean(housing.df$BEDROOMS, na.rm = TRUE)
summary(housing.df$BEDROOMS)

