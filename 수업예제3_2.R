setwd("C:\\Users\\LG\\Desktop\\학교\\마케팅애널리틱스")
housing.df <- read.csv("WestRoxbury.csv",header = T)
housing.df

# 학습데이터 생성
set.seed(1) # 랜덤 넘버 발생 : 교수님과 같은 랜덤순서를 갖게 하기 위해 실행
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.6)
train.data <- housing.df[train.rows,]

# 검증데이터 생성
valid.rows <- setdiff(rownames(housing.df),train.rows)
valid.data <- housing.df[valid.rows,]

